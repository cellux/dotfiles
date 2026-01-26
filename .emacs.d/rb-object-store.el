;;; rb-object-store.el --- RB's object store -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Object store implementation for LLM use.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'rb-tools)
(require 'rb-io)
(require 'rb-rg)

(defvar rb-object-store-root nil
  "Override for the object store root directory.
When non-nil, object store is created under this root instead of the
current project root.")

;;; JSON schema for classes -----------------------

(defvar rb-object-store--json-value-missing
  (make-symbol "rb-object-store--json-value-missing")
  "Sentinel value used internally when a JSON property is absent.")

(defun rb-object-store--json-key->string (key)
  "Return KEY as a string, dropping the leading colon for keywords."
  (cond
   ((keywordp key)
    (substring (symbol-name key) 1))
   ((symbolp key)
    (symbol-name key))
   ((stringp key)
    key)
   (t
    (format "%s" key))))

(defun rb-object-store--normalize-json-object (obj)
  "Normalize OBJ (alist/plist/hash) into a hash table keyed by strings."
  (let ((table (make-hash-table :test #'equal)))
    (cond
     ((hash-table-p obj)
      (maphash (lambda (k v)
                 (puthash (rb-object-store--json-key->string k) v table)) obj))
     ((and (listp obj) (seq-every-p #'consp obj))
      (mapc (lambda (entry)
              (let ((k (car entry))
                    (v (cdr entry)))
                (puthash (rb-object-store--json-key->string k) v table)))
            obj))
     ((listp obj)
      (let ((plist (copy-sequence obj)))
        (unless (cl-evenp (length plist))
          (error "Property list must contain an even number of elements"))
        (while plist
          (let ((k (pop plist))
                (v (pop plist)))
            (puthash (rb-object-store--json-key->string k) v table)))))
     (t
      (error "Cannot normalize JSON object: %s" obj)))
    table))

(defun rb-object-store--json-schema-properties-alist (schema)
  "Return an alist of property definitions from SCHEMA."
  (let ((properties (plist-get schema :properties))
        (result '()))
    (while properties
      (let ((key (pop properties))
            (value (pop properties)))
        (push (cons (rb-object-store--json-key->string key) value) result)))
    (nreverse result)))

(defun rb-object-store--json-schema-property-required-p (key schema)
  "Return non-nil if property KEY must be present in an object according to SCHEMA."
  (let* ((properties (plist-get schema :properties))
         (definition (plist-get properties (rb-tools-ensure-keyword key)))
         (required (seq-into (plist-get schema :required) 'list)))
    (or (seq-contains-p required key)
        (plist-get definition :const))))

(defun rb-object-store--json-value-satisfies-type-p (value type)
  "Return non-nil if VALUE satisfies TYPE from a JSON schema."
  (pcase type
    ("string" (stringp value))
    ("integer" (integerp value))
    ("number" (numberp value))
    ("boolean" (or (eq value t) (eq value nil)))
    (_ (error "Unsupported schema type: %s" type))))

(defun rb-object-store--validate-json-property (key value definition)
  "Validate VALUE for property KEY using DEFINITION."
  (let ((type (plist-get definition :type))
        (const (plist-get definition :const)))
    (when type
      (unless (rb-object-store--json-value-satisfies-type-p value type)
        (error "Property %s must be %s" key type)))
    (when const
      (unless (equal value const)
        (error "Property %s must be %s" key const)))))

(defun rb-object-store--validate-json-object (obj schema)
  "Validate OBJ against SCHEMA.
Implements only a small subset of the JSON schema spec.
Usable with schemas sourced from `get_json_schema_for_class'."
  (unless (and schema (string= (plist-get schema :type) "object"))
    (error "Schema must describe an object"))
  (let ((properties (rb-object-store--json-schema-properties-alist schema))
        (normalized (rb-object-store--normalize-json-object obj)))
    (dolist (entry properties)
      (let* ((key (car entry))
             (definition (cdr entry))
             (value (gethash key normalized rb-object-store--json-value-missing)))
        (if (eq value rb-object-store--json-value-missing)
            (when (rb-object-store--json-schema-property-required-p key schema)
              (error "Missing required property: %s" key))
          (rb-object-store--validate-json-property key value definition))))
    t))

(cl-defgeneric rb-object-store--get-json-schema-for-class (class)
  "Return the JSON schema for object store class CLASS."
  (:method :around (class)
           (let* ((result (cl-call-next-method))
                  (properties (plist-get result :properties))
                  (class-string (format "%s" class)))
             (plist-put properties :class `( :type "string"
                                             :const ,class-string
                                             :description ,(format "Must be the literal string '%s'" class-string)))
             (plist-put properties :id `( :type "string"
                                          :description ,(format "Unique identifier of the %s object" class-string)))
             result))
  (error "Unknown class: %s" class))

(cl-defmethod rb-object-store--get-json-schema-for-class ((_class (eql 'STORY)))
  "Return the JSON schema for object store class `STORY'."
  '( :type "object"
     :description "Objects of the STORY class describe user stories.

A user story describes the goal we want to achieve, the problem we want
to solve, the change we want to make.

Each user story has a number of TASK objects which are standalone,
bite-sized chunks of the implementation plan.

STORY IDs look like STORY-<number>-<slug>.

One iteration of the development cycle typically consists of the following steps:

1. Create user story
2. Create tasks for the user story
3. Implement individual tasks of the user story

*How to resolve STORY references:*

If the reference looks like a full STORY id (STORY-<number>-<slug>) then
try to fetch it via =get_object=. If it looks more like a prefix, use
=find_objects= to get a list of matching stories. If there is a single
match, use =get_object= to retrieve the details.

"
     :properties ( :name ( :type "string"
                           :description "Short name of the user story, serves as a one-line summary")
                   :description ( :type "string"
                                  :format "markdown"
                                  :description "Describes the goal we want to achieve, the problem we want to solve
or the change we want to make through this user story.
Written from the perspective of the user who requested the feature.
Populated during the planning.")
                   :implementation_plan ( :type "string"
                                          :format "markdown"
                                          :description "Describes in detail the implementation plan:
how we are going to achieve the goal, solve the problem, make the change.
Includes architecture, data flow, APIs, trade-offs, non-goals.
Gives an overview of the individual steps necessary to build the solution.
Written from the perspective of the programmer.
Populated during the planning.")
                   :review ( :type "string"
                             :format "markdown"
                             :description "What we learned from the implementation of the story.
Populated only after all tasks have been completed."))))

(cl-defmethod rb-object-store--get-json-schema-for-class ((_class (eql 'TASK)))
  "Return the JSON schema for object store class `TASK'."
  '( :type "object"
     :description "Objects of the TASK class describe individual development tasks.

TASKS may be associated with STORIES or may stand alone.

Each task is implemented by an independent agent who fetches the TASK
and its parent STORY - if it has one - to build the necessary context.

STORY IDs look like STORY-<number>-<slug>, and tasks under that story
like TASK-<number>-<sequence>-<slug> where <number> matches the story’s
number and <sequence> is an integer order.

Example TASK identifiers: TASK-1-1-first, TASK-1-2-second, TASK-124-5-wrap-up

TASK-124-5-wrap-up means the fifth task of STORY-124 with the slug `wrap-up'

*How to resolve TASK references:*

If the reference looks like a full TASK
id (STORY-<number>-<sequence>-<slug>) then try to fetch it via
=get_object=. If it looks more like a prefix, use =find_objects= to get
a list of matching tasks. If there is a single match, use =get_object=
to retrieve the details.

"
     :properties ( :name ( :type "string"
                           :description "Short name of the task, serves as a one-line summary")
                   :description ( :type "string"
                                  :format "markdown"
                                  :description "Step-by-step breakdown of the process necessary to implement this task.

If an independent agent has access to the TASK and its parent STORY,
it should be able to implement the TASK relying solely on the information
contained within these two objects.")
                   :review ( :type "string"
                             :format "markdown"
                             :description "What we learned from the implementation of the task.
Populated only after the task has been completed.")
                   :story ( :type "string"
                            :description "Link to the parent story which owns this task."))))

(defun rb-object-store-get-json-schema-for-class (class)
  "Return the JSON schema for object store class CLASS."
  (when (stringp class)
    (setq class (intern class)))
  (unless (symbolp class)
    (error "CLASS must be string or symbol, got %s" class))
  (rb-object-store--get-json-schema-for-class class))

;;; Object store ----------------------------------------------------------

(defun rb-object-store--sanitize-path-component (component)
  "Return COMPONENT as a safe path segment string."
  (let* ((raw (format "%s" component))
         (clean (string-replace "/" "_" raw)))
    (when (string-empty-p clean)
      (error "Path component cannot be empty"))
    clean))

(defun rb-object-store--base-dir (class id)
  "Return filesystem path for object CLASS with ID.
When `rb-object-store-root' is non-nil, use it; otherwise fall back to
`rb-tools-project-root'."
  (let* ((root (or rb-object-store-root
                   (rb-tools-project-root)))
         (class-part (rb-object-store--sanitize-path-component class))
         (id-part (rb-object-store--sanitize-path-component id)))
    (expand-file-name (format ".store/%s/%s" class-part id-part) root)))

(defun rb-object-store--extract-property (entry)
  "Extract (NAME . VALUE) from property ENTRY.
ENTRY may be a plist, alist, or hash table with keys :name/:value or name/value."
  (let* ((name (cond
               ((hash-table-p entry) (or (gethash "name" entry)
                                        (gethash :name entry)))
               (t (or (plist-get entry :name)
                      (plist-get entry 'name)
                      (alist-get :name entry)
                      (alist-get 'name entry)))))
         (value (cond
                 ((hash-table-p entry) (or (gethash "value" entry)
                                           (gethash :value entry)))
                 (t (or (plist-get entry :value)
                        (plist-get entry 'value)
                        (alist-get :value entry)
                        (alist-get 'value entry))))))
    (cons name value)))

(defun rb-object-store--object-from-args (class id properties)
  "Build an object alist from CLASS, ID and PROPERTIES array."
  (unless (or (stringp class) (symbolp class))
    (error "CLASS must be a string or symbol"))
  (unless (stringp id)
    (error "ID must be a string"))
  (let* ((class-str (if (symbolp class) (symbol-name class) class))
         (props (seq-into properties 'list))
         (seen (make-hash-table :test #'equal))
         (object (list (cons "class" class-str)
                       (cons "id" id))))
    (puthash "class" t seen)
    (puthash "id" t seen)
    (dolist (entry props)
      (pcase-let* ((`(,name . ,value) (rb-object-store--extract-property entry)))
        (unless (and (stringp name) (not (string-empty-p name)))
          (error "Property name must be a non-empty string"))
        (when (member name '("class" "id"))
          (error "Property %s is reserved" name))
        (when (gethash name seen)
          (error "Duplicate property: %s" name))
        (puthash name t seen)
        (push (cons name value) object)))
    (nreverse object)))

(defun rb-object-store--write-object-to-filesystem (object &optional overwrite)
  "Persist OBJECT alist to the object store.
OBJECT must contain at least keys `class' and `id'.
If OVERWRITE is non-nil, the object must already exist and its properties
are replaced.  When OVERWRITE is nil, an error is raised if the object
already exists."
  (let* ((class (cdr (assoc "class" object)))
         (id (cdr (assoc "id" object))))
    (unless class (error "Missing class property"))
    (unless id (error "Missing id property"))
    (let* ((base (rb-object-store--base-dir class id))
           (exists (file-directory-p base)))
      (cond
       (exists
        (unless overwrite
          (error "Object %s-%s already exists" class id)))
       (overwrite
        (error "Object %s-%s does not exist" class id))
       (t
        (make-directory base t)))
      (dolist (entry object)
        (let* ((key (car entry))
               (val (cdr entry))
               (key-str (rb-object-store--sanitize-path-component key))
               (file (expand-file-name key-str base)))
          (unless (stringp val)
            (error "Property %s value must be a string" key))
          (rb-io-write-file file val))))))

(defun rb-object-store-insert-object (class id properties)
  "Insert a new OBJECT into the store.

CLASS (string), ID (string), and PROPERTIES (array of name/value pairs)
are validated against class schema and written to the object store.

Returns the validated object alist."
  (let* ((object (rb-object-store--object-from-args class id properties))
         (schema (rb-object-store-get-json-schema-for-class class)))
    (rb-object-store--validate-json-object object schema)
    (rb-object-store--write-object-to-filesystem object nil)
    object))

(defun rb-object-store--read-object-from-filesystem (class id &optional properties)
  "Return the object ID of CLASS from the store as an alist.
If PROPERTIES is non-nil, it must be a list/array of property names to
return; when PROPERTIES is nil or empty, return all properties."
  (let* ((base (rb-object-store--base-dir class id))
         (prop-list (when properties (seq-into properties 'list))))
    (unless (file-directory-p base)
      (error "Object %s-%s does not exist" class id))
    (if (and prop-list (not (seq-empty-p prop-list)))
        (let ((keys prop-list)
              (result '()))
          (dolist (key keys)
            (unless (and (stringp key) (not (string-empty-p key)))
              (error "Property name must be a non-empty string: %s" key))
            (let ((file (expand-file-name (rb-object-store--sanitize-path-component key) base)))
              (unless (file-readable-p file)
                (error "Missing property %s for object %s-%s" key class id))
              (push (cons key (rb-io-read-file file)) result)))
          (nreverse result))
      (let ((files (directory-files base nil "^[^.].*" t))
            (result '()))
        (dolist (fname files)
          (let ((file (expand-file-name fname base)))
            (when (file-regular-p file)
              (push (cons fname (rb-io-read-file file)) result))))
        (nreverse result)))))

(defun rb-object-store-get-object (class id &optional properties)
  "Retrieve an object from the store.

CLASS and ID identify the object.  PROPERTIES, if provided, is an
array/list of property names to return; when omitted, all properties are
returned.  The result is a plist with keyword keys."
  (let ((alist (rb-object-store--read-object-from-filesystem class id properties)))
    (rb-tools-alist-to-keyword-plist alist)))

(defun rb-object-store-update-object (class id properties)
  "Update an existing object in the store.

CLASS (string), ID (string), and PROPERTIES (array of name/value pairs)
are combined into a single object.  Only the provided properties are
validated and updated; other properties remain unchanged.

Returns the full updated object as a plist."
  (let* ((schema (rb-object-store-get-json-schema-for-class class))
         (base (rb-object-store--base-dir class id)))
    (unless (file-directory-p base)
      (error "Object %s-%s does not exist" class id))
    ;; Read existing object
    (let* ((existing (rb-object-store--read-object-from-filesystem class id nil))
           (existing-object (rb-object-store--normalize-json-object existing))
           (updates (seq-into properties 'list)))
      ;; Apply updates
      (dolist (entry updates)
        (pcase-let* ((`(,name . ,value) (rb-object-store--extract-property entry)))
          (unless (and (stringp name) (not (string-empty-p name)))
            (error "Property name must be a non-empty string"))
          (when (member name '("class" "id"))
            (error "Property %s is reserved" name))
          (puthash name value existing-object)))
      ;; Build updated alist preserving original keys order where possible
      (let ((updated '()))
        ;; Ensure class/id first, then other keys sorted for stability
        (push (cons "class" (gethash "class" existing-object)) updated)
        (push (cons "id" (gethash "id" existing-object)) updated)
        (let* ((keys (seq-filter (lambda (k) (not (member k '("class" "id"))))
                                 (hash-table-keys existing-object)))
               (sorted (sort keys #'string<)))
          (dolist (k sorted)
            (push (cons k (gethash k existing-object)) updated)))
        (setq updated (nreverse updated))
        ;; Validate only provided properties against schema
        (dolist (entry updates)
          (pcase-let* ((`(,name . ,value) (rb-object-store--extract-property entry)))
            (let* ((definition (plist-get (plist-get schema :properties)
                                          (rb-tools-ensure-keyword name))))
              (unless definition
                (error "Unknown property for class %s: %s" class name))
              (rb-object-store--validate-json-property name value definition))))
        ;; Write back
        (rb-object-store--write-object-to-filesystem updated t)
        (rb-tools-alist-to-keyword-plist updated)))))

(defun rb-object-store--list-object-ids (class)
  "Return a list of object IDs for CLASS from the store."
  (let* ((root (or rb-object-store-root
                   (rb-tools-project-root)))
         (class-dir (expand-file-name
                     (format ".store/%s" (rb-object-store--sanitize-path-component class))
                     root))
         (ids '()))
    (when (file-directory-p class-dir)
      (dolist (entry (directory-files class-dir nil "^[^.].*" t))
        (let ((full (expand-file-name entry class-dir)))
          (when (file-directory-p full)
            (push entry ids)))))
    (nreverse ids)))

(cl-defun rb-object-store-find-objects (class &optional id properties)
  "Find objects of CLASS satisfying the search criteria in ID and PROPERTIES.

Returns only objects satisfying all provided search criteria.  ID, when
supplied, is treated as a regex matched against the object's id
property.  PROPERTIES, when supplied, is a list/array of name/value
pairs where VALUE is a regex matched against the property's content.

Returns a list of matching objects or nil if none were found."
  (unless (or (stringp class) (symbolp class))
    (error "CLASS must be a string or symbol"))
  (when (and id (not (and (stringp id) (not (string-empty-p id)))))
    (error "ID regex must be a non-empty string when provided"))
  (let* ((class-str (if (symbolp class) (symbol-name class) class))
         (root (or rb-object-store-root
                   (rb-tools-project-root)))
         (class-dir (expand-file-name
                     (format ".store/%s" (rb-object-store--sanitize-path-component class-str))
                     root)))
    (unless (file-directory-p class-dir)
      (cl-return-from rb-object-store-find-objects nil))
    (let* ((filters (seq-into properties 'list))
           (candidates (rb-object-store--list-object-ids class-str)))
      (unless candidates
        (cl-return-from rb-object-store-find-objects nil))
      (when id
        (let* ((output (rb-rg id class-dir "--glob id"))
               (match-set (make-hash-table :test #'equal)))
          (dolist (line (split-string output "\n" t))
            (when (string-match "^\\(.*\\):[0-9]+:" line)
              (let* ((matched-path (match-string 1 line))
                     (abs-path (expand-file-name matched-path class-dir))
                     (relative (file-relative-name abs-path class-dir))
                     (oid (car (split-string relative "/" t))))
                (when oid (puthash oid t match-set)))))
          (setq candidates (seq-filter (lambda (oid) (gethash oid match-set)) candidates)))
        (unless candidates
          (cl-return-from rb-object-store-find-objects nil)))
      (while (and filters candidates)
        (pcase-let* ((`(,pname . ,regex) (rb-object-store--extract-property (pop filters))))
          (unless (and (stringp pname) (not (string-empty-p pname)))
            (error "Property name must be a non-empty string"))
          (unless (and (stringp regex) (not (string-empty-p regex)))
            (error "Property regex must be a non-empty string"))
          (let* ((prop-file (rb-object-store--sanitize-path-component pname))
                 (output (rb-rg regex class-dir (format "--glob %s" prop-file)))
                 (match-set (make-hash-table :test #'equal)))
            (dolist (line (split-string output "\n" t))
              (when (string-match "^\\(.*\\):[0-9]+:" line)
                (let* ((matched-path (match-string 1 line))
                       (abs-path (expand-file-name matched-path class-dir))
                       (relative (file-relative-name abs-path class-dir))
                       (oid (car (split-string relative "/" t))))
                  (when oid (puthash oid t match-set)))))
            (setq candidates (seq-filter (lambda (oid) (gethash oid match-set)) candidates)))))
      (mapcar (lambda (oid) (rb-object-store-get-object class-str oid)) candidates))))


;;; Unit tests -----------------------------------------------------------------
(require 'ert)

(ert-deftest rb-object-store--normalize-json-object/plist-and-alist ()
  (let* ((plist '(:name "Story" story 42))
         (alist '((:name . "Story") (story . 42)))
         (hash (let ((h (make-hash-table :test #'equal)))
                 (puthash :name "Story" h)
                 (puthash 'story 42 h)
                 h)))
    (dolist (obj (list plist alist hash))
      (let ((tbl (rb-object-store--normalize-json-object obj)))
        (should (equal (gethash "name" tbl) "Story"))
        (should (equal (gethash "story" tbl) 42))))))


(ert-deftest rb-object-store--validate-json-object/accepts-valid-story ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'STORY))
         (obj '((class . "STORY") (id . "STORY-8") (name . "Login") (description . "As a user..."))))
    (should (rb-object-store--validate-json-object obj schema))))

(ert-deftest rb-object-store--validate-json-object/rejects-missing-required-const ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'STORY))
         (obj '((id . "STORY-8") (name . "Login"))))
    (should-error (rb-object-store--validate-json-object obj schema))))

(ert-deftest rb-object-store--validate-json-object/rejects-wrong-type ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'TASK))
         (obj '((class . "TASK") (id . 2) (name . "Implement") (story . "not-integer"))))
    (should-error (rb-object-store--validate-json-object obj schema))))

(ert-deftest rb-object-store--validate-json-object/rejects-wrong-const ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'TASK))
         (obj '((class . "STORY") (id . "STORY-3") (name . "Mismatch"))))
    (should-error (rb-object-store--validate-json-object obj schema))))

(ert-deftest rb-object-store--json-schema-property-required-p/const-and-required ()
  (let ((schema '( :type "object"
                   :properties ( :class (:const "STORY")
                                 :name (:type "string")
                                 :description (:type "string"))
                   :required (:name))))
    (should (rb-object-store--json-schema-property-required-p :class schema))
    (should (rb-object-store--json-schema-property-required-p :name schema))
    (should-not (rb-object-store--json-schema-property-required-p :description schema))))


(provide 'rb-object-store)
;;; rb-object-store.el ends here
