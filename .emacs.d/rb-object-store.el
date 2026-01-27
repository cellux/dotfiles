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

(defvar rb-object-store--property-missing
  (make-symbol "rb-object-store--property-missing")
  "Sentinel value used internally when an object property is absent.")

(defun rb-object-store--object-key->keyword (key)
  "Return KEY as a keyword."
  (rb-tools-ensure-keyword key))

(defun rb-object-store--object-key->string (key)
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

(defun rb-object-store--property-required-p (key schema)
  "Return non-nil if property KEY must be present in an object according to SCHEMA."
  (let* ((properties (plist-get schema :properties))
         (definition (plist-get properties (rb-object-store--object-key->keyword key)))
         (required (seq-into (plist-get schema :required) 'list)))
    (or (seq-contains-p required (rb-object-store--object-key->string key))
        (plist-get definition :const))))

(defun rb-object-store--property-value-satisfies-type-p (value type)
  "Return non-nil if VALUE satisfies TYPE from a schema."
  (pcase type
    ("string" (stringp value))
    ("integer" (integerp value))
    ("number" (numberp value))
    ("boolean" (or (eq value t) (eq value nil)))
    (_ (error "Unsupported schema type: %s" type))))

(defun rb-object-store--validate-property (key value definition)
  "Validate VALUE for property KEY using DEFINITION."
  (let ((type (plist-get definition :type))
        (const (plist-get definition :const)))
    (when type
      (unless (rb-object-store--property-value-satisfies-type-p value type)
        (error "Property %s must be %s" key type)))
    (when const
      (unless (equal value const)
        (error "Property %s must be %s" key const)))))

(defun rb-object-store--validate-object (obj schema)
  "Validate OBJ against SCHEMA.
Implements only a small subset of the JSON schema spec.
Usable with schemas sourced from `get_json_schema_for_class'."
  (unless (and schema (string= (plist-get schema :type) "object"))
    (error "Schema must describe an object"))
  (let ((properties (plist-get schema :properties)))
    (cl-loop
     for (key definition) on properties by #'cddr
     do (let ((value (plist-get obj key)))
          (if (null value)
              (when (rb-object-store--property-required-p key schema)
                (error "Missing required property: %s" key))
            (rb-object-store--validate-property key value definition))))
    t))

(cl-defgeneric rb-object-store--get-json-schema-for-class (class)
  "Return the JSON schema for object store class CLASS."
  (:method
   :around (class)
   (let* ((result (cl-call-next-method))
          (properties (plist-get result :properties))
          (class-string (format "%s" class)))
     (plist-put properties :class `( :type "string"
                                     :const ,class-string
                                     :description ,(format "Must be the literal string '%s'"
                                                           class-string)))
     (plist-put properties :id `( :type "string"
                                  :description ,(format "Unique identifier of the %s object"
                                                        class-string)))
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

(defun rb-object-store--base-dir ()
  "Return the base directory of the object store."
  (let ((root (or rb-object-store-root (rb-tools-project-root))))
    (expand-file-name ".store" root)))

(defun rb-object-store--object-dir (class id)
  "Return filesystem path for object with CLASS and ID."
  (let* ((base-dir (rb-object-store--base-dir))
         (class-part (rb-object-store--sanitize-path-component class))
         (id-part (rb-object-store--sanitize-path-component id)))
    (expand-file-name (format "%s/%s" class-part id-part) base-dir)))

(defun rb-object-store--properties-from-args (properties)
  "Translates PROPERTIES as taken from a tool arg into a plist.
PROPERTIES is a list of objects, each a plist with :name/:value keys and
string values."
  (let* ((seen (make-hash-table :test #'equal))
         (result nil))
    (dolist (prop properties)
      (let ((name (plist-get prop :name))
            (value (plist-get prop :value)))
        (unless (stringp name)
          (error "Property name must be a string"))
        (unless (stringp value)
          (error "Property value must be a string"))
        (when (member name '("class" "id"))
          (error "Property %s is reserved" name))
        (when (gethash name seen)
          (error "Duplicate property: %s" name))
        (puthash name t seen)
        (push (rb-object-store--object-key->keyword name) result)
        (push value result)))
    (nreverse result)))

(defun rb-object-store--object-from-args (class id properties)
  "Build an object plist from CLASS, ID and PROPERTIES.
CLASS and ID are strings, PROPERTIES is a list of objects, each a plist
with :name/:value keys and string values."
  (unless (stringp class)
    (error "CLASS must be a string"))
  (unless (stringp id)
    (error "ID must be a string"))
  (apply #'list :class class :id id (rb-object-store--properties-from-args properties)))

(defun rb-object-store--write-object-to-filesystem (object &optional overwrite)
  "Persist OBJECT plist to the object store.
OBJECT must contain at least keys :class and :id.
If OVERWRITE is non-nil, the object must already exist and its properties
are replaced.  When OVERWRITE is nil, an error is raised if the object
already exists."
  (let* ((class (plist-get object :class))
         (id (plist-get object :id)))
    (unless class (error "Missing class property"))
    (unless id (error "Missing id property"))
    (let* ((object-dir (rb-object-store--object-dir class id))
           (exists (file-directory-p object-dir)))
      (cond
       (exists
        (unless overwrite
          (error "Object %s-%s already exists" class id)))
       (overwrite
        (error "Object %s-%s does not exist" class id))
       (t
        (make-directory object-dir t)))
      (cl-loop
       for (key value) on object by #'cddr
       do (let* ((key-str (rb-object-store--object-key->string key))
                 (key-filename (rb-object-store--sanitize-path-component key-str))
                 (file (expand-file-name key-filename object-dir)))
            (unless (stringp value)
              (error "Property %s value must be a string" key))
            (rb-io-write-file file value))))))

(defun rb-object-store-insert-object (class id properties)
  "Insert a new OBJECT into the store.

CLASS (string), ID (string), and PROPERTIES (list of objects with
name/value fields) are validated against class schema and written to the
object store.

Returns the validated object plist."
  (let* ((object (rb-object-store--object-from-args class id properties))
         (schema (rb-object-store-get-json-schema-for-class class)))
    (rb-object-store--validate-object object schema)
    (rb-object-store--write-object-to-filesystem object nil)
    object))

(defun rb-object-store-get-object (class id &optional properties)
  "Retrieve an object from the store.

CLASS and ID identify the object.  PROPERTIES, if provided, is a
array/list of property names to return; when omitted, all properties are
returned.  The result is a plist with keyword keys."
  (let* ((object-dir (rb-object-store--object-dir class id))
         (prop-list (seq-into properties 'list))
         (result nil))
    (unless (file-directory-p object-dir)
      (error "Object %s-%s does not exist" class id))
    (if (and prop-list (not (seq-empty-p prop-list)))
        (dolist (key prop-list)
          (let* ((key-str (rb-object-store--object-key->string key))
                 (key-filename (rb-object-store--sanitize-path-component key-str))
                 (file (expand-file-name key-filename object-dir)))
            (unless (file-readable-p file)
              (error "Missing property %s for object %s-%s" key class id))
            (setq result (plist-put
                          result
                          (rb-object-store--object-key->keyword key)
                          (rb-io-read-file file)))))
      (dolist (fname (directory-files object-dir))
        (let ((file (expand-file-name fname object-dir)))
          (when (file-regular-p file)
            (setq result (plist-put
                          result
                          (rb-object-store--object-key->keyword fname)
                          (rb-io-read-file file)))))))
    result))

(defun rb-object-store-update-object (class id properties)
  "Update an existing object in the store.

CLASS (string), ID (string), and PROPERTIES (list of objects with
name/value fields) are combined into a single object.  Only the provided
properties are validated and updated; other properties remain unchanged.

Returns the full updated object as a plist."
  (let* ((schema (rb-object-store-get-json-schema-for-class class))
         (object-dir (rb-object-store--object-dir class id)))
    (unless (file-directory-p object-dir)
      (error "Object %s-%s does not exist" class id))
    ;; Read existing object
    (let* ((object (rb-object-store-get-object class id)))
      ;; Apply updates
      (cl-loop
       for (name value) on (rb-object-store--properties-from-args properties) by #'cddr do
       (let ((definition (plist-get (plist-get schema :properties) name)))
         (unless definition
           (error "Unknown property for class %s: %s" class name))
         (rb-object-store--validate-property name value definition))
       (plist-put object name value))
      ;; Write back
      (rb-object-store--write-object-to-filesystem object t)
      object)))

(defun rb-object-store--list-object-ids (class)
  "Return a list of object IDs for CLASS from the store."
  (let* ((base-dir (rb-object-store--base-dir))
         (class-filename (rb-object-store--sanitize-path-component class))
         (class-dir (expand-file-name class-filename base-dir))
         (ids '()))
    (when (file-directory-p class-dir)
      (dolist (entry (directory-files class-dir nil "^[^.]"))
        (let ((full (expand-file-name entry class-dir)))
          (when (file-directory-p full)
            (push entry ids)))))
    (nreverse ids)))

(cl-defun rb-object-store-find-objects (class &optional id properties)
  "Find objects of CLASS satisfying the search criteria in ID and PROPERTIES.

Returns only objects satisfying all provided search criteria.

ID, when supplied, is treated as a regex matched against the object's id
property.  PROPERTIES, when supplied, is a list of objects with
name/value fields where VALUE is a regex matched against the property's
content.

Returns a list of matching objects or nil if none were found."
  (unless (stringp class)
    (error "CLASS must be a string"))
  (when (and id (not (and (stringp id) (not (string-empty-p id)))))
    (error "ID regex must be a non-empty string when provided"))
  (let* ((base-dir (rb-object-store--base-dir))
         (class-filename (rb-object-store--sanitize-path-component class))
         (class-dir (expand-file-name class-filename base-dir)))
    (unless (file-directory-p class-dir)
      (cl-return-from rb-object-store-find-objects nil))
    (let* ((filters (rb-object-store--properties-from-args properties))
           (candidates (rb-object-store--list-object-ids class)))
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
      (cl-loop
       while (and filters candidates)
       for (pname regex) on filters by #'cddr do
       (unless (keywordp pname)
         (error "Property name must be a keyword"))
       (unless (and (stringp regex) (not (string-empty-p regex)))
         (error "Property regex must be a non-empty string"))
       (let* ((pname-str (rb-object-store--object-key->string pname))
              (prop-filename (rb-object-store--sanitize-path-component pname-str))
              (output (rb-rg regex class-dir (format "--glob %s" prop-filename)))
              (match-set (make-hash-table :test #'equal)))
         (dolist (line (split-string output "\n" t))
           (when (string-match "^\\(.*\\):[0-9]+:" line)
             (let* ((matched-path (match-string 1 line))
                    (abs-path (expand-file-name matched-path class-dir))
                    (relative (file-relative-name abs-path class-dir))
                    (oid (car (split-string relative "/" t))))
               (when oid (puthash oid t match-set)))))
         (setq candidates (seq-filter (lambda (oid) (gethash oid match-set)) candidates))))
      (mapcar (lambda (oid) (rb-object-store-get-object class oid)) candidates))))


;;; Unit tests -----------------------------------------------------------------
(require 'ert)

(ert-deftest rb-object-store--validate-object/accepts-valid-story ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'STORY))
         (obj '(:class "STORY" :id "STORY-8" :name "Login" :description "As a user...")))
    (should (rb-object-store--validate-object obj schema))))

(ert-deftest rb-object-store--validate-object/rejects-missing-required-const ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'STORY))
         (obj '(:id "STORY-8" :name "Login")))
    (should-error (rb-object-store--validate-object obj schema))))

(ert-deftest rb-object-store--validate-object/rejects-wrong-type ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'TASK))
         (obj '(:class "TASK" :id 2 :name "Implement" :story "not-integer")))
    (should-error (rb-object-store--validate-object obj schema))))

(ert-deftest rb-object-store--validate-object/rejects-wrong-const ()
  (let* ((schema (rb-object-store--get-json-schema-for-class 'TASK))
         (obj '(:class "STORY" :id "STORY-3" :name "Mismatch")))
    (should-error (rb-object-store--validate-object obj schema))))

(ert-deftest rb-object-store--property-required-p/const-and-required ()
  (let ((schema '( :type "object"
                   :properties ( :class (:const "STORY")
                                 :name (:type "string")
                                 :description (:type "string"))
                   :required ("name"))))
    (should (rb-object-store--property-required-p :class schema))
    (should (rb-object-store--property-required-p :name schema))
    (should-not (rb-object-store--property-required-p :description schema))))

(ert-deftest rb-object-store--object-key->keyword/handles-input-types ()
  (should (equal (rb-object-store--object-key->keyword :foo) :foo))
  (should (equal (rb-object-store--object-key->keyword 'bar) :bar))
  (should (equal (rb-object-store--object-key->keyword "baz") :baz)))

(ert-deftest rb-object-store--object-key->string/handles-input-types ()
  (should (equal (rb-object-store--object-key->string :foo) "foo"))
  (should (equal (rb-object-store--object-key->string 'bar) "bar"))
  (should (equal (rb-object-store--object-key->string "baz") "baz")))

(ert-deftest rb-object-store--sanitize-path-component/handles-slashes-and-empty ()
  (should (equal (rb-object-store--sanitize-path-component "foo/bar") "foo_bar"))
  (should-error (rb-object-store--sanitize-path-component "")))

(ert-deftest rb-object-store--object-from-args/validates-reserved-and-duplicates ()
  (let ((valid (rb-object-store--object-from-args "STORY" "STORY-1"
                                                  '((:name "name" :value "Example")))))
    (should (equal valid '(:class "STORY" :id "STORY-1" :name "Example"))))
  (should-error (rb-object-store--object-from-args "TASK" "TASK-1"
                                                   '((:name "class" :value "TASK"))))
  (should-error (rb-object-store--object-from-args "TASK" "TASK-1"
                                                   '((:name "name" :value "A")
                                                     (:name "name" :value "B")))))

(ert-deftest rb-object-store-insert-update-and-get-object ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (let ((inserted (rb-object-store-insert-object
                       "STORY" "STORY-1-test"
                       '((:name "name" :value "First")
                         (:name "description" :value "details")))))
        (should (equal (plist-get inserted :class) "STORY"))
        (should (equal (plist-get inserted :id) "STORY-1-test"))
        (should (equal (plist-get inserted :name) "First"))
        (should (equal (plist-get inserted :description) "details"))
        (let ((retrieved (rb-object-store-get-object "STORY" "STORY-1-test")))
          (should (equal (plist-get retrieved :name) "First")))
        (let ((updated (rb-object-store-update-object "STORY" "STORY-1-test"
                                                      '((:name "name" :value "Updated")
                                                        (:name "description" :value "details-2")))))
          (should (equal (plist-get updated :name) "Updated"))
          (should (equal (plist-get updated :description) "details-2"))))
      (should-error (rb-object-store-update-object "STORY" "STORY-1-test"
                                                   '((:name "class" :value "STORY")))))))

(ert-deftest rb-object-store-find-objects/filters-and-returns-nil ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (rb-object-store-insert-object "STORY" "STORY-1-search" '((:name "name" :value "First")))
      (rb-object-store-insert-object "STORY" "STORY-2-search" '((:name "name" :value "Second")))
      (should (equal (length (rb-object-store-find-objects "STORY" nil nil)) 2))
      (should (equal (length (rb-object-store-find-objects "STORY" "STORY-1" nil)) 1))
      (should (equal (length (rb-object-store-find-objects "STORY" nil '(( :name "name"
                                                                           :value "Second")))) 1))
      (should-not (rb-object-store-find-objects "STORY" "UNKNOWN" nil)))))

(ert-deftest rb-object-store-find-objects/list-object-ids-parallel-to-insert ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (rb-object-store-insert-object "STORY" "STORY-3-list" '((:name "name" :value "Listing")))
      (rb-object-store-insert-object "STORY" "STORY-4-list" '((:name "name" :value "Listing")))
      (let ((ids (rb-object-store--list-object-ids "STORY")))
        (should (member "STORY-3-list" ids))
        (should (member "STORY-4-list" ids))))))

(ert-deftest rb-object-store-find-objects/errors-on-nonexistent-class ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (should-not (rb-object-store-find-objects "MISSING")))))

(ert-deftest rb-object-store-find-objects/property-filter-invalid ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (rb-object-store-insert-object "STORY" "STORY-5-filter" '((:name "name" :value "Filter")))
      (should-error (rb-object-store-find-objects "STORY" nil '((:name nil :value "x")))))))

(ert-deftest rb-object-store-find-objects/name-filter-empty-regex ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (rb-object-store-insert-object "STORY" "STORY-6-filter" '((:name "name" :value "Filter")))
      (should-error (rb-object-store-find-objects "STORY" nil '((:name "name" :value "")))))))

(ert-deftest rb-object-store-get-json-schema-for-class/accepts-string-argument ()
  (let ((schema (rb-object-store-get-json-schema-for-class "STORY")))
    (should (string= (plist-get schema :type) "object"))))

(ert-deftest rb-object-store-update-object/unknown-property-error ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (rb-object-store-insert-object "STORY" "STORY-7-update"
                                     '((:name "name" :value "Update")))
      (should-error (rb-object-store-update-object "STORY" "STORY-7-update"
                                                   '((:name "foo" :value "bar")))))))

(ert-deftest rb-object-store--property-value-satisfies-type-p/handles-supported-types ()
  (should (rb-object-store--property-value-satisfies-type-p "foo" "string"))
  (should (rb-object-store--property-value-satisfies-type-p 42 "integer"))
  (should (rb-object-store--property-value-satisfies-type-p 3.14 "number"))
  (should (rb-object-store--property-value-satisfies-type-p t "boolean"))
  (should (rb-object-store--property-value-satisfies-type-p nil "boolean"))
  (should-error (rb-object-store--property-value-satisfies-type-p "foo" "unknown")))

(ert-deftest rb-object-store--validate-property/errors-on-type-and-const-mismatch ()
  (should-error (rb-object-store--validate-property :name 1 '(:type "string")))
  (should-error (rb-object-store--validate-property :story "TASK" '(:const "STORY"))))

(ert-deftest rb-object-store--object-from-args/validates-input-types ()
  (should-error (rb-object-store--object-from-args 1 "STORY-1" (list :name "Example")))
  (should-error (rb-object-store--object-from-args "STORY" 1 (list :name "Example")))
  (should-error (rb-object-store--object-from-args "STORY" "STORY-1" (list "name" "Example")))
  (should-error (rb-object-store--object-from-args "STORY" "STORY-1" (list :name 123))))

(ert-deftest rb-object-store--object-dir/sanitizes-components ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (let ((path (rb-object-store--object-dir "ST/ORY" "TASK/1")))
        (should (string-match-p "ST_ORY" path))
        (should (string-match-p "TASK_1" path))
        (should (string-prefix-p (expand-file-name ".store" root) path))))))

(ert-deftest rb-object-store--write-object-to-filesystem/enforces-string-properties ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (should-error
       (rb-object-store--write-object-to-filesystem
        '(:class "STORY" :id "STORY-100" :name 42))))))

(ert-deftest rb-object-store-get-object/returns-requested-properties ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (rb-object-store-insert-object "STORY" "STORY-11" '((:name "name" :value "Name")
                                                          (:name "description" :value "Desc")))
      (let ((subset (rb-object-store-get-object "STORY" "STORY-11" (list "name"))))
        (should (equal (plist-get subset :name) "Name"))
        (should-not (plist-get subset :description))))))

(ert-deftest rb-object-store-get-object/error-on-missing-requested-property ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (rb-object-store-insert-object "STORY" "STORY-12" '((:name "name" :value "Name")
                                                          (:name "description" :value "Desc")))
      (let* ((object-dir (rb-object-store--object-dir "STORY" "STORY-12"))
             (file (expand-file-name (rb-object-store--sanitize-path-component "description")
                                     object-dir)))
        (delete-file file)
        (should-error (rb-object-store-get-object "STORY" "STORY-12" (list "description")))))))

(ert-deftest rb-object-store--properties-from-args/converts-to-keyword-plist ()
  (should (equal (rb-object-store--properties-from-args
                  '((:name "name" :value "Name")
                    (:name "description" :value "Desc")))
                 '(:name "Name" :description "Desc"))))

(ert-deftest rb-object-store-get-json-schema-for-class/errors-on-invalid-class ()
  (should-error (rb-object-store-get-json-schema-for-class nil)))

(ert-deftest rb-object-store--base-dir/respects-root-override ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (should (string= (rb-object-store--base-dir)
                       (expand-file-name ".store" root))))))

(ert-deftest rb-object-store-insert-object/validates-class-and-id-types ()
  (rb-tools-with-temp-dir root
    (let ((rb-object-store-root root))
      (should-error (rb-object-store-insert-object 1 "STORY-1"
                                                   '((:name "name" :value "Name"))))
      (should-error (rb-object-store-insert-object "STORY" 1
                                                   '((:name "name" :value "Name")))))))

(ert-deftest rb-object-store-find-objects/id-filter-invalid-type ()
  (should-error (rb-object-store-find-objects "STORY" 1 nil)))

(ert-deftest rb-object-store-find-objects/id-filter-empty-string ()
  (should-error (rb-object-store-find-objects "STORY" "" nil)))

(provide 'rb-object-store)
;;; rb-object-store.el ends here
