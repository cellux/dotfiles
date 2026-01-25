;;; rb-tools.el --- RB's helper tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for RB's GPT/Tree-sitter and tooling setup.

;;; Code:

(require 'cl-generic)
(require 'cl-lib)
(require 'subr-x)
(require 'treesit)

(declare-function cider-nrepl-sync-request:eval "cider-client")
(declare-function nrepl-dict-contains "nrepl-dict")
(declare-function nrepl-dict-get "nrepl-dict")

(declare-function gptel-make-preset "gptel")
(declare-function gptel-make-tool "gptel-request")

(defun rb-tools--truthy? (arg)
  "Return the Emacs Lisp truth value of the boolean argument ARG from a tool call."
  (when arg
    (if (eq arg :json-false)
        nil
      t)))

(defun rb-tools-clojure-eval (input &optional ns)
  "Evaluate INPUT in the connected Clojure/ClojureScript REPL in namespace NS.
If NS is not given, evaluation happens in the current namespace of the REPL.
The first line of the response is OK if the call succeeded, ERROR if it did not.
The rest of the response contains the evaluation result or the error message."
  (let* ((response (cider-nrepl-sync-request:eval input nil ns)))
    (if (nrepl-dict-contains response "err")
        (format "ERROR\n%s" (nrepl-dict-get response "err"))
      (format "OK\n%s" (nrepl-dict-get response "value")))))

(defun rb-tools-elisp-eval (form)
  "Evaluate FORM in the connected Emacs instance and return the textual result.

The first line of the response is OK if the call succeeded, ERROR if it did not.
The rest of the response contains the evaluation result or the error message."
  (unless (and (stringp form) (not (string-empty-p form)))
    (error "Input expression is required"))
  (condition-case err
      (let ((value (with-temp-buffer
                     (insert form)
                     (goto-char (point-min))
                     (eval (read (current-buffer))))))
        (format "OK\n%s" (prin1-to-string value)))
    (error (format "ERROR\n%s" (error-message-string err)))))

(defun rb-tools-bash-eval (script)
  "Execute SCRIPT via Bash and return a plist describing the result.
SCRIPT must be a non-empty string.  The script is executed through `bash
-c' and stdout/stderr are captured.  The returned plist contains
:exit_code, :stdout, and :stderr."
  (unless (and (stringp script) (not (string-empty-p script)))
    (error "Script string is required"))
  (let* ((bash (or (executable-find "bash")
                   (error "Executable not found: bash")))
         (stdout (generate-new-buffer "*rb-tools-bash-eval-stdout*"))
         (stderr (generate-new-buffer "*rb-tools-bash-eval-stderr*")))
    (unwind-protect
        (let* ((process (make-process :name "rb-tools-bash-eval"
                                      :buffer stdout
                                      :command (list bash "-c" script)
                                      :noquery t
                                      :stderr stderr
                                      :sentinel (lambda (_proc _event) nil))))
          (while (process-live-p process)
            (accept-process-output process))
          (let* ((exit-code (process-exit-status process))
                 (stdout-str (with-current-buffer stdout (buffer-string)))
                 (stderr-str (with-current-buffer stderr (buffer-string))))
            (list :exit_code exit-code
                  :stdout stdout-str
                  :stderr stderr-str)))
      (dolist (buffer (list stdout stderr))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defvar rb-tools-major-mode-to-ts-lang-alist
  '((emacs-lisp-mode . elisp)
    (go-mode . go)))

;;; Configuration and IO injection ---------------------------------------

(defvar rb-tools-object-store-root nil
  "Override for the object store base directory.
When non-nil, object store files are written under this root instead of the
current project root.")

(defvar rb-tools-ts-format-function #'indent-region
  "Function used to format updated Tree-sitter nodes.
Called with START and END when formatting is requested.  Bind to nil to
disable formatting.")

(defvar rb-tools-io-read-file-function #'rb-tools--default-io-read-file
  "Function used to read a whole file into a string.
It must accept a PATH argument and return the file contents as a string.")

(defvar rb-tools-io-write-file-function #'rb-tools--default-io-write-file
  "Function used to write a string to PATH.
It must accept PATH and CONTENT arguments.")

(defun rb-tools--io-read-file (path)
  "Read PATH using `rb-tools-io-read-file-function'."
  (funcall rb-tools-io-read-file-function path))

(defun rb-tools--io-write-file (path content)
  "Write CONTENT to PATH using `rb-tools-io-write-file-function'."
  (funcall rb-tools-io-write-file-function path content))

(defun rb-tools--default-io-read-file (path)
  "Default implementation to read PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun rb-tools--default-io-write-file (path content)
  "Default implementation to write CONTENT to PATH."
  (with-temp-file path
    (insert content)))

(defun rb-tools--ts-node-successfully-parsed? (node)
  "Determine if Tree-sitter could parse NODE without errors or missing parts.
Returns nil if the parse failed and a non-nil value otherwise."
  (and node (not (treesit-search-subtree
                  node
                  (lambda (node)
                    (or (treesit-node-check node 'has-error)
                        (treesit-node-check node 'missing)))))))

(defun rb-tools--ts-node-parse-errors (node)
  "Return a report of parse errors in NODE's subtree, or nil if none."
  (when node
    (let (errors)
      (treesit-search-subtree
       node
       (lambda (n)
         (let* ((node-type (treesit-node-type n))
                (error? (string= node-type "ERROR"))
                (missing? (treesit-node-check n 'missing)))
           (when (or error? missing?)
             (push (list :type node-type
                         :line (line-number-at-pos (treesit-node-start n))
                         :missing missing?
                         :text (unless missing? (treesit-node-text n t)))
                   errors)))))
      (when errors
        (mapconcat
         (lambda (err)
           (format "line %d: %s %s"
                   (plist-get err :line)
                   (plist-get err :type)
                   (if (plist-get err :missing) "(missing)" (plist-get err :text))))
         (nreverse errors)
         "\n")))))

(defun rb-tools--ts-with-root (path require-writable fn)
  "Open PATH in a temp buffer, ensure mode/Tree-sitter ready, and run FN.
FN gets the Tree-sitter PARSER and root NODE as arguments.
If REQUIRE-WRITABLE is non-nil, verify that path is writable."
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (when require-writable
    (unless (file-writable-p path)
      (error "File is not writable: %s" path)))
  (with-temp-buffer
    (insert-file-contents path)
    ;; Make mode detection behave as if visiting PATH.
    (setq buffer-file-name path)
    (unwind-protect
        (progn
          (delay-mode-hooks
            (set-auto-mode))
          (let ((language (alist-get major-mode rb-tools-major-mode-to-ts-lang-alist)))
            (unless language
              (error "No Tree-sitter language configured for %s" major-mode))
            (unless (treesit-ready-p language t)
              (error "Tree-sitter not ready for language %s" language))
            (let* ((parser (treesit-parser-create language))
                   (root (treesit-parser-root-node parser)))
              (unless (rb-tools--ts-node-successfully-parsed? root)
                (error (or (rb-tools--ts-node-parse-errors root)
                           (format "Tree-sitter failed to parse %s" path))))
              (funcall fn parser root))))
      (set-buffer-modified-p nil))))

(defun rb-tools--ts-validate-node (root spec)
  "Find child node of ROOT matching SPEC and return (NODE TEXT KIND LINE HASH)."
  (let* ((child-count (treesit-node-child-count root t))
         (idx (plist-get spec :index)))
    (unless (and (integerp idx) (>= idx 0) (< idx child-count))
      (error "Invalid node index: %s" idx))
    (let* ((node (treesit-node-child root idx t))
           (text (treesit-node-text node))
           (kind (treesit-node-type node))
           (line (line-number-at-pos (treesit-node-start node)))
           (hash (secure-hash 'md5 text)))
      (unless (string= (plist-get spec :kind) kind)
        (error "Kind mismatch for node %s: expected %s, got %s"
               idx (plist-get spec :kind) kind))
      (unless (= (plist-get spec :line) line)
        (error "Line mismatch for node %s: expected %s, got %s"
               idx (plist-get spec :line) line))
      (unless (string= (plist-get spec :text_hash) hash)
        (error "Hash mismatch for node %s" idx))
      (list node text kind line hash))))

(defun rb-tools--ts-collect-root-children (root)
  "Return child nodes of ROOT as plists with :index, :kind, :line, :text."
  (let ((child-count (treesit-node-child-count root t))
        (result '()))
    (dotimes (i child-count)
      (let* ((node (treesit-node-child root i t))
             (text (treesit-node-text node))
             (line (line-number-at-pos (treesit-node-start node))))
        (push (list :index i
                    :kind (treesit-node-type node)
                    :line line
                    :text text)
              result)))
    (nreverse result)))

(defun rb-tools--ts-format-list-nodes (children &optional preview-lines)
  "Return formatted node plists from CHILDREN for list-nodes output.
CHILDREN is a list of plists containing :index, :kind, :line, and :text.
PREVIEW-LINES controls preview length; defaults to 1 when nil or non-positive."
  (let ((preview-count (if (and (integerp preview-lines)
                                (> preview-lines 0))
                           preview-lines
                         1)))
    (mapcar
     (lambda (child)
       (let* ((text (plist-get child :text))
              (lines (split-string text "\n"))
              (preview (string-join (seq-take lines preview-count) "\n")))
         (list :index (plist-get child :index)
               :kind (plist-get child :kind)
               :line (plist-get child :line)
               :text_hash (secure-hash 'md5 text)
               :preview preview)))
     children)))

(defun rb-tools--ts-select-nodes-by-line (children line-numbers)
  "Return nodes from CHILDREN whose :line matches LINE-NUMBERS.
LINE-NUMBERS is a list of starting lines.
Raises an error if any line is missing."
  (let ((lines (seq-into line-numbers 'list)))
    (mapcar
     (lambda (line)
       (unless (integerp line)
         (error "Line numbers must be integers, got %s" line))
       (let ((child (seq-find (lambda (c) (= (plist-get c :line) line)) children)))
         (unless child
           (error "No node starts on line %s" line))
         (let ((text (plist-get child :text)))
           (list :index (plist-get child :index)
                 :kind (plist-get child :kind)
                 :line line
                 :text_hash (secure-hash 'md5 text)
                 :text text))))
     lines)))

(defun rb-tools--ts-resolve-node (root selector)
  "Resolve a unique top-level node in ROOT matching SELECTOR.

SELECTOR is a plist that may contain :text_hash (string) and/or :line
 (integer).  At least one selector must be provided.  When both are
provided, both must refer to the same node.

Returns a plist with :node, :index, :kind, :line, :start, :end, :text,
and :text_hash.  Signals an error on missing selectors, missing matches,
or ambiguous matches."
  (let* ((line (plist-get selector :line))
         (text-hash (plist-get selector :text_hash)))
    (unless (or line text-hash)
      (error "Selector must provide :line and/or :text_hash"))
    (let ((child-count (treesit-node-child-count root t))
          line-match
          hash-matches)
      (dotimes (i child-count)
        (let* ((node (treesit-node-child root i t))
               (start (treesit-node-start node))
               (end (treesit-node-end node))
               (kind (treesit-node-type node))
               (text (treesit-node-text node))
               (line-num (line-number-at-pos start))
               (hash (secure-hash 'md5 text))
               (entry (list :node node
                            :index i
                            :kind kind
                            :line line-num
                            :start start
                            :end end
                            :text text
                            :text_hash hash)))
          (when (and line (= line line-num))
            (setq line-match entry))
          (when (and text-hash (string= text-hash hash))
            (push entry hash-matches))))
      (when (and line (not line-match))
        (error "No node starts on line %s" line))
      (when (and text-hash (null hash-matches))
        (error "No node matches text_hash %s" text-hash))
      (when (> (length hash-matches) 1)
        (error "Multiple nodes match text_hash %s; disambiguate with :line" text-hash))
      (let (candidate)
        (cond
         ((and line text-hash)
          (unless (and line-match
                       (string= text-hash (plist-get line-match :text_hash)))
            (error "Line %s and text_hash did not match the same node" line))
          (setq candidate line-match))
         (line
          (setq candidate line-match))
         (text-hash
          (setq candidate (car hash-matches))))
        (unless candidate
          (error "Failed to resolve node"))
        candidate))))

(defun rb-tools--ts-list-nodes-pure (root &optional preview-lines)
  "Pure worker for `rb-tools-ts-list-nodes'.
Accepts ROOT and returns formatted node plists.
PREVIEW-LINES controls preview length."
  (rb-tools--ts-format-list-nodes
   (rb-tools--ts-collect-root-children root)
   preview-lines))

(defun rb-tools--ts-get-nodes-pure (root line-numbers)
  "Pure worker for `rb-tools-ts-get-nodes'.
Accepts ROOT and LINE-NUMBERS."
  (rb-tools--ts-select-nodes-by-line
   (rb-tools--ts-collect-root-children root)
   line-numbers))

(defun rb-tools-ts-list-nodes (path &optional preview-lines)
  "Parse PATH using Tree-sitter and return the list of top-level AST nodes.

Each AST node has the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- preview: string - first PREVIEW-LINES lines of node text

PREVIEW-LINES controls how many lines are included in each preview (default: 1)."
  (rb-tools--ts-with-root
   path nil
   (lambda (_parser root)
     (rb-tools--ts-list-nodes-pure root preview-lines))))

(defun rb-tools-ts-get-nodes (path line-numbers)
  "Parse PATH using Tree-sitter and return the text of nodes on LINE-NUMBERS.

LINE-NUMBERS should be a list of integer line numbers referencing the start
of each target node.

Each element in the returned list contains the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- text: string - full node text"
  (rb-tools--ts-with-root
   path nil
   (lambda (_parser root)
     (rb-tools--ts-get-nodes-pure root line-numbers))))

(defun rb-tools-ts-update-nodes (path nodes &optional skip-format dry-run)
  "Parse PATH using Tree-sitter and update specified NODES.

Each element in NODES must contain the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- new_text: string - new node text

The target file is first loaded into a temporary buffer and the
requested updates are applied in decreasing line number order.  After
all changes have been made, the buffer is reparsed.  The target file is
updated only if the parse is successful.

Updated nodes are formatted via `indent-region' unless SKIP-FORMAT is
non-nil.

If DRY-RUN is non-nil, apply the edits in-memory, reparse to validate,
and return a report without writing PATH.  The report includes the
planned changes, the reparse result, and a flag indicating no write
occurred.

On success, returns the list of updated nodes (or the dry-run report):

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of new node text"
  (rb-tools--ts-with-root
   path t
   (lambda (parser _root)
     (let* ((updates (sort (seq-into nodes 'list)
                           (lambda (a b)
                             (> (plist-get a :line)
                                (plist-get b :line)))))
            (changes '()))
       (dolist (spec updates)
         (unless (stringp (plist-get spec :new_text))
           (error "Missing new_text for node %s" (plist-get spec :index)))
         (let* ((latest-root (treesit-buffer-root-node)) ; root changes after each update
                (validated (rb-tools--ts-validate-node latest-root spec))
                (node (nth 0 validated))
                (old-text (treesit-node-text node))
                (new-text (plist-get spec :new_text)))
           (let ((start (treesit-node-start node))
                 (end (treesit-node-end node)))
             (goto-char start)
             (delete-region start end)
             (insert new-text)
             (let ((fmt-fn (and (not (rb-tools--truthy? skip-format))
                                rb-tools-ts-format-function)))
               (when fmt-fn
                 (funcall fmt-fn start (point)))))
           (push (list :index (plist-get spec :index)
                       :kind (plist-get spec :kind)
                       :line (plist-get spec :line)
                       :old_text_hash (secure-hash 'md5 old-text)
                       :new_text_hash (secure-hash 'md5 new-text)
                       :old_text old-text
                       :new_text new-text)
                 changes)))
       (let* ((root2 (treesit-parser-root-node parser)))
         (unless (rb-tools--ts-node-successfully-parsed? root2)
           (error (or (rb-tools--ts-node-parse-errors root2)
                      "Tree-sitter reparsing failed")))
         (let* ((child-count (treesit-node-child-count root2 t))
                (result '()))
           (dotimes (i child-count)
             (let* ((node (treesit-node-child root2 i t))
                    (text (treesit-node-text node))
                    (line (line-number-at-pos (treesit-node-start node))))
               (push (list :index i
                           :kind (treesit-node-type node)
                           :line line
                           :text_hash (secure-hash 'md5 text))
                     result)))
           (if (rb-tools--truthy? dry-run)
               (list :dry_run t
                     :changes (nreverse changes)
                     :result (nreverse result))
             (progn
               (rb-tools--io-write-file path (buffer-string))
               (nreverse result)))))))))

(defun rb-tools--ts-insert-relative (path selector new-text position &optional skip-format)
  "Helper to insert NEW-TEXT relative to a resolved node in PATH.
POSITION is either :before or :after.  SELECTOR is passed to
`rb-tools--ts-resolve-node'.  Formats inserted region unless SKIP-FORMAT
is non-nil.  Writes the file only after Tree-sitter reparsing succeeds.
If the insert was successful, returns plist (:inserted t)."
  (unless (member position '(:before :after))
    (error "POSITION must be :before or :after"))
  (unless (stringp new-text)
    (error "NEW-TEXT must be a string"))
  (rb-tools--ts-with-root
   path t
   (lambda (parser root)
     (let* ((resolved (rb-tools--ts-resolve-node root selector))
            (insert-pos (if (eq position :before)
                            (plist-get resolved :start)
                          ;; :end points to last char of the node
                          (1+ (plist-get resolved :end)))))
       (goto-char insert-pos)
       (insert new-text)
       (let ((fmt-fn (and (not (rb-tools--truthy? skip-format))
                          rb-tools-ts-format-function))
             (insert-end (point)))
         (when fmt-fn
           (funcall fmt-fn insert-pos insert-end)))
       (let ((root2 (treesit-parser-root-node parser)))
         (unless (rb-tools--ts-node-successfully-parsed? root2)
           (error (or (rb-tools--ts-node-parse-errors root2)
                      "Tree-sitter reparsing failed")))
         (rb-tools--io-write-file path (buffer-string))
         (list :inserted t))))))

(defun rb-tools-ts-insert-before-node (path selector new-text &optional skip-format)
  "Insert NEW-TEXT before the node selected by SELECTOR in PATH.

SELECTOR is a plist that may contain :text_hash (string) and/or :line
 (integer) referring to a top-level node.  The insertion is performed in
memory, Tree-sitter is asked to reparse, and the file is written only if
parsing succeeds.  The inserted region is formatted unless SKIP-FORMAT
is truthy.

If the insert was successful, returns plist (:inserted t)."
  (rb-tools--ts-insert-relative path selector new-text :before skip-format))

(defun rb-tools-ts-insert-after-node (path selector new-text &optional skip-format)
  "Insert NEW-TEXT after the node selected by SELECTOR in PATH.

SELECTOR is a plist that may contain :text_hash (string) and/or :line
 (integer) referring to a top-level node.  The insertion is performed in
memory, Tree-sitter is asked to reparse, and the file is written only if
parsing succeeds.  The inserted region is formatted unless SKIP-FORMAT
is truthy.

If the insert was successful, returns plist (:inserted t)."
  (rb-tools--ts-insert-relative path selector new-text :after skip-format))

(defun rb-tools-get-line-ranges (path ranges)
  "Return the exact text for each line range in RANGES from PATH.

RANGES is a list of plists with :start and :end (1-based, inclusive).
Returns a list of plists (:start :end :text :text_hash) in normalized
order (sorted descending by :start)."
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((total-lines (line-number-at-pos (point-max)))
           (normalized (rb-tools--normalize-line-range-specs ranges total-lines nil nil))
           (result '()))
      (dolist (spec normalized)
        (let* ((start (plist-get spec :start))
               (end (plist-get spec :end))
               (text (rb-tools--line-range-slice start end)))
          (push (list :start start
                      :end end
                      :text text
                      :text_hash (secure-hash 'md5 text))
                result)))
      (nreverse result))))

(defun rb-tools-update-line-ranges (path ranges)
  "Update the specified line RANGES in PATH with verification.

Each element of RANGES contains the following fields:

- :start integer (1-based inclusive)
- :end integer (1-based inclusive)
- :text_hash string (MD5 of existing slice)
- :new_text string (replacement text)

All ranges are validated, sorted descending by :start, verified against
the current file contents, and applied in-memory.  The file is written
only after all checks pass.  Returns a list of plists (:start :end
:updated t) in normalized order."
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (unless (file-writable-p path)
    (error "File is not writable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((total-lines (line-number-at-pos (point-max)))
           (normalized (rb-tools--normalize-line-range-specs ranges total-lines t t))
           (result '()))
      (dolist (spec normalized)
        (let* ((start (plist-get spec :start))
               (end (plist-get spec :end))
               (new-text (plist-get spec :new_text))
               (expected-hash (plist-get spec :text_hash))
               (existing (rb-tools--line-range-slice start end))
               (existing-hash (secure-hash 'md5 existing)))
          (when (not (string= existing-hash expected-hash))
            (error "Hash mismatch for range %s-%s" start end))
          (pcase-let ((`(,start-pos . ,end-pos) (rb-tools--line-range-boundaries start end)))
            (goto-char start-pos)
            (delete-region start-pos end-pos)
            (insert new-text))
          (push (list :start start :end end :updated t) result)))
      (rb-tools--io-write-file path (buffer-string))
      (nreverse result))))

(defun rb-tools-read-file (path &optional with-line-numbers)
  "Read the file at PATH and return its content.
If WITH-LINE-NUMBERS is non-nil, prefix each line with its number."
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (if (rb-tools--truthy? with-line-numbers)
        (let ((lines '())
              (line-number 1))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (push (format "%d:%s" line-number line) lines))
            (forward-line 1)
            (setq line-number (1+ line-number)))
          (let ((result (string-join (nreverse lines) "\n")))
            (when (and (not (string-empty-p result))
                       (eq (char-before (point-max)) ?\n))
              (setq result (concat result "\n")))
            result))
      (buffer-string))))

(defun rb-tools-write-file (path content)
  "Write CONTENT to the file at PATH."
  (unless (file-writable-p path)
    (error "File is not writable: %s" path))
  (with-temp-file path
    (insert content)))

(defun rb-tools-rg (pattern &optional directory extra-args)
  "Run ripgrep PATTERN inside DIRECTORY and return its output as a string.

EXTRA-ARGS is a string of additional flags passed to rg, parsed with
`split-string-and-unquote'."
  (unless (and (stringp pattern) (not (string-empty-p pattern)))
    (error "Pattern is required"))
  (let* ((rg-program (or (executable-find "rg")
                         (error "Executable not found: rg")))
         (target-dir (if (and directory (not (string-empty-p directory)))
                         (expand-file-name directory)
                       default-directory)))
    (unless (file-directory-p target-dir)
      (error "Not a directory: %s" target-dir))
    (let ((default-directory target-dir))
      (with-temp-buffer
        (let* ((args (append '("--line-number" "--color" "never" "--no-heading")
                             (when (and extra-args (not (string-empty-p extra-args)))
                               (split-string-and-unquote extra-args))
                             (list pattern ".")))
               (exit-code (apply #'process-file rg-program nil t nil args)))
          (cond
           ((<= exit-code 1)
            (string-trim-right (buffer-string)))
           (t
            (error "Executable rg failed with exit code %s\n%s"
                   exit-code
                   (string-trim-right (buffer-string))))))))))

(defun rb-tools-fd (query &optional directory extra-args)
  "Run fd QUERY inside DIRECTORY and return its output as a string.

EXTRA-ARGS is a string of additional flags passed to fd, parsed with
`split-string-and-unquote'.

Hidden directories are searched by default except the .git folder."
  (unless (and (stringp query) (not (string-empty-p query)))
    (error "Query is required"))
  (let* ((fd-program (or (executable-find "fd")
                         (error "Executable not found: fd")))
         (target-dir (if (and directory (not (string-empty-p directory)))
                         (expand-file-name directory)
                       default-directory)))
    (unless (file-directory-p target-dir)
      (error "Not a directory: %s" target-dir))
    (let ((default-directory target-dir))
      (with-temp-buffer
        (let* ((args (append '("--color" "never" "--hidden" "--follow"
                               "--exclude" ".git")
                             (when (and extra-args (not (string-empty-p extra-args)))
                               (split-string-and-unquote extra-args))
                             (list query ".")))
               (exit-code (apply #'process-file fd-program nil t nil args)))
          (if (= exit-code 0)
              (string-trim-right (buffer-string))
            (error "Executable fd failed with exit code %s\n%s"
                   exit-code
                   (string-trim-right (buffer-string)))))))))

;;; Line range tools API ---------------------------------

(defun rb-tools--line-range-boundaries (start end)
  "Return (START-POS . END-POS) for line range START..END in current buffer.
START/END are 1-based, inclusive and must already be validated.  END-POS
points to the beginning of the line after END (or `point-max' when END
is the last line)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start))
    (let ((start-pos (point)))
      (goto-char (point-min))
      (forward-line end)
      (cons start-pos (point)))))

(defun rb-tools--line-range-slice (start end)
  "Return the exact buffer substring for inclusive lines START..END."
  (pcase-let ((`(,start-pos . ,end-pos) (rb-tools--line-range-boundaries start end)))
    (buffer-substring-no-properties start-pos end-pos)))

(defun rb-tools--normalize-line-range-specs (ranges total-lines require-new-text require-verification)
  "Validate and normalize line range RANGES for a file with TOTAL-LINES.
Returns the specs sorted descending by :start.  Signals an error on invalid
input (out-of-bounds, overlaps, missing fields).

When REQUIRE-NEW-TEXT is non-nil, each spec must contain a string :new_text.
When REQUIRE-VERIFICATION is non-nil, each spec must provide :text_hash."
  (let* ((specs (seq-sort (lambda (a b)
                            (> (plist-get a :start)
                               (plist-get b :start)))
                          (seq-into ranges 'list)))
         (prev-start nil)
         (prev-end nil))
    (dolist (spec specs)
      (let* ((start (plist-get spec :start))
             (end (plist-get spec :end))
             (new-text (plist-get spec :new_text))
             (text-hash (plist-get spec :text_hash)))
        (unless (and (integerp start) (>= start 1))
          (error "Invalid start line: %s" start))
        (unless (and (integerp end) (>= end 1))
          (error "Invalid end line: %s" end))
        (when (> start total-lines)
          (error "Start line %s beyond end of file (%s)" start total-lines))
        (when (> end total-lines)
          (error "End line %s beyond end of file (%s)" end total-lines))
        (when (> start end)
          (error "Start line %s after end line %s" start end))
        (when (and require-new-text (not (stringp new-text)))
          (error "Missing new_text for range starting at %s" start))
        (when (and require-verification
                   (not (plist-member spec :text_hash)))
          (error "Missing verification for range starting at %s" start))
        (when (and (plist-member spec :text_hash) (not (stringp text-hash)))
          (error "Invalid text_hash for range starting at %s" start))
        (when (and prev-start (>= end prev-start))
          (error "Overlapping ranges %s-%s and %s-%s" start end prev-start prev-end))
        (setq prev-start start
              prev-end end)))
    specs))


;;; Regex match helpers -------------------------------------

(defun rb-tools--regex-match--parse-rg-line (line)
  "Return a match plist for LINE produced by ripgrep or nil if LINE is malformed."
  (when (and (stringp line)
             (not (string-empty-p line))
             (string-match "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)$" line))
    (let* ((file (match-string 1 line))
           (line-num (string-to-number (match-string 2 line)))
           (column (string-to-number (match-string 3 line)))
           (text (match-string 4 line)))
      (when (and (> line-num 0) (> column 0))
        (list :path file
              :line line-num
              :column column
              :text text)))))

(defun rb-tools--regex-match--closest (matches anchor-line)
  "Return the element of MATCHES whose :line is closest to ANCHOR-LINE.
If several such matches are found, pick the earliest match."
  (let ((best nil))
    (dolist (match matches)
      (let* ((line (plist-get match :line))
             (distance (abs (- line anchor-line))))
        (when (or (null best)
                  (< distance (plist-get best :distance))
                  (and (= distance (plist-get best :distance))
                       (< line (plist-get (plist-get best :match) :line))))
          (setq best (list :match match :distance distance)))))
    (plist-get best :match)))

(defun rb-tools--regex-match--matches-for-file (abs-path regex)
  "Return the ripgrep matches for REGEX within ABS-PATH."
  (let* ((dir (file-name-directory abs-path))
         (filename (file-name-nondirectory abs-path)))
    (unless (and dir (not (string-empty-p filename)))
      (error "Invalid file path: %s" abs-path))
    (let* ((extra (format "--column --glob %s" (shell-quote-argument filename)))
           (output (rb-tools-rg regex dir extra)))
      (if (string-empty-p output)
          nil
        (let ((lines (split-string output "\n" t))
              (matches '()))
          (dolist (line lines)
            (when-let ((match (rb-tools--regex-match--parse-rg-line line)))
              (unless (string= (file-name-nondirectory (plist-get match :path))
                               filename)
                (error "Ripgrep returned %s when searching %s" (plist-get match :path) abs-path))
              (push match matches)))
          (nreverse matches))))))

(defun rb-tools--regex-match--describe (abs-path match &optional anchor-line)
  "Return metadata for MATCH recorded in file at ABS-PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (condition-case err
          (insert-file-contents abs-path)
        (file-error
         (error "File %s is not valid UTF-8: %s" abs-path (error-message-string err)))))
    (let ((total-lines (line-number-at-pos (point-max)))
          (match-line (plist-get match :line)))
      (when (and anchor-line (> anchor-line total-lines))
        (error "Anchor line %d beyond end of file (%d lines) in %s"
               anchor-line total-lines abs-path))
      (unless (and (integerp match-line) (>= match-line 1) (<= match-line total-lines))
        (error "Ripgrep reported line %s outside 1-%s" match-line total-lines))
      (let ((bounds (rb-tools--line-range-boundaries match-line match-line)))
        (list :path abs-path
              :line match-line
              :column (plist-get match :column)
              :match (plist-get match :text)
              :before (car bounds)
              :after (cdr bounds))))))

(defun rb-tools--regex-match-info (path regex &optional anchor-line)
  "Return metadata about the unique line matching REGEX in PATH.

REGEX is evaluated using ripgrep's default Rust regex flavor and restricted
solely to PATH.  PATH must be readable, regular, and encoded in UTF-8.  When
REGEX matches multiple lines, supply the 1-based ANCHOR-LINE to pick the
match closest to that line; without the anchor an ambiguous match raises an
error.  Errors are also signaled for missing files, non-UTF-8 content, and
when the regex does not match any line.

The returned plist contains :path, :line, :column, :match, :before, and
:after, where :before/:after are character positions that bracket the
matched line and can be used for insertions."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "Path is required"))
  (unless (and (stringp regex) (not (string-empty-p regex)))
    (error "Regex is required"))
  (let* ((abs-path (expand-file-name path))
         (matches nil))
    (unless (file-readable-p abs-path)
      (error "File is not readable: %s" abs-path))
    (unless (file-regular-p abs-path)
      (error "Path is not a regular file: %s" abs-path))
    (when (and anchor-line (not (and (integerp anchor-line) (>= anchor-line 1))))
      (error "Anchor line must be an integer >= 1"))
    (setq matches (rb-tools--regex-match--matches-for-file abs-path regex))
    (when (null matches)
      (error "No matches for regex %s in %s" regex abs-path))
    (let ((selected
           (cond
            ((null anchor-line)
             (if (= (length matches) 1)
                 (car matches)
               (error "Regex %s matches %d lines in %s; provide :line to disambiguate"
                      regex (length matches) abs-path)))
            ((= (length matches) 1)
             (car matches))
            (t
             (rb-tools--regex-match--closest matches anchor-line)))))
      (rb-tools--regex-match--describe abs-path selected anchor-line))))

(defun rb-tools--regex-insert--validate-buffer ()
  "Re-parse the current buffer with Tree-sitter and signal errors.
The buffer-local variable `major-mode' must be already set according to
the buffer contents.  Returns non-nil when the parse succeeds; signals
with diagnostics otherwise."
  (let ((language (alist-get major-mode rb-tools-major-mode-to-ts-lang-alist)))
    (unless language
      (error "No Tree-sitter language configured for %s" major-mode))
    (unless (treesit-ready-p language t)
      (error "Tree-sitter not ready for language %s" language))
    (let* ((parser (treesit-parser-create language))
           (root (treesit-parser-root-node parser)))
      (unless (rb-tools--ts-node-successfully-parsed? root)
        (error (or (rb-tools--ts-node-parse-errors root)
                   (format "Tree-sitter failed to parse %s" (or buffer-file-name "buffer")))))
      t)))

(defun rb-tools--regex-insert (path regex new-text position &optional line validate)
  "Insert NEW-TEXT relative to the line matching REGEX in PATH.

REGEX is interpreted using ripgrep's default Rust regex flavor and is
restricted to PATH.  When REGEX matches multiple lines, supply the
1-based LINE anchor to select the occurrence closest to that line;
omitting LINE in that situation signals an error.  POSITION must be
either :before or :after and determines whether NEW-TEXT is inserted
before or after the matched line.  When VALIDATE is non-nil, the buffer
is re-parsed with Tree-sitter before writing so syntax errors are caught
early.

Returns a plist (:inserted t) on success."
  (unless (member position '(:before :after))
    (error "POSITION must be :before or :after"))
  (unless (stringp new-text)
    (error "NEW-TEXT must be a string"))
  (let* ((abs-path (expand-file-name path))
         (match (rb-tools--regex-match-info abs-path regex line))
         (insert-pos (if (eq position :before)
                         (plist-get match :before)
                       (plist-get match :after)))
         (validate? (rb-tools--truthy? validate)))
    (with-temp-buffer
      (insert-file-contents abs-path)
      (goto-char insert-pos)
      (insert new-text)
      (when validate?
        (setq buffer-file-name abs-path)
        (delay-mode-hooks
          (set-auto-mode))
        (rb-tools--regex-insert--validate-buffer))
      (rb-tools--io-write-file abs-path (buffer-string))
      (list :inserted t))))

(defun rb-tools-insert-before-regex (path regex new-text &optional line validate)
  "Insert NEW-TEXT immediately before the line matching REGEX in PATH.

REGEX follows ripgrep's default Rust regex syntax and is restricted to PATH.
When the expression matches multiple lines, supply the 1-based LINE anchor to
pick the occurrence closest to that line.  VALIDATE, when non-nil, re-parses
the edited buffer with Tree-sitter before writing so syntax errors are
reported instead of corrupting the file.  Returns (:inserted t) on success."
  (rb-tools--regex-insert path regex new-text :before line validate))

(defun rb-tools-insert-after-regex (path regex new-text &optional line validate)
  "Insert NEW-TEXT immediately after the line matching REGEX in PATH.

REGEX follows ripgrep's default Rust regex syntax and is restricted to PATH.
When the expression matches multiple lines, supply the 1-based LINE anchor to
pick the occurrence closest to that line.  VALIDATE, when non-nil, re-parses
the edited buffer with Tree-sitter before writing so syntax errors are
reported instead of corrupting the file.  Returns (:inserted t) on success."
  (rb-tools--regex-insert path regex new-text :after line validate))


;;; JSON schema for classes -----------------------

(defvar rb-tools--json-value-missing
  (make-symbol "rb-tools--json-value-missing")
  "Sentinel value used internally when a JSON property is absent.")

(defun rb-tools--json-key->string (key)
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

(defun rb-tools--normalize-json-object (obj)
  "Normalize OBJ (alist/plist/hash) into a hash table keyed by strings."
  (let ((table (make-hash-table :test #'equal)))
    (cond
     ((hash-table-p obj)
      (maphash (lambda (k v)
                 (puthash (rb-tools--json-key->string k) v table)) obj))
     ((and (listp obj) (seq-every-p #'consp obj))
      (mapc (lambda (entry)
              (let ((k (car entry))
                    (v (cdr entry)))
                (puthash (rb-tools--json-key->string k) v table)))
            obj))
     ((listp obj)
      (let ((plist (copy-sequence obj)))
        (unless (cl-evenp (length plist))
          (error "Property list must contain an even number of elements"))
        (while plist
          (let ((k (pop plist))
                (v (pop plist)))
            (puthash (rb-tools--json-key->string k) v table)))))
     (t
      (error "Cannot normalize JSON object: %s" obj)))
    table))

(defun rb-tools--json-schema-properties-alist (schema)
  "Return an alist of property definitions from SCHEMA."
  (let ((properties (plist-get schema :properties))
        (result '()))
    (while properties
      (let ((key (pop properties))
            (value (pop properties)))
        (push (cons (rb-tools--json-key->string key) value) result)))
    (nreverse result)))

(defun rb-tools--ensure-keyword (x)
  "Coerces X into a keyword."
  (if (keywordp x)
      x
    (intern (format ":%s" x))))

(defun rb-tools--json-schema-property-required-p (key schema)
  "Return non-nil if property KEY must be present according to SCHEMA."
  (let* ((properties (plist-get schema :properties))
         (definition (plist-get properties (rb-tools--ensure-keyword key)))
         (required (seq-into (plist-get schema :required) 'list)))
    (or (seq-contains-p required key)
        (plist-get definition :const))))

(defun rb-tools--json-value-satisfies-type-p (value type)
  "Return non-nil if VALUE satisfies TYPE from a JSON schema."
  (pcase type
    ("string" (stringp value))
    ("integer" (integerp value))
    ("number" (numberp value))
    ("boolean" (or (eq value t) (eq value nil)))
    (_ (error "Unsupported schema type: %s" type))))

(defun rb-tools--validate-json-property (key value definition)
  "Validate VALUE for property KEY using DEFINITION."
  (let ((type (plist-get definition :type))
        (const (plist-get definition :const)))
    (when type
      (unless (rb-tools--json-value-satisfies-type-p value type)
        (error "Property %s must be %s" key type)))
    (when const
      (unless (equal value const)
        (error "Property %s must be %s" key const)))))

(defun rb-tools--validate-json-object (obj schema)
  "Validate OBJ against SCHEMA.
Implements only a small subset of the JSON schema spec.
Usable with schemas sourced from `get_json_schema_for_class'."
  (unless (and schema (string= (plist-get schema :type) "object"))
    (error "Schema must describe an object"))
  (let ((properties (rb-tools--json-schema-properties-alist schema))
        (normalized (rb-tools--normalize-json-object obj)))
    (dolist (entry properties)
      (let* ((key (car entry))
             (definition (cdr entry))
             (value (gethash key normalized rb-tools--json-value-missing)))
        (if (eq value rb-tools--json-value-missing)
            (when (rb-tools--json-schema-property-required-p key schema)
              (error "Missing required property: %s" key))
          (rb-tools--validate-json-property key value definition))))
    t))

(cl-defgeneric rb-tools--get-json-schema-for-class (class)
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

(cl-defmethod rb-tools--get-json-schema-for-class ((_class (eql 'STORY)))
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

(cl-defmethod rb-tools--get-json-schema-for-class ((_class (eql 'TASK)))
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

(defun rb-tools-get-json-schema-for-class (class)
  "Return the JSON schema for object store class CLASS."
  (when (stringp class)
    (setq class (intern class)))
  (unless (symbolp class)
    (error "CLASS must be string or symbol, got %s" class))
  (rb-tools--get-json-schema-for-class class))


;;; Object store ----------------------------------------------------------

(require 'project)

(defun rb-tools--project-root ()
  "Return the current project root, defaulting to `default-directory'."
  (or (when (and (featurep 'projectile)
                 (fboundp 'projectile-project-root))
        (ignore-errors (projectile-project-root)))
      (when (fboundp 'project-current)
        (when-let* ((proj (project-current nil default-directory)))
          (project-root proj)))
      default-directory))

(defun rb-tools--sanitize-path-component (component)
  "Return COMPONENT as a safe path segment string."
  (let* ((raw (format "%s" component))
         (clean (replace-regexp-in-string "/" "_" raw)))
    (when (string-empty-p clean)
      (error "Path component cannot be empty"))
    clean))

(defun rb-tools--object-store-base-dir (class id)
  "Return filesystem path for object CLASS with ID.
When `rb-tools-object-store-root' is non-nil, use it; otherwise fall back to
`rb-tools--project-root'."
  (let* ((root (or rb-tools-object-store-root
                   (rb-tools--project-root)))
         (class-part (rb-tools--sanitize-path-component class))
         (id-part (rb-tools--sanitize-path-component id)))
    (expand-file-name (format ".store/%s/%s" class-part id-part) root)))

(defun rb-tools--extract-property (entry)
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

(defun rb-tools--object-from-args (class id properties)
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
      (pcase-let* ((`(,name . ,value) (rb-tools--extract-property entry)))
        (unless (and (stringp name) (not (string-empty-p name)))
          (error "Property name must be a non-empty string"))
        (when (member name '("class" "id"))
          (error "Property %s is reserved" name))
        (when (gethash name seen)
          (error "Duplicate property: %s" name))
        (puthash name t seen)
        (push (cons name value) object)))
    (nreverse object)))

(defun rb-tools--write-object-to-filesystem (object &optional overwrite)
  "Persist OBJECT alist to the object store.
OBJECT must contain at least keys `class' and `id'.
If OVERWRITE is non-nil, the object must already exist and its properties
are replaced.  When OVERWRITE is nil, an error is raised if the object
already exists."
  (let* ((class (cdr (assoc "class" object)))
         (id (cdr (assoc "id" object))))
    (unless class (error "Missing class property"))
    (unless id (error "Missing id property"))
    (let* ((base (rb-tools--object-store-base-dir class id))
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
               (key-str (rb-tools--sanitize-path-component key))
               (file (expand-file-name key-str base)))
          (unless (stringp val)
            (error "Property %s value must be a string" key))
          (rb-tools--io-write-file file val))))))

(defun rb-tools-insert-object (class id properties)
  "Insert a new OBJECT into the store.

CLASS (string), ID (string), and PROPERTIES (array of name/value pairs)
are combined via `rb-tools--object-from-args', validated against the
schema from `rb-tools-get-json-schema-for-class', and written to the
object store.

Returns the validated object alist."
  (let* ((object (rb-tools--object-from-args class id properties))
         (schema (rb-tools-get-json-schema-for-class class)))
    (rb-tools--validate-json-object object schema)
    (rb-tools--write-object-to-filesystem object nil)
    object))

(defun rb-tools--read-object-from-filesystem (class id &optional properties)
  "Return the object ID of CLASS from the store as an alist.
If PROPERTIES is non-nil, it must be a list/array of property names to
return; when PROPERTIES is nil or empty, return all properties."
  (let* ((base (rb-tools--object-store-base-dir class id))
         (prop-list (when properties (seq-into properties 'list))))
    (unless (file-directory-p base)
      (error "Object %s-%s does not exist" class id))
    (if (and prop-list (not (seq-empty-p prop-list)))
        (let ((keys prop-list)
              (result '()))
          (dolist (key keys)
            (unless (and (stringp key) (not (string-empty-p key)))
              (error "Property name must be a non-empty string: %s" key))
            (let ((file (expand-file-name (rb-tools--sanitize-path-component key) base)))
              (unless (file-readable-p file)
                (error "Missing property %s for object %s-%s" key class id))
              (push (cons key (rb-tools--io-read-file file)) result)))
          (nreverse result))
      (let ((files (directory-files base nil "^[^.].*" t))
            (result '()))
        (dolist (fname files)
          (let ((file (expand-file-name fname base)))
            (when (file-regular-p file)
              (push (cons fname (rb-tools--io-read-file file)) result))))
        (nreverse result)))))

(defun rb-tools--alist-to-keyword-plist (alist)
  "Return ALIST converted to a plist with keyword keys, preserving order."
  (let ((plist '()))
    (dolist (entry alist)
      (pcase-let ((`(,k . ,v) entry))
        (push (rb-tools--ensure-keyword k) plist)
        (push v plist)))
    (nreverse plist)))

(defun rb-tools-get-object (class id &optional properties)
  "Retrieve an object from the store.

CLASS and ID identify the object.  PROPERTIES, if provided, is an
array/list of property names to return; when omitted, all properties are
returned.  The result is a plist with keyword keys."
  (let ((alist (rb-tools--read-object-from-filesystem class id properties)))
    (rb-tools--alist-to-keyword-plist alist)))

(defun rb-tools-update-object (class id properties)
  "Update an existing object in the store.

CLASS (string), ID (string), and PROPERTIES (array of name/value pairs)
are combined via `rb-tools--object-from-args'.  Only the provided
properties are validated and updated; other properties remain unchanged.

Returns the full updated object as a plist."
  (let* ((schema (rb-tools-get-json-schema-for-class class))
         (base (rb-tools--object-store-base-dir class id)))
    (unless (file-directory-p base)
      (error "Object %s-%s does not exist" class id))
    ;; Read existing object
    (let* ((existing (rb-tools--read-object-from-filesystem class id nil))
           (existing-table (rb-tools--normalize-json-object existing))
           (updates (seq-into properties 'list)))
      ;; Apply updates
      (dolist (entry updates)
        (pcase-let* ((`(,name . ,value) (rb-tools--extract-property entry)))
          (unless (and (stringp name) (not (string-empty-p name)))
            (error "Property name must be a non-empty string"))
          (when (member name '("class" "id"))
            (error "Property %s is reserved" name))
          (puthash name value existing-table)))
      ;; Build updated alist preserving original keys order where possible
      (let ((updated '()))
        ;; Ensure class/id first, then other keys sorted for stability
        (push (cons "class" (gethash "class" existing-table)) updated)
        (push (cons "id" (gethash "id" existing-table)) updated)
        (let* ((keys (seq-filter (lambda (k) (not (member k '("class" "id"))))
                                 (hash-table-keys existing-table)))
               (sorted (sort keys #'string<)))
          (dolist (k sorted)
            (push (cons k (gethash k existing-table)) updated)))
        (setq updated (nreverse updated))
        ;; Validate only provided properties against schema
        (dolist (entry updates)
          (pcase-let* ((`(,name . ,value) (rb-tools--extract-property entry)))
            (let* ((definition (plist-get (plist-get schema :properties)
                                          (rb-tools--ensure-keyword name))))
              (unless definition
                (error "Unknown property for class %s: %s" class name))
              (rb-tools--validate-json-property name value definition))))
        ;; Write back
        (rb-tools--write-object-to-filesystem updated t)
        (rb-tools--alist-to-keyword-plist updated)))))

(defun rb-tools--list-object-ids (class)
  "Return a list of object IDs for CLASS from the store."
  (let* ((class-dir (expand-file-name
                     (format ".store/%s" (rb-tools--sanitize-path-component class))
                     (rb-tools--project-root)))
         (ids '()))
    (when (file-directory-p class-dir)
      (dolist (entry (directory-files class-dir nil "^[^.].*" t))
        (let ((full (expand-file-name entry class-dir)))
          (when (file-directory-p full)
            (push entry ids)))))
    (nreverse ids)))

(cl-defun rb-tools-find-objects (class &optional id properties)
  "Find objects of CLASS satisfying the search criteria in ID and PROPERTIES.

Returns only objects satisfying every provided search criterion.  ID,
when supplied, is treated as a regex matched against the object's id
property.  PROPERTIES, when supplied, is a list/array of name/value
pairs where VALUE is a regex matched against the property's file
content.

Returns a list of matching objects as plists (keyword keys)."
  (unless (or (stringp class) (symbolp class))
    (error "CLASS must be a string or symbol"))
  (when (and id (not (and (stringp id) (not (string-empty-p id)))))
    (error "ID regex must be a non-empty string when provided"))
  (let* ((class-str (if (symbolp class) (symbol-name class) class))
         (class-dir (expand-file-name
                     (format ".store/%s" (rb-tools--sanitize-path-component class-str))
                     (rb-tools--project-root))))
    (unless (file-directory-p class-dir)
      (cl-return-from rb-tools-find-objects '()))
    (let* ((filters (seq-into properties 'list))
           (candidates (rb-tools--list-object-ids class-str)))
      (unless candidates
        (cl-return-from rb-tools-find-objects '()))
      (when id
        (let* ((output (rb-tools-rg id class-dir "--glob id"))
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
          (cl-return-from rb-tools-find-objects '())))
      (while (and filters candidates)
        (pcase-let* ((`(,pname . ,regex) (rb-tools--extract-property (pop filters))))
          (unless (and (stringp pname) (not (string-empty-p pname)))
            (error "Property name must be a non-empty string"))
          (unless (and (stringp regex) (not (string-empty-p regex)))
            (error "Property regex must be a non-empty string"))
          (let* ((prop-file (rb-tools--sanitize-path-component pname))
                 (output (rb-tools-rg regex class-dir (format "--glob %s" prop-file)))
                 (match-set (make-hash-table :test #'equal)))
            (dolist (line (split-string output "\n" t))
              (when (string-match "^\\(.*\\):[0-9]+:" line)
                (let* ((matched-path (match-string 1 line))
                       (abs-path (expand-file-name matched-path class-dir))
                       (relative (file-relative-name abs-path class-dir))
                       (oid (car (split-string relative "/" t))))
                  (when oid (puthash oid t match-set)))))
            (setq candidates (seq-filter (lambda (oid) (gethash oid match-set)) candidates)))))
      (mapcar (lambda (oid) (rb-tools-get-object class-str oid)) candidates))))

;;; Tests -----------------------------------------------------------------
(require 'ert)

;; Minimal harness helpers for filesystem/buffer isolation ----------------
(defmacro rb-tools--with-temp-dir (dir-sym &rest body)
  "Bind DIR-SYM to a fresh temp directory and evaluate BODY.
The directory is cleaned up on exit."
  (declare (indent 1))
  `(let ((,dir-sym (make-temp-file "rb-tools" t)))
     (unwind-protect
         (progn ,@body)
       (when (and ,dir-sym (file-directory-p ,dir-sym))
         (delete-directory ,dir-sym t)))))

(defmacro rb-tools--with-temp-file (file-sym &rest body)
  "Bind FILE-SYM to a fresh temp file path inside a temp dir and eval BODY.
Cleans up the directory afterwards."
  (declare (indent 1))
  `(rb-tools--with-temp-dir dir
     (let ((,file-sym (expand-file-name "tmp" dir)))
       (write-region "" nil ,file-sym nil 'silent)
       ,@body)))

(defun rb-tools--line-number-of-substring (content substring)
  "Return the 1-based line number where SUBSTRING first occurs in CONTENT."
  (let ((pos (string-match (regexp-quote substring) content)))
    (unless pos
      (error "Substring %s not found in fixture content" substring))
    (1+ (cl-count ?\n (substring content 0 pos)))))

(defun rb-tools--multi-form-fixture ()
  "Return fixture metadata describing a file with multiple top-level forms."
  (let* ((defvar "(defvar alpha 1)")
         (nl "\n")
         (alpha-defun "(defun alpha ()\n  (message \"alpha\"))")
         (beta-defun "(defun beta ()\n  (message \"beta\"))")
         (content (concat ";;; fixture -*- mode: emacs-lisp -*-\n\n" defvar nl nl alpha-defun nl nl beta-defun nl)))
    (list :content content
          :alpha-line (rb-tools--line-number-of-substring content "(defun alpha")
          :beta-line (rb-tools--line-number-of-substring content "(defun beta")
          :beta-text beta-defun)))


;; Unit tests --------------------------------------------------------------

(ert-deftest rb-tools--normalize-json-object/plist-and-alist ()
  (let* ((plist '(:name "Story" story 42))
         (alist '((:name . "Story") (story . 42)))
         (hash (let ((h (make-hash-table :test #'equal)))
                 (puthash :name "Story" h)
                 (puthash 'story 42 h)
                 h)))
    (dolist (obj (list plist alist hash))
      (let ((tbl (rb-tools--normalize-json-object obj)))
        (should (equal (gethash "name" tbl) "Story"))
        (should (equal (gethash "story" tbl) 42))))))


(ert-deftest rb-tools--validate-json-object/accepts-valid-story ()
  (let* ((schema (rb-tools--get-json-schema-for-class 'STORY))
         (obj '((class . "STORY") (id . "STORY-8") (name . "Login") (description . "As a user..."))))
    (should (rb-tools--validate-json-object obj schema))))

(ert-deftest rb-tools--validate-json-object/rejects-missing-required-const ()
  (let* ((schema (rb-tools--get-json-schema-for-class 'STORY))
         (obj '((id . "STORY-8") (name . "Login"))))
    (should-error (rb-tools--validate-json-object obj schema))))

(ert-deftest rb-tools--validate-json-object/rejects-wrong-type ()
  (let* ((schema (rb-tools--get-json-schema-for-class 'TASK))
         (obj '((class . "TASK") (id . 2) (name . "Implement") (story . "not-integer"))))
    (should-error (rb-tools--validate-json-object obj schema))))

(ert-deftest rb-tools--validate-json-object/rejects-wrong-const ()
  (let* ((schema (rb-tools--get-json-schema-for-class 'TASK))
         (obj '((class . "STORY") (id . "STORY-3") (name . "Mismatch"))))
    (should-error (rb-tools--validate-json-object obj schema))))


(ert-deftest rb-tools--json-schema-property-required-p/const-and-required ()
  (let ((schema '( :type "object"
                   :properties ( :class (:const "STORY")
                                 :name (:type "string")
                                 :description (:type "string"))
                   :required (:name))))
    (should (rb-tools--json-schema-property-required-p :class schema))
    (should (rb-tools--json-schema-property-required-p :name schema))
    (should-not (rb-tools--json-schema-property-required-p :description schema))))


(ert-deftest rb-tools--ts-format-list-nodes/basic-preview ()
  (let* ((children (list (list :index 0 :kind "func" :line 5 :text "line1\nline2\nline3")
                         (list :index 1 :kind "var" :line 10 :text "only1"))))
    (let ((result (rb-tools--ts-format-list-nodes children 2)))
      (should (= 2 (length result)))
      (pcase-let ((`((:index 0 :kind "func" :line 5 :text_hash ,h1 :preview ,p1)
                     (:index 1 :kind "var" :line 10 :text_hash ,h2 :preview ,p2)) result))
        (should (string= h1 (secure-hash 'md5 "line1\nline2\nline3")))
        (should (string= h2 (secure-hash 'md5 "only1")))
        (should (string= p1 "line1\nline2"))
        (should (string= p2 "only1"))))))

(ert-deftest rb-tools--ts-format-list-nodes/defaults-to-1-line ()
  (let* ((children (list (list :index 0 :kind "func" :line 5 :text "a\nb"))))
    (dolist (preview '(nil 0 -1))
      (let* ((result (rb-tools--ts-format-list-nodes children preview))
             (first (car result)))
        (should (string= (plist-get first :preview) "a"))))))


(ert-deftest rb-tools--ts-select-nodes-by-line/picks-and-validates ()
  (let* ((children (list (list :index 0 :kind "func" :line 5 :text "fn")
                         (list :index 1 :kind "var" :line 10 :text "var"))))
    (let ((result (rb-tools--ts-select-nodes-by-line children '(10 5))))
      (pcase-let ((`((:index 1 :kind "var" :line 10 :text_hash ,h2 :text "var")
                     (:index 0 :kind "func" :line 5 :text_hash ,h1 :text "fn")) result))
        (should (string= h2 (secure-hash 'md5 "var")))
        (should (string= h1 (secure-hash 'md5 "fn"))))))
  ;; Missing node
  (should-error (rb-tools--ts-select-nodes-by-line '() '(1)))
  ;; Non-integer line
  (should-error (rb-tools--ts-select-nodes-by-line '() '("x"))))


(ert-deftest rb-tools-get-line-ranges/basic ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "l1\nl2\nl3\n")
    (let* ((ranges (list (list :start 3 :end 3)
                         (list :start 1 :end 2)))
           (result (rb-tools-get-line-ranges path ranges)))
      (should (equal (mapcar (lambda (r) (plist-get r :start)) result)
                     '(3 1)))
      (should (string= (plist-get (nth 0 result) :text) "l3\n"))
      (should (string= (plist-get (nth 1 result) :text) "l1\nl2\n"))
      (should (string= (plist-get (nth 0 result) :text_hash)
                       (secure-hash 'md5 "l3\n"))))))


(ert-deftest rb-tools-update-line-ranges/sorts-and-hash-ok ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "a\nb\nc\n")
    (let* ((hash-a (secure-hash 'md5 "a\n"))
           (hash-c (secure-hash 'md5 "c\n"))
           ;; Intentionally unsorted input; should be applied in descending order.
           (ranges (list (list :start 1 :end 1 :new_text "A\n" :text_hash hash-a)
                         (list :start 3 :end 3 :new_text "C\n" :text_hash hash-c))))
      (should (equal (rb-tools-update-line-ranges path ranges)
                     '((:start 3 :end 3 :updated t)
                       (:start 1 :end 1 :updated t))))
      (should (string= (rb-tools--io-read-file path) "A\nb\nC\n")))))

(ert-deftest rb-tools-update-line-ranges/hash-ok-single ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "x\ny\n")
    (let* ((hash-x (secure-hash 'md5 "x\n"))
           (ranges (list (list :start 1 :end 1 :new_text "X\n" :text_hash hash-x))))
      (should (equal (rb-tools-update-line-ranges path ranges)
                     '((:start 1 :end 1 :updated t))))
      (should (string= (rb-tools--io-read-file path) "X\ny\n")))))

(ert-deftest rb-tools-update-line-ranges/two-ranges-hash-ok ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "p\nq\n")
    (let* ((hash-p (secure-hash 'md5 "p\n"))
           (hash-q (secure-hash 'md5 "q\n"))
           (ranges (list (list :start 2 :end 2 :new_text "Q\n" :text_hash hash-q)
                         (list :start 1 :end 1 :new_text "P\n" :text_hash hash-p))))
      (should (equal (rb-tools-update-line-ranges path ranges)
                     '((:start 2 :end 2 :updated t)
                       (:start 1 :end 1 :updated t))))
      (should (string= (rb-tools--io-read-file path) "P\nQ\n")))))

(ert-deftest rb-tools-update-line-ranges/hash-mismatch ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "u\nv\n")
    (let* ((ranges (list (list :start 2 :end 2 :new_text "V\n" :text_hash "deadbeef"))))
      (should-error (rb-tools-update-line-ranges path ranges))
      (should (string= (rb-tools--io-read-file path) "u\nv\n")))))

(ert-deftest rb-tools-update-line-ranges/missing-verification-with-old-text ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "p\nq\n")
    (let* ((ranges (list (list :start 1 :end 1 :new_text "P\n" :old_text "p\n"))))
      (should-error (rb-tools-update-line-ranges path ranges))
      (should (string= (rb-tools--io-read-file path) "p\nq\n")))))

(ert-deftest rb-tools-update-line-ranges/missing-verification ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "m\nn\n")
    (let* ((ranges (list (list :start 1 :end 1 :new_text "M\n"))))
      (should-error (rb-tools-update-line-ranges path ranges))
      (should (string= (rb-tools--io-read-file path) "m\nn\n")))))

(ert-deftest rb-tools-update-line-ranges/overlap-and-bounds ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "a\nb\nc\n")
    (let* ((hash-c (secure-hash 'md5 "c\n"))
           (hash-bc (secure-hash 'md5 "b\nc\n")))
      ;; Overlapping ranges rejected
      (should-error (rb-tools-update-line-ranges path (list (list :start 3 :end 3 :new_text "Z\n" :text_hash hash-c)
                                                            (list :start 2 :end 3 :new_text "Y\n" :text_hash hash-bc))))
      ;; Out-of-bounds rejected
      (should-error (rb-tools-update-line-ranges path (list (list :start 5 :end 5 :new_text "oops" :text_hash "dummy"))))
      (should (string= (rb-tools--io-read-file path) "a\nb\nc\n")))))

(ert-deftest rb-tools-update-line-ranges/all-or-nothing ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "one\ntwo\nthree\n")
    (let* ((hash-two (secure-hash 'md5 "two\n"))
           (ranges (list (list :start 2 :end 2 :new_text "TWO\n" :text_hash hash-two)
                         (list :start 1 :end 1 :new_text "ONE\n" :text_hash "bad"))))
      (should-error (rb-tools-update-line-ranges path ranges))
      (should (string= (rb-tools--io-read-file path) "one\ntwo\nthree\n")))))

(ert-deftest rb-tools-update-line-ranges/empty-new-text ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "keep\nremove\n")
    (let* ((hash-remove (secure-hash 'md5 "remove\n"))
           (ranges (list (list :start 2 :end 2 :new_text "" :text_hash hash-remove))))
      (should (equal (rb-tools-update-line-ranges path ranges)
                     '((:start 2 :end 2 :updated t))))
      (should (string= (rb-tools--io-read-file path) "keep\n")))))

(ert-deftest rb-tools-update-line-ranges/multi-range-hash-only ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path "l1\nl2\nl3\n")
    (let* ((hash-l3 (secure-hash 'md5 "l3\n"))
           (hash-l1 (secure-hash 'md5 "l1\n"))
           (ranges (list (list :start 3 :end 3 :new_text "L3\n" :text_hash hash-l3)
                         (list :start 1 :end 1 :new_text "L1\n" :text_hash hash-l1))))
      (should (equal (rb-tools-update-line-ranges path ranges)
                     '((:start 3 :end 3 :updated t)
                       (:start 1 :end 1 :updated t))))
      (should (string= (rb-tools--io-read-file path) "L1\nl2\nL3\n")))))


(ert-deftest rb-tools-ts-insert-before-node/basic ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let* ((original ";;; -*- mode: emacs-lisp; -*-\n\n(defun foo ()\n  (message \"hi\"))\n")
           (selector (list :line 3))
           (new-text ";; inserted\n")
           (expected ";;; -*- mode: emacs-lisp; -*-\n\n;; inserted\n(defun foo ()\n  (message \"hi\"))\n"))
      (rb-tools--io-write-file path original)
      (let* ((result (rb-tools-ts-insert-before-node path selector new-text))
             (updated (rb-tools--io-read-file path)))
        (should (eq t (plist-get result :inserted)))
        (should (equal updated expected))))))

(ert-deftest rb-tools-ts-insert-before-node/skip-format ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path ";;; -*- mode: emacs-lisp; -*-\n\n(defun foo ()\n  (message \"hi\"))\n")
    (let* (formatted
           (rb-tools-ts-format-function (lambda (_s _e) (setq formatted t))))
      (rb-tools-ts-insert-before-node path (list :line 1) ";; fmt\n")
      (should formatted))
    (rb-tools--io-write-file path ";;; -*- mode: emacs-lisp; -*-\n\n(defun foo ()\n  (message \"hi\"))\n")
    (let* ((formatted nil)
           (rb-tools-ts-format-function (lambda (_s _e) (setq formatted t))))
      (rb-tools-ts-insert-before-node path (list :line 1) ";; nofmt\n" t)
      (should-not formatted))))

(ert-deftest rb-tools-ts-insert-before-node/rejects-ambiguous-hash ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let* ((node-text "(defvar x 1)\n")
           (content (concat node-text node-text))
           (hash (secure-hash 'md5 node-text)))
      (rb-tools--io-write-file path content)
      (should-error (rb-tools-ts-insert-before-node path (list :text_hash hash) ";; fail\n")))))

(ert-deftest rb-tools-ts-insert-before-node/line-and-hash-selectors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let* ((fixture (rb-tools--multi-form-fixture))
           (before-text ";; before alpha\n")
           (after-text ";; after beta\n")
           (beta-hash (secure-hash 'md5 (plist-get fixture :beta-text))))
      (rb-tools--io-write-file path (plist-get fixture :content))
      (rb-tools-ts-insert-before-node path
                                      (list :line (plist-get fixture :alpha-line))
                                      before-text)
      (rb-tools-ts-insert-after-node path
                                     (list :text_hash beta-hash)
                                     after-text)
      (let ((expected (with-temp-buffer
                        (insert (plist-get fixture :content))
                        (goto-char (point-min))
                        (forward-line (1- (plist-get fixture :alpha-line)))
                        (insert before-text)
                        (goto-char (point-max))
                        (insert after-text)
                        (buffer-string))))
        (should (string= (rb-tools--io-read-file path) expected))))))

(ert-deftest rb-tools-ts-insert-before-node/fixture-parse-error-preserves-content ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let ((fixture (rb-tools--multi-form-fixture)))
      (rb-tools--io-write-file path (plist-get fixture :content))
      (should-error (rb-tools-ts-insert-before-node path
                                                    (list :line (plist-get fixture :alpha-line))
                                                    "("))
      (should (string= (rb-tools--io-read-file path) (plist-get fixture :content))))))


(ert-deftest rb-tools-ts-insert-after-node/rejects-parse-errors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let* ((original "(defun foo ()\n  (message \"hi\"))\n"))
      (rb-tools--io-write-file path original)
      (should-error (rb-tools-ts-insert-after-node path (list :line 1) "("))
      (should (string= original (rb-tools--io-read-file path))))))

(ert-deftest rb-tools-ts-insert-after-node/missing-selector ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let ((fixture (rb-tools--multi-form-fixture)))
      (rb-tools--io-write-file path (plist-get fixture :content))
      (should-error (rb-tools-ts-insert-after-node path (list :line 999)
                                                   ";; missing\n"))
      (should (string= (rb-tools--io-read-file path) (plist-get fixture :content))))))


(ert-deftest rb-tools-insert-before-regex/basic ()
  (rb-tools--with-temp-file path
    (let* ((fixture ";;; fixture\n\n(defvar foo 1)\n(defvar bar 2)\n")
           (expected ";;; fixture\n\n;; inserted\n(defvar foo 1)\n(defvar bar 2)\n"))
      (rb-tools--io-write-file path fixture)
      (rb-tools-insert-before-regex path "\\(defvar foo" ";; inserted\n")
      (should (string= (rb-tools--io-read-file path) expected)))))

(ert-deftest rb-tools-insert-before-regex/basic-2 ()
  "Inserting before a unique marker should succeed."
  (rb-tools--with-temp-file path
    (let* ((fixture ";;; fixture\nmarker\nline\n")
           (expected ";;; fixture\n;; before marker\nmarker\nline\n"))
      (rb-tools--io-write-file path fixture)
      (rb-tools-insert-before-regex path "marker" ";; before marker\n")
      (should (string= (rb-tools--io-read-file path) expected)))))

(ert-deftest rb-tools-insert-before-regex/anchor-line ()
  "Using an anchor line selects the closest match for inserting before."
  (rb-tools--with-temp-file path
    (let* ((fixture "alpha\nmarker\nbeta\nmarker\ngamma\n")
           (expected "alpha\nmarker\nbeta\n;; inserted near second marker\nmarker\ngamma\n"))
      (rb-tools--io-write-file path fixture)
      (rb-tools-insert-before-regex path "marker" ";; inserted near second marker\n" 5)
      (should (string= (rb-tools--io-read-file path) expected)))))

(ert-deftest rb-tools-insert-before-regex/missing-match ()
  (rb-tools--with-temp-file path
    (rb-tools--io-write-file path ";; nothing\n")
    (should-error (rb-tools-insert-before-regex path "absent" ";; missing\n"))
    (should (string= (rb-tools--io-read-file path) ";; nothing\n"))))

(ert-deftest rb-tools-insert-before-regex/missing-match ()
  "Missing regex matches must error and leave the file untouched."
  (rb-tools--with-temp-file path
    (let ((fixture "alpha\nbeta\n"))
      (rb-tools--io-write-file path fixture)
      (should-error (rb-tools-insert-before-regex path "missing" ";; inserted\n"))
      (should (string= (rb-tools--io-read-file path) fixture)))))


(ert-deftest rb-tools-insert-after-regex/basic ()
  "Inserting after a unique marker should succeed."
  (rb-tools--with-temp-file path
    (let* ((fixture "marker\nline\n")
           (expected "marker\n;; after marker\nline\n"))
      (rb-tools--io-write-file path fixture)
      (rb-tools-insert-after-regex path "marker" ";; after marker\n")
      (should (string= (rb-tools--io-read-file path) expected)))))

(ert-deftest rb-tools-insert-after-regex/anchor-line ()
  (rb-tools--with-temp-file path
    (let* ((fixture ";;; fixture\nmarker\nalpha\nmarker\nomega\n")
           (expected ";;; fixture\nmarker\nalpha\nmarker\n;; after second\nomega\n"))
      (rb-tools--io-write-file path fixture)
      (rb-tools-insert-after-regex path "marker" ";; after second\n" 4)
      (should (string= (rb-tools--io-read-file path) expected)))))

(ert-deftest rb-tools-insert-after-regex/anchor-line-2 ()
  "Using an anchor line selects the closest match for inserting after."
  (rb-tools--with-temp-file path
    (let* ((fixture "alpha\nmarker\nbeta\nmarker\ngamma\n")
           (expected "alpha\nmarker\nbeta\nmarker\n;; after second marker\ngamma\n"))
      (rb-tools--io-write-file path fixture)
      (rb-tools-insert-after-regex path "marker" ";; after second marker\n" 5)
      (should (string= (rb-tools--io-read-file path) expected)))))

(ert-deftest rb-tools-insert-after-regex/ambiguous-match ()
  (rb-tools--with-temp-file path
    (let ((fixture "marker\nmarker\n"))
      (rb-tools--io-write-file path fixture)
      (should-error (rb-tools-insert-after-regex path "marker" ";; fail\n"))
      (should (string= (rb-tools--io-read-file path) fixture)))))

(ert-deftest rb-tools-insert-after-regex/validate-detection ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let ((fixture "(defun foo ()\n  (message \"hi\"))\n"))
      (rb-tools--io-write-file path fixture)
      (should-error (rb-tools-insert-after-regex path "(defun foo" "(\n" nil t))
      (should (string= (rb-tools--io-read-file path) fixture)))))

(ert-deftest rb-tools-insert-after-regex/rust-regex-syntax ()
  "Rust-flavored regexes such as inline flags should work through ripgrep."
  (rb-tools--with-temp-file path
    (let* ((fixture "hello\nHELLO\n")
           (expected "hello\nHELLO\n;; inserted after uppercase\n"))
      (rb-tools--io-write-file path fixture)
      (rb-tools-insert-after-regex path "(?i)hello" ";; inserted after uppercase\n" 2)
      (should (string= (rb-tools--io-read-file path) expected)))))

(ert-deftest rb-tools-test-insert-after-regex/ambiguous-match ()
  "Ambiguous regex matches must error and leave the file untouched."
  (rb-tools--with-temp-file path
    (let ((fixture "marker\nmarker\n"))
      (rb-tools--io-write-file path fixture)
      (should-error (rb-tools-insert-after-regex path "marker" ";; inserted\n"))
      (should (string= (rb-tools--io-read-file path) fixture)))))

(ert-deftest rb-tools-test-insert-after-regex/validate-failure ()
  "Validation prevents writing corrupted files when :validate is true."
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools--with-temp-file path
    (let ((fixture "(defun foo ()\n  (message \"hi\"))\n"))
      (rb-tools--io-write-file path fixture)
      (should-error (rb-tools-insert-after-regex path "(defun foo" "(\n" nil t))
      (should (string= (rb-tools--io-read-file path) fixture)))))


;;; Tools -----------------------------------------------------------------

(gptel-make-tool
 :name "clojure_eval"
 :category "rb"
 :description (documentation 'rb-tools-clojure-eval)
 :function #'rb-tools-clojure-eval
 :args
 '(( :name "input"
     :type string
     :description "The form to evaluate in the context of the connected Clojure/ClojureScript app.")
   ( :name "ns"
     :type string
     :description "The Clojure/ClojureScript namespace in which the form should be evaluated."))
 :confirm t
 :include t)

(gptel-make-tool
 :name "elisp_eval"
 :category "rb"
 :description (documentation 'rb-tools-elisp-eval)
 :function #'rb-tools-elisp-eval
 :args
 '(( :name "input"
     :type string
     :description "The Emacs Lisp expression to evaluate inside the connected Emacs instance."))
 :confirm t
 :include t)

(gptel-make-tool
 :name "bash_eval"
 :category "rb"
 :description (documentation 'rb-tools-bash-eval)
 :function #'rb-tools-bash-eval
 :args
 '(( :name "script"
     :type string
     :description "The Bash script to evaluate via `bash -c`."))
 :confirm t
 :include t)

(gptel-make-tool
 :name "tree_sitter_list_nodes"
 :category "rb"
 :description (documentation 'rb-tools-ts-list-nodes)
 :function #'rb-tools-ts-list-nodes
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "preview_lines"
     :type integer
     :description "Number of lines to include in the preview (defaults to 1)."
     :optional t))
 :include t)

(gptel-make-tool
 :name "tree_sitter_get_nodes"
 :category "rb"
 :description (documentation 'rb-tools-ts-get-nodes)
 :function #'rb-tools-ts-get-nodes
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "line_numbers"
     :type array
     :items (:type integer)
     :description "List of line numbers referencing the start of each target node.")
   )
 :include t)

(gptel-make-tool
 :name "tree_sitter_update_nodes"
 :category "rb"
 :description (documentation 'rb-tools-ts-update-nodes)
 :function #'rb-tools-ts-update-nodes
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "nodes"
     :type array
     :items ( :type object
              :properties ( :index (:type integer)
                            :kind (:type string)
                            :line (:type integer)
                            :text_hash (:type string)
                            :new_text (:type string)))
     :description "List of nodes to update.")
   ( :name "skip_format"
     :type boolean
     :description "If true, do not format/reindent the new node."
     :optional t)
   ( :name "dry_run"
     :type boolean
     :description "If true, do not modify the file, just report what would be done."
     :optional t))
 :confirm t
 :include t)

(gptel-make-tool
 :name "tree_sitter_insert_before_node"
 :category "rb"
 :description (documentation 'rb-tools-ts-insert-before-node)
 :function #'rb-tools-ts-insert-before-node
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "selector"
     :type object
     :properties ( :line (:type integer :description "Starting line of the node." :optional t)
                   :text_hash (:type string :description "MD5 hash of the node text." :optional t))
     :description "Node selector by :line and/or :text_hash.")
   ( :name "new_text"
     :type string
     :description "Text to insert before the node.")
   ( :name "skip_format"
     :type boolean
     :description "If true, do not format/reindent the inserted text."
     :optional t))
 :confirm t)

(gptel-make-tool
 :name "tree_sitter_insert_after_node"
 :category "rb"
 :description (documentation 'rb-tools-ts-insert-after-node)
 :function #'rb-tools-ts-insert-after-node
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "selector"
     :type object
     :properties ( :line (:type integer :description "Starting line of the node." :optional t)
                   :text_hash (:type string :description "MD5 hash of the node text." :optional t))
     :description "Node selector by :line and/or :text_hash.")
   ( :name "new_text"
     :type string
     :description "Text to insert after the node.")
   ( :name "skip_format"
     :type boolean
     :description "If true, do not format/reindent the inserted text."
     :optional t))
 :confirm t)

(gptel-make-tool
 :name "insert_before_regex"
 :category "rb"
 :description (documentation 'rb-tools-insert-before-regex)
 :function #'rb-tools-insert-before-regex
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "regex"
     :type string
     :description "Regex marker to search for using ripgrep.")
   ( :name "new_text"
     :type string
     :description "Text to insert before the matching line.")
   ( :name "line"
     :type integer
     :description "Anchor line when multiple matches exist."
     :optional t)
   ( :name "validate"
     :type boolean
     :description "When true, re-parse the buffer with Tree-sitter before writing."
     :optional t))
 :confirm t)

(gptel-make-tool
 :name "insert_after_regex"
 :category "rb"
 :description (documentation 'rb-tools-insert-after-regex)
 :function #'rb-tools-insert-after-regex
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "regex"
     :type string
     :description "Regex marker to search for using ripgrep.")
   ( :name "new_text"
     :type string
     :description "Text to insert after the matching line.")
   ( :name "line"
     :type integer
     :description "Anchor line when multiple matches exist."
     :optional t)
   ( :name "validate"
     :type boolean
     :description "When true, re-parse the buffer with Tree-sitter before writing."
     :optional t))
 :confirm t)

(gptel-make-tool
 :name "get_line_ranges"
 :category "rb"
 :description (documentation 'rb-tools-get-line-ranges)
 :function #'rb-tools-get-line-ranges
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "ranges"
     :type array
     :items ( :type object
              :properties ( :start (:type integer)
                            :end (:type integer)))
     :description "List of line ranges to retrieve (1-based, inclusive)."))
 :include t)

(gptel-make-tool
 :name "update_line_ranges"
 :category "rb"
 :description (documentation 'rb-tools-update-line-ranges)
 :function #'rb-tools-update-line-ranges
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "ranges"
     :type array
     :items ( :type object
              :properties ( :start (:type integer)
                            :end (:type integer)
                            :text_hash (:type string :description "MD5 hash of current text block between :start and :end.")
                            :new_text (:type string :description "Replacement text.")))
     :description "List of line ranges to replace with verification."))
 :confirm t)

(gptel-make-tool
 :name "read_file"
 :category "rb"
 :description (documentation 'rb-tools-read-file)
 :function #'rb-tools-read-file
 :args
 '(( :name "path"
     :type string
     :description "Path of the file to read.")
   ( :name "with_line_numbers"
     :type boolean
     :description "If true, include line numbers."
     :optional t))
 :confirm t
 :include t)

(gptel-make-tool
 :name "write_file"
 :category "rb"
 :description (documentation 'rb-tools-write-file)
 :function #'rb-tools-write-file
 :args
 '(( :name "path"
     :type string
     :description "Path of the file to write.")
   ( :name "content"
     :type string
     :description "New content of the file."))
 :confirm t)

(gptel-make-tool
 :name "rg"
 :category "rb"
 :description "Run ripgrep in a directory and return matches."
 :function #'rb-tools-rg
 :args
 '(( :name "pattern"
     :type string
     :description "Regex pattern to search for.")
   ( :name "directory"
     :type string
     :description "Directory to search (defaults to current directory)."
     :optional t)
   ( :name "extra_args"
     :type string
     :description "Additional args to pass to rg, e.g. \"-g *.el --hidden\"."
     :optional t))
 :include t)

(gptel-make-tool
 :name "fd"
 :category "rb"
 :description "Run fd in a directory and return matches."
 :function #'rb-tools-fd
 :args
 '(( :name "query"
     :type string
     :description "Pattern to search for.")
   ( :name "directory"
     :type string
     :description "Directory to search (defaults to current directory).")
   ( :name "extra_args"
     :type string
     :description "Additional args to pass to fd, e.g. \"--type f --max-depth 3\"."))
 :include t)

(gptel-make-tool
 :name "get_json_schema_for_class"
 :category "rb"
 :description (documentation 'rb-tools-get-json-schema-for-class)
 :function #'rb-tools-get-json-schema-for-class
 :args
 '(( :name "class"
     :type string
     :description "Name of the class, an uppercase string."))
 :include t)

(gptel-make-tool
 :name "insert_object"
 :category "rb"
 :description (documentation 'rb-tools-insert-object)
 :function #'rb-tools-insert-object
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Object identifier, unique within a class.")
         ( :name "properties"
           :type array
           :items ( :type object
                    :properties ( :name (:type string)
                                  :value (:type string)))
           :description "Additional properties as name/value pairs."
           :optional t))
 :include t)

(gptel-make-tool
 :name "get_object"
 :category "rb"
 :description (documentation 'rb-tools-get-object)
 :function #'rb-tools-get-object
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Object identifier, unique within a class.")
         ( :name "properties"
           :type array
           :items (:type string)
           :description "Optional list of property names to return; if omitted, all properties are returned."
           :optional t))
 :include t)

(gptel-make-tool
 :name "update_object"
 :category "rb"
 :description (documentation 'rb-tools-update-object)
 :function #'rb-tools-update-object
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Object identifier, unique within a class.")
         ( :name "properties"
           :type array
           :items ( :type object
                    :properties ( :name (:type string)
                                  :value (:type string)))
           :description "Properties to update as name/value pairs."))
 :include t)

(gptel-make-tool
 :name "find_objects"
 :category "rb"
 :description (documentation 'rb-tools-find-objects)
 :function #'rb-tools-find-objects
 :args '(( :name "class"
           :type string
           :description "Object class, uppercase string.")
         ( :name "id"
           :type string
           :description "Optional object identifier regex to restrict search."
           :optional t)
         ( :name "properties"
           :type array
           :items ( :type object
                    :properties ( :name (:type string)
                                  :value (:type string)))
           :description "List of property name/regex pairs to match content."
           :optional t))
 :include t)

;;; Presets ---------------------------------------------------------------

(defun rb-tools--all-active-major-modes ()
  "Return a list of all major modes currently used by any live buffer."
  (let (modes)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (push major-mode modes)))
    (delete-dups modes)))

(defun rb-tools--preset-tools-dev ()
  "Build tools for the @dev preset."
  (let ((tools '("elisp_eval" "bash_eval"))
        (modes (rb-tools--all-active-major-modes)))
    (when (memq 'clojure-mode modes)
      (push "clojure_eval" tools))
    tools))

(gptel-make-preset 'dev
  :description "Development preset - only used as a base for others."
  :system "You play the role of an agent who helps me develop software.

You are inside a project folder and we are conversing via gptel in Emacs.

_Some notes regarding the nature of our collaboration_

I am a dreamer by nature and I easily get consumed by exciting
ideas. You should be like a father figure who provides the necessary
contrast to this dreamer, who curbs the exuberance if necessary, such
that the project under development has a better chance to succeed.

_General instructions_

1. If something is not clear, ask for clarification.

2. Never touch the =project.org= file, that is used to store our
   conversations.

3. The tools which you are using have been mostly developed by me. They
   may be buggy. If a tool does work as advertised, stop immediately and
   report the problem so that I have a chance to fix it.

_Development guidelines_

1. Follow the philosophy of `functional core, imperative shell'

2. When it is feasible, write tests first.

"
  :tools '(:eval (rb-tools--preset-tools-dev))
  :temperature 0.1
  :prompt-transform-functions '(:prepend (rb-gptel-mark-user-response-regions)))

(gptel-make-preset 'repo-reader
  :description "Grants read access to all files in the Git repository."
  :tools '(:append ("rg" "fd" "read_file" "get_line_ranges"))
  :system '(:append "To explore the repository, use the following tools:

- =fd= for getting a recursive listing of all files in the project
- =rg= for finding files with lines matching a certain regex
- =read_file= to slurp an entire file into context
- =get_line_ranges= to load parts of a file into context

"))

(gptel-make-preset 'repo-editor
  :description "Grants write access to all files in the Git repository."
  :parents '(repo-reader)
  :tools '(:append ("write_file" "update_line_ranges"))
  :system '(:append "To update a range of lines in a text file:

1. If it is a source code file, use the tree_sitter tools.

2. If it is a plain text file, use =get_line_ranges= to fetch the
content and text hash for the ranges you want to update, then call
=update_line_ranges= with the hashes and the replacements.

"))

(gptel-make-preset 'store-reader
  :description "Grants read access to the object store."
  :tools '(:append ("get_json_schema_for_class" "get_object" "find_objects"))
  :system '(:append "You have access to an object store where we store objects of classes
like STORY or TASK.

Currently the following classes are available:

- STORY: user stories
- TASK: implementation tasks

Before you do anything with objects of a certain class, invoke
=get_json_schema_for_class= to get a description of the class and the
JSON schema for its objects.

"))

(gptel-make-preset 'store-editor
  :description "Grants write access to the object store."
  :parents '(store-reader)
  :tools '(:append ("insert_object" "update_object"))
  :system '(:append "Occasionally I will ask you to create an object of some class.

When this happens, I may or may not explicitly state the object id you
should use. If I do not supply you with an explicit object id, use the
=find_objects= tool to list all objects of the class, determine the
highest id, increment by one and add a slug which feels adequate.

"))

(gptel-make-preset 'code-reader
  :description "Grants read access to source code files."
  :tools '(:append ("tree_sitter_list_nodes" "tree_sitter_get_nodes" "get_line_ranges" "read_file"))
  :system '(:append "_Guidelines on analyzing source code files_

*If you want to analyze a source code file, use the following tools:*

1. tree_sitter_list_nodes: Gives you the name, type signature and
   starting line number of all function and variable definitions in the
   file.

2. tree_sitter_get_nodes: If you have starting line numbers, this tool
   translates them to the complete text of the AST nodes which start on
   those lines

3. get_line_ranges: Loads line ranges of a file into the context.

4. read_file: Slurps the whole file into the context, with optional line
   numbers. Use this as a last resort as it may consume a lot of
   context.

"))

(gptel-make-preset 'code-editor
  :description "Grants write access to source code files."
  :parents '(code-reader)
  :tools '(:append ("tree_sitter_update_nodes"
                    "tree_sitter_insert_before_node"
                    "tree_sitter_insert_after_node"
                    "write_file"))
  :system '(:append "_Guidelines on changing source code files_

*If you want to change something in a source code file:*

1. Use =tree_sitter_list_nodes= and =tree_sitter_get_nodes= to get the
   current state.

2a. Use =tree_sitter_update_nodes= to update or remove desired AST nodes.

2b. Use =tree_sitter_insert_before_node= or
    =tree_sitter_insert_after_node= to insert new AST nodes.

"))

(gptel-make-preset 'plan
  :description "Used to plan stories and write tasks."
  :parents '(dev repo-editor code-reader store-editor)
  :system '(:append "Your job is to help me plan a STORY and write its TASKS.

_Description of the planning workflow_

We will enter into a dialogue about a new feature or idea I have in
mind. Your task is to help me flesh out the details. When I feel that we
are ready, I will ask you to write a STORY and break it down to TASKs.

The STORY and TASKs you create will be used for implementation so they
should be sharp and crystal clear. If you feel that you do not have
enough information to guarantee this, stop and ask for clarification. We
shall continue this feedback loop until you are satisfied.

In the current stage, writing the code is out of scope: your only job is
to prepare the story and its tasks. Implementation will be done by a
coding agent in a later stage.

The planning workflow has the following outcomes:

- STORY created, description and implementation_plan fleshed out
- TASKS created, linked to parent STORY, descriptions filled

TASKS break down the implementation plan into individual steps. Each
task should be implementable by an independent agent who has access to
both the TASK and its STORY.

All planning must be done upfront. There are no planning TASKS.

"))

(gptel-make-preset 'code
  :description "Used to implement tasks."
  :parents '(dev repo-editor code-editor store-editor)
  :system '(:append "Your job is to implement a TASK.

_Description of the implementation workflow_

1. I provide you with the id of the TASK which should be implemented.

2. You retrieve the TASK and the corresponding STORY from the object
   store, analyze them, understand what to do.  If something is not
   clear, you stop and ask for clarification.

3. You implement the TASK.

4. You populate the =review= field of the TASK object with information
   about the result: what went well, what had to be changed, what we
   learnt, what reality taught us.

"))

(gptel-make-preset 'review
  :description "Used to review stories."
  :parents '(dev repo-editor code-reader store-editor)
  :system '(:append "Your job is to review a story based on the individual reviews of its tasks.

_Description of the review workflow_

1. I give you a STORY id.
2. You collect all TASKS of the STORY, extract their =review= fields.
3. Analyze the individual reviews and write a summary into the =review= field of the STORY.

"))

;;; rb-tools.el ends here
