;;; rb-tools.el --- RB's helper tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for RB's GPT/Tree-sitter and tooling setup.

;;; Code:

(require 'cl-generic)
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

(defvar rb-tools-major-mode-to-ts-lang-alist
  '((emacs-lisp-mode . elisp)
    (go-mode . go)))

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
              (funcall fn))))
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

(defun rb-tools-ts-list-nodes (path &optional preview-lines)
  "Parse PATH using Tree-sitter and return the list of top-level AST nodes.

Each AST node has the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- preview: string - first PREVIEW-LINES lines of node text

PREVIEW-LINES controls how many lines are included in each preview (default: 1)."
  (unless (and (integerp preview-lines) (> preview-lines 0))
    (setq preview-lines 1))
  (rb-tools--ts-with-root
   path nil
   (lambda ()
     (let* ((root (treesit-buffer-root-node))
            (child-count (treesit-node-child-count root t))
            (result '()))
       (dotimes (i child-count)
         (let* ((node (treesit-node-child root i t))
                (text (treesit-node-text node))
                (line (line-number-at-pos (treesit-node-start node)))
                (lines (split-string text "\n"))
                (line-count (length lines))
                (preview-count (min preview-lines line-count))
                (preview-lines-list (seq-take lines preview-count))
                (preview (string-join preview-lines-list "\n")))
           (push (list :index i
                       :kind (treesit-node-type node)
                       :line line
                       :text_hash (secure-hash 'md5 text)
                       :preview preview)
                 result)))
       (nreverse result)))))

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
 :confirm t
 :include t)

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
   (lambda ()
     (let* ((root (treesit-buffer-root-node))
            (child-count (treesit-node-child-count root t))
            (lines (seq-into line-numbers 'list))
            (result '()))
       (dolist (line lines)
         (unless (and (integerp line) (>= line 1))
           (error "Invalid line number: %s" line))
         (let ((entry
                (catch 'found
                  (dotimes (idx child-count)
                    (let* ((node (treesit-node-child root idx t))
                           (node-line (line-number-at-pos (treesit-node-start node))))
                      (when (= node-line line)
                        (let ((text (treesit-node-text node)))
                          (throw 'found
                                 (list :index idx
                                       :kind (treesit-node-type node)
                                       :line node-line
                                       :text_hash (secure-hash 'md5 text)
                                       :text text)))))))))
           (unless entry
             (error "No node starts on line %s" line))
           (push entry result)))
       (nreverse result)))))

;; (rb-tools-ts-list-nodes "/home/rb/projects/dotfiles/.emacs.d/rb-tools.el")

;; (rb-tools-ts-list-nodes "/home/rb/projects/mixtape/box.go")

;; (rb-tools-ts-get-nodes "/home/rb/projects/mixtape/box.go" '(12))

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
 :confirm t
 :include t)

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
   (lambda ()
     (let* ((updates (sort (seq-into nodes 'list)
                           (lambda (a b)
                             (> (plist-get a :line)
                                (plist-get b :line)))))
            (changes '()))
       (dolist (spec updates)
         (unless (stringp (plist-get spec :new_text))
           (error "Missing new_text for node %s" (plist-get spec :index)))
         (let* ((validated (rb-tools--ts-validate-node (treesit-buffer-root-node) spec))
                (node (nth 0 validated))
                (old-text (treesit-node-text node))
                (new-text (plist-get spec :new_text)))
           (let ((start (treesit-node-start node))
                 (end (treesit-node-end node)))
             (goto-char start)
             (delete-region start end)
             (insert new-text)
             (unless (rb-tools--truthy? skip-format)
               (let ((new-end (point)))
                 (indent-region start new-end))))
           (push (list :index (plist-get spec :index)
                       :kind (plist-get spec :kind)
                       :line (plist-get spec :line)
                       :old_text_hash (secure-hash 'md5 old-text)
                       :new_text_hash (secure-hash 'md5 new-text)
                       :old_text old-text
                       :new_text new-text)
                 changes)))
       (let* ((root2 (treesit-buffer-root-node)))
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
               (write-region (point-min) (point-max) path nil 'silent)
               (nreverse result)))))))))

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

(defun rb-tools-replace-line-ranges (path ranges)
  "Replace the specified line RANGES in PATH with new content.

Each element of RANGES contains the following fields:

- start: integer - line number of range start
- end: integer - line number of range end
- end_inclusive: boolean - is line at END included in the range?
- new_text: string - replacement text"
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (unless (file-writable-p path)
    (error "File is not writable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (let ((updates (sort (seq-into ranges 'list)
                         (lambda (a b)
                           (> (plist-get a :start)
                              (plist-get b :start))))))
      (dolist (spec updates)
        (let* ((total-lines (line-number-at-pos (point-max)))
               (start (plist-get spec :start))
               (end (plist-get spec :end))
               (end-inc (rb-tools--truthy? (plist-get spec :end_inclusive)))
               (new-text (plist-get spec :new_text)))
          (unless (stringp new-text)
            (error "Missing new_text for range starting at %s" start))
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
          (let (start-pos end-pos)
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- start))
              (setq start-pos (point)))
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- end))
              (when end-inc
                (forward-line 1))
              (setq end-pos (point)))
            (goto-char start-pos)
            (delete-region start-pos end-pos)
            (insert new-text)))))
    (write-region (point-min) (point-max) path nil 'silent)))

(gptel-make-tool
 :name "replace_line_ranges"
 :category "rb"
 :description (documentation 'rb-tools-replace-line-ranges)
 :function #'rb-tools-replace-line-ranges
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file.")
   ( :name "ranges"
     :type array
     :items ( :type object
              :properties ( :start (:type integer)
                            :end (:type integer)
                            :end_inclusive (:type boolean)
                            :new_text (:type string)))
     :description "List of line ranges to replace."))
 :confirm t)

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

(defun rb-tools-write-file (path content)
  "Write CONTENT to the file at PATH."
  (unless (file-writable-p path)
    (error "File is not writable: %s" path))
  (with-temp-file path
    (insert content)))

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
 :confirm t
 :include t)

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
 :confirm t
 :include t)

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

Example STORY identifiers (SIDs): STORY-1, STORY-3, STORY-124.

One iteration of the development cycle typically consists of the following steps:

1. Create user story
2. Create tasks for the user story
3. Implement individual tasks of the user story"
     :properties ( :name ( :type "string"
                           :description "Short name of the user story, serves as a one-line summary")
                   :description ( :type "string"
                                  :description "Long description of the user story"))))

(cl-defmethod rb-tools--get-json-schema-for-class ((_class (eql 'TASK)))
  "Return the JSON schema for object store class `TASK'."
  '( :type "object"
     :description "Objects of the TASK class describe development tasks.

TASKS may be associated with STORIES or may stand alone.

Example TASK identifiers (TIDs): TASK-1-1-first, TASK-1-2-second, TASK-124-5-wrap-up

TASK-124-5-wrap-up means the fifth task of STORY-124 with the name `wrap-up'

"
     :properties ( :name ( :type "string"
                           :description "Short name of the task, serves as a one-line summary")
                   :description ( :type "string"
                                  :description "Long description of the task.

Fleshes out all the details necessary for successful implementation.")
                   :story ( :type "string"
                            :description "Link to the parent story which owns this task."))))

(defun rb-tools-get-json-schema-for-class (class)
  "Return the JSON schema for object store class CLASS."
  (when (stringp class)
    (setq class (intern class)))
  (unless (symbolp class)
    (error "CLASS must be string or symbol, got %s" class))
  (rb-tools--get-json-schema-for-class class))

(gptel-make-tool
 :name "get_json_schema_for_class"
 :category "rb"
 :description (documentation 'rb-tools-get-json-schema-for-class)
 :function #'rb-tools-get-json-schema-for-class
 :args
 '(( :name "class"
     :type string
     :description "Name of the class, an uppercase string."))
 :confirm t
 :include t)


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
  "Return filesystem path for object CLASS with ID."
  (let* ((root (rb-tools--project-root))
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
          (with-temp-file file
            (insert val)))))))

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
 :confirm t
 :include t)

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
              (with-temp-buffer
                (insert-file-contents file)
                (push (cons key (buffer-string)) result))))
          (nreverse result))
      (let ((files (directory-files base nil "^[^.].*" t))
            (result '()))
        (dolist (fname files)
          (let ((file (expand-file-name fname base)))
            (when (file-regular-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (push (cons fname (buffer-string)) result)))))
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
 :confirm t
 :include t)

(defun rb-tools-update-object (class id properties)
  "Update an existing object in the store.

CLASS (string), ID (string), and PROPERTIES (array of name/value pairs)
are combined via `rb-tools--object-from-args'. Only the provided
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
 :confirm t
 :include t)

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

(cl-defun rb-tools-find-object (class &optional id properties)
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
      (cl-return-from rb-tools-find-object '()))
    (let* ((filters (seq-into properties 'list))
           (candidates (rb-tools--list-object-ids class-str)))
      (unless candidates
        (cl-return-from rb-tools-find-object '()))
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
          (cl-return-from rb-tools-find-object '())))
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

(gptel-make-tool
 :name "find_object"
 :category "rb"
 :description (documentation 'rb-tools-find-object)
 :function #'rb-tools-find-object
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
 :confirm t
 :include t)

;;; Tests -----------------------------------------------------------------
(require 'ert)

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

3. If a tool does not seem to work as advertised, stop immediately and
   report the problem.

_Development guidelines_

1. Follow the philosophy of `functional core, imperative shell'

2. When it is feasible, write tests first.

"
  :tools '(:eval (rb-tools--preset-tools-dev))
  :temperature 0.1
  :prompt-transform-functions '(:prepend (rb-gptel-mark-user-response-regions)))

(gptel-make-preset 'repo-reader
  :description "Grants read access to all files in the Git repository."
  :tools '(:append ("rg" "fd" "read_file"))
  :system '(:append "To explore the repository, use the following tools:

- =fd= for getting a recursive listing of all files in the project
- =rg= for finding files with lines matching a certain regex
- =read_file= to slurp an entire file into the context

"))

(gptel-make-preset 'repo-editor
  :description "Grants write access to all files in the Git repository."
  :parents '(repo-reader)
  :tools '(:append ("replace_line_ranges" "write_file"))
  :system '(:append "_Guidelines on changing files_

*If you want to add new content to the top of a file:*

Use =replace_line_ranges= with =start= and =end= both set to 1 and
=end_inclusive= set to false

*If you want to append new content to a file:*

Do something like this via bash:

```
cat <<EOF >>FILENAME
enter the content to append here
EOF
```

"))

(gptel-make-preset 'store-reader
  :description "Grants read access to the object store."
  :tools '(:append ("get_json_schema_for_class" "get_object" "find_object"))
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
  :system '(:append "Occasionally I will ask you to create an object of some class. When this
happens, I will explicitly state the object id you should use. If I do
not supply you with an explicit object id, stop and complain.

"))

(gptel-make-preset 'code-reader
  :description "Grants read access to source code files."
  :tools '(:append ("tree_sitter_list_nodes" "tree_sitter_get_nodes" "read_file"))
  :system '(:append "_Guidelines on analyzing source code files_

*If you want to analyze a source code file, use the following tools:*

1. tree_sitter_list_nodes: Gives you the name, type signature and
   starting line number of all function and variable definitions in the
   file.

2. tree_sitter_get_nodes: If you have starting line numbers, this tool
   translates them to the complete text of the AST nodes which start on
   those lines

3. read_file: Slurps the whole file into the context, with optional line
   numbers. Use this as a last resort as it may consume a lot of
   context.

"))

(gptel-make-preset 'code-editor
  :description "Grants write access to source code files."
  :parents '(code-reader)
  :tools '(:append ("tree_sitter_update_nodes" "replace_line_ranges" "write_file"))
  :system '(:append "_Guidelines on changing source code files_

*If you want to change something in a source code file:*

1. Use =tree_sitter_list_nodes= and =tree_sitter_get_nodes= to get the
   current state.

2. Use =tree_sitter_update_nodes= to update the desired AST nodes.

*If you want to add new AST nodes to the middle of a source code file:*

1. Use =tree_sitter_list_nodes= to get an overview of the top-level AST nodes.

2. Determine the best location for the insert.

3. Use =replace_line_ranges= with =start= and =end= both set to the
   point of insertion, and =end_inclusive= set to false: this will
   insert the content before the =start= line
"))

(gptel-make-preset 'plan
  :description "Used to plan stories and write tasks."
  :parents '(dev repo-editor code-reader store-editor)
  :system `(:append "Your present job is to help me plan stories and write tasks.

_Description of the planning workflow_

We will enter into a dialogue about a new feature or idea I have in
mind. Your task is to help me flesh out the details. When I feel that we
we are ready, I will ask you to write a STORY and break it down to
TASKs.

The STORY and TASKs you create will be used for implementation so they
should be sharp and crystal clear. If you feel that you do not have
enough information to guarantee this, stop and ask for clarification. We
shall continue this feedback loop until you are satisfied.

In the current stage, writing the code is out of scope: your only job is
to prepare the story and its tasks. Implementation will be done by a
coding agent in a later stage.

"))

(gptel-make-preset 'code
  :description "Used to implement tasks."
  :parents '(dev repo-editor code-editor store-editor)
  :system `(:append "Your present job is to implement story tasks.

_Description of the implementation workflow_

1. I provide you with the id of the TASK which should be implemented.

2. You retrieve the task and the corresponding story from the object
   store, analyze them, understand what to do.  If something is not
   clear, you stop and ask for clarification.

3. You implement the task.

4. If due to the implementation changes the story or the task
   descriptions do not accurately reflect reality, update them to ensure
   consistency.

"))

;;; rb-tools.el ends here
