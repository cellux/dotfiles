;;; rb-tools.el --- RB's helper tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for RB's GPT/Tree-sitter and tooling setup.

;;; Code:

(require 'cider)

(defun rb-tools-nrepl-eval (input &optional ns)
  "Send INPUT to currently connected Clojure/ClojureScript REPL for evaluation.
The form is evaluated in the namespace NS if specified or in the current namespace otherwise.
The first line of the response is OK if the call succeeded and ERROR if it did not.
The rest of the response contains the value of the expression or the error message."
  (let* ((response (cider-nrepl-sync-request:eval input nil ns)))
    (if (nrepl-dict-contains response "err")
        (format "ERROR\n%s" (nrepl-dict-get response "err"))
      (format "OK\n%s" (nrepl-dict-get response "value")))))

(gptel-make-tool
 :name "nrepl_eval"
 :category "rb"
 :description (documentation 'rb-tools-nrepl-eval)
 :function #'rb-tools-nrepl-eval
 :args
 '(( :name "input"
     :type string
     :description "The form to evaluate in the context of the connected Clojure/ClojureScript app.")
   ( :name "ns"
     :type string
     :description "The Clojure/ClojureScript namespace in which the form should be evaluated.")))

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

(defun rb-tools--ts-with-root (path require-writable fn)
  "Open PATH in a temp buffer, ensure mode/Tree-sitter ready, and run FN.

FN is called with three args: LANGUAGE, PARSER, ROOT."
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
                (error "Tree-sitter failed to parse %s" path))
              (funcall fn language parser root))))
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

(defun rb-tools-ts-list-nodes (path)
  "Parse PATH using Tree-sitter and return the list of top-level AST nodes.

Each AST node has the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- preview: string - first line of node text"
  (rb-tools--ts-with-root
   path nil
   (lambda (_language _parser root)
     (let* ((child-count (treesit-node-child-count root t))
            (result '()))
       (dotimes (i child-count)
         (let* ((node (treesit-node-child root i t))
                (text (treesit-node-text node))
                (preview (car (split-string text "\n")))
                (line (line-number-at-pos (treesit-node-start node))))
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
     :description "Path to the source code file."))
 :confirm t)

(defun rb-tools-ts-get-nodes (path nodes)
  "Parse PATH using Tree-sitter and return the text of specified NODES.

Each element in NODES must contain the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text

Each element in the returned list contains the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- text: string - full node text"
  (rb-tools--ts-with-root
   path nil
   (lambda (_language _parser root)
     (let ((result '()))
       (dolist (spec (seq-into nodes 'list))
         (let* ((validated (rb-tools--ts-validate-node root spec))
                (node (nth 0 validated))
                (text (nth 1 validated))
                (kind (nth 2 validated))
                (line (nth 3 validated))
                (hash (nth 4 validated)))
           (push (list :index (plist-get spec :index)
                       :kind kind
                       :line line
                       :text_hash hash
                       :text text)
                 result)))
       (nreverse result)))))

;; (rb-tools-ts-list-nodes "/home/rb/projects/dotfiles/.emacs.d/rb-tools.el")q

;; (rb-tools-ts-list-nodes "/home/rb/projects/mixtape/box.go")

;; ((:index 0 :kind "package_clause" :line 1 :text_hash "8b75bc9db3ddddd09c3da91d38387339" :preview "package main") (:index 1 :kind "import_declaration" :line 3 :text_hash "5f26f138b736f5e1f13dbcbcd32c11ce" :preview "import (") (:index 2 :kind "type_declaration" :line 7 :text_hash "1161bfa084f3b03dd8f637f9b2f4539d" :preview "type Box[T any] struct {") (:index 3 :kind "method_declaration" :line 12 :text_hash "4a1956f47bfc77ec05f31f0e2e6a4c52" :preview "func (box *Box[T]) Get() T {") (:index 4 :kind "method_declaration" :line 18 :text_hash "cc601fcd91e614c77ff2e5d19a9b627a" :preview "func (box *Box[T]) Set(v T) {") (:index 5 :kind "method_declaration" :line 24 :text_hash "23057b94d43efa4227bbbe599f0681b0" :preview "func (box *Box[T]) Update(fn func(T) T) {"))

;; (rb-tools-ts-get-nodes "/home/rb/projects/mixtape/box.go" '((:index 3 :kind "method_declaration" :line 12 :text_hash "4a1956f47bfc77ec05f31f0e2e6a4c52" :preview "func (box *Box[T]) Get() T {")))

(gptel-make-tool
 :name "tree_sitter_get_nodes"
 :category "rb"
 :description (documentation 'rb-tools-ts-get-nodes)
 :function #'rb-tools-ts-get-nodes
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
                            :text_hash (:type string)))
     :description "List of nodes to return."))
 :confirm t)

(defun rb-tools-ts-update-nodes (path nodes)
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

On success, returns the list of updated nodes:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of new node text"
  (rb-tools--ts-with-root
   path t
   (lambda (language _parser root)
     (let* ((updates (sort (seq-into nodes 'list)
                           (lambda (a b)
                             (> (plist-get a :line)
                                (plist-get b :line))))))
       (dolist (spec updates)
         (unless (stringp (plist-get spec :new_text))
           (error "Missing new_text for node %s" (plist-get spec :index)))
         (let* ((validated (rb-tools--ts-validate-node root spec))
                (node (nth 0 validated))
                (new-text (plist-get spec :new_text)))
           (let ((start (treesit-node-start node))
                 (end (treesit-node-end node)))
             (goto-char start)
             (delete-region start end)
             (insert new-text))))
       (let* ((parser2 (treesit-parser-create language))
              (root2 (treesit-parser-root-node parser2)))
         (unless (rb-tools--ts-node-successfully-parsed? root2)
           (error "Tree-sitter reparsing failed"))
         (write-region (point-min) (point-max) path nil 'silent)
         (let* ((child-count (treesit-node-child-count root2 t))
                (result '()))
           (dotimes (i child-count)
             (let* ((node (treesit-node-child root i t))
                    (text (treesit-node-text node))
                    (line (line-number-at-pos (treesit-node-start node))))
               (push (list :index i
                           :kind (treesit-node-type node)
                           :line line
                           :text_hash (secure-hash 'md5 text))
                     result)))
           (nreverse result)))))))

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
     :description "List of nodes to update."))
 :confirm t)

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
               (end-inc (plist-get spec :end_inclusive))
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

(defun rb-tools-read-file (path)
  "Read the file at PATH and return its content."
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(gptel-make-tool
 :name "read_file"
 :category "rb"
 :description (documentation 'rb-tools-read-file)
 :function #'rb-tools-read-file
 :args
 '(( :name "path"
     :type string
     :description "Path of the file to read."))
 :confirm t)

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

;;; rb-tools.el ends here
