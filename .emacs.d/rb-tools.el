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
 :category "nrepl"
 :description (documentation 'rb-tools-nrepl-eval)
 :function #'rb-tools-nrepl-eval
 :args
 '(( :name "input"
     :type string
     :description "The form to evaluate in the context of the connected Clojure/ClojureScript app.")
  '( :name "ns"
     :type string
     :description "The Clojure/ClojureScript namespace in which the form should be evaluated.")))

(defvar rb-tools-major-mode-to-ts-lang-alist
  '((emacs-lisp-mode . elisp)
    (go-mode . go)))

(defun rb-tools-ts-list-nodes (path)
  "Parse PATH using a Tree-sitter parser and return the list of top-level nodes.

Each element in the returned list contains the following fields:

- index: integer - node index (0..N-1)
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- preview: string - first line of node text"
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    ;; Make mode detection behave as if visiting PATH.
    (setq buffer-file-name path)
    (unwind-protect
        (progn
          (delay-mode-hooks
            (set-auto-mode))
          (let* ((language (alist-get major-mode rb-tools-major-mode-to-ts-lang-alist)))
            (unless language
              (error "No Tree-sitter language configured for %s" major-mode))
            (unless (treesit-ready-p language t)
              (error "Tree-sitter not ready for language %s" language))
            (let* ((parser (treesit-parser-create language))
                   (root (treesit-parser-root-node parser))
                   (child-count (treesit-node-child-count root t))
                   (i 0)
                   (result '()))
              (while (< i child-count)
                (let* ((node (treesit-node-child root i t))
                       (text (treesit-node-text node))
                       (preview (car (split-string text "\n")))
                       (line (line-number-at-pos (treesit-node-start node)))
                       (hash (secure-hash 'md5 text)))
                  (push (list :index i
                              :kind (treesit-node-type node)
                              :line line
                              :text_hash hash
                              :preview preview)
                        result))
                (setq i (1+ i)))
              (nreverse result))))
      (set-buffer-modified-p nil))))

(gptel-make-tool
 :name "tree_sitter_list_nodes"
 :category "TreeSitter"
 :description (documentation 'rb-tools-ts-list-nodes)
 :function #'rb-tools-ts-list-nodes
 :args
 '(( :name "path"
     :type string
     :description "Path to the source code file."))
 :confirm t)

(defun rb-tools-ts-get-nodes (path nodes)
  "Parse PATH using a Tree-sitter parser and return the text of specified NODES.

Each element in NODES contains the following fields:

- index: integer - node index (0..N-1)
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text

Each element in the returned list contains the following fields:

- index: integer - node index (0..N-1)
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- text: string - complete node text"
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    ;; Make mode detection behave as if visiting PATH.
    (setq buffer-file-name path)
    (unwind-protect
        (progn
          (delay-mode-hooks
            (set-auto-mode))
          (let* ((language (alist-get major-mode rb-tools-major-mode-to-ts-lang-alist)))
            (unless language
              (error "No Tree-sitter language configured for %s" major-mode))
            (unless (treesit-ready-p language t)
              (error "Tree-sitter not ready for language %s" language))
            (let* ((parser (treesit-parser-create language))
                   (root (treesit-parser-root-node parser))
                   (child-count (treesit-node-child-count root t))
                   (result '()))
              (dolist (spec (seq-into nodes 'list))
                (let* ((idx (plist-get spec :index)))
                  (unless (and (integerp idx) (>= idx 0) (< idx child-count))
                    (error "Invalid node index: %s" idx))
                  (let* ((node (treesit-node-child root idx t))
                         (text (treesit-node-text node))
                         (kind (treesit-node-type node))
                         (line (line-number-at-pos (treesit-node-start node)))
                         (hash (secure-hash 'md5 text))
                         (expected-kind (plist-get spec :kind))
                         (expected-line (plist-get spec :line))
                         (expected-hash (plist-get spec :text_hash)))
                    (when (not (string= expected-kind kind))
                      (error "Kind mismatch for node %s: expected %s, got %s"
                             idx expected-kind kind))
                    (when (not (= expected-line line))
                      (error "Line mismatch for node %s: expected %s, got %s"
                             idx expected-line line))
                    (when (not (string= expected-hash hash))
                      (error "Hash mismatch for node %s" idx))
                    (push (list :index idx
                                :kind kind
                                :line line
                                :text_hash hash
                                :text text)
                          result))))
              (nreverse result))))
      (set-buffer-modified-p nil))))

;; (rb-tools-ts-list-nodes "/home/rb/projects/dotfiles/.emacs.d/rb-tools.el")

;; (rb-tools-ts-list-nodes "/home/rb/projects/mixtape/box.go")

;; ((:index 0 :kind "package_clause" :line 1 :text_hash "8b75bc9db3ddddd09c3da91d38387339" :preview "package main") (:index 1 :kind "import_declaration" :line 3 :text_hash "5f26f138b736f5e1f13dbcbcd32c11ce" :preview "import (") (:index 2 :kind "type_declaration" :line 7 :text_hash "1161bfa084f3b03dd8f637f9b2f4539d" :preview "type Box[T any] struct {") (:index 3 :kind "method_declaration" :line 12 :text_hash "4a1956f47bfc77ec05f31f0e2e6a4c52" :preview "func (box *Box[T]) Get() T {") (:index 4 :kind "method_declaration" :line 18 :text_hash "cc601fcd91e614c77ff2e5d19a9b627a" :preview "func (box *Box[T]) Set(v T) {") (:index 5 :kind "method_declaration" :line 24 :text_hash "23057b94d43efa4227bbbe599f0681b0" :preview "func (box *Box[T]) Update(fn func(T) T) {"))

;; (rb-tools-ts-get-nodes "/home/rb/projects/mixtape/box.go" '((:index 3 :kind "method_declaration" :line 12 :text_hash "4a1956f47bfc77ec05f31f0e2e6a4c52" :preview "func (box *Box[T]) Get() T {")))

(gptel-make-tool
 :name "tree_sitter_get_nodes"
 :category "TreeSitter"
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
  "Parse PATH using a Tree-sitter parser and update specified NODES.

Each element in NODES contains the following fields:

- index: integer - node index (0..N-1)
- kind: string - node kind
- line: integer - line number
- text_hash: string - hash of node text
- new_text: string - new node text

The target file is first loaded into a temporary buffer.  The requested
updates are applied in decreasing line number order.  After all changes
have been made, the buffer is reparsed.  The target file is updated only
if the parse is successful."
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (unless (file-writable-p path)
    (error "File is not writable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    ;; Make mode detection behave as if visiting PATH.
    (setq buffer-file-name path)
    (unwind-protect
        (progn
          (delay-mode-hooks
            (set-auto-mode))
          (let* ((language (alist-get major-mode rb-tools-major-mode-to-ts-lang-alist)))
            (unless language
              (error "No Tree-sitter language configured for %s" major-mode))
            (unless (treesit-ready-p language t)
              (error "Tree-sitter not ready for language %s" language))
            (let* ((parser (treesit-parser-create language))
                   (root (treesit-parser-root-node parser))
                   (child-count (treesit-node-child-count root t))
                   (updates (sort (copy-sequence nodes)
                                  (lambda (a b)
                                    (> (plist-get a :line) (plist-get b :line))))))
              (dolist (spec (seq-into updates 'list))
                (let* ((idx (plist-get spec :index))
                       (new-text (plist-get spec :new_text)))
                  (unless (and (integerp idx) (>= idx 0) (< idx child-count))
                    (error "Invalid node index: %s" idx))
                  (unless (stringp new-text)
                    (error "Missing new_text for node %s" idx))
                  (let* ((node (treesit-node-child root idx t))
                         (text (treesit-node-text node))
                         (kind (treesit-node-type node))
                         (line (line-number-at-pos (treesit-node-start node)))
                         (hash (secure-hash 'md5 text))
                         (expected-kind (plist-get spec :kind))
                         (expected-line (plist-get spec :line))
                         (expected-hash (plist-get spec :text_hash)))
                    (unless (string= expected-kind kind)
                      (error "Kind mismatch for node %s: expected %s, got %s"
                             idx expected-kind kind))
                    (unless (= expected-line line)
                      (error "Line mismatch for node %s: expected %s, got %s"
                             idx expected-line line))
                    (unless (string= expected-hash hash)
                      (error "Hash mismatch for node %s" idx))
                    (let ((start (treesit-node-start node))
                          (end (treesit-node-end node)))
                      (goto-char start)
                      (delete-region start end)
                      (insert new-text)))))
              ;; Re-parse after edits; update original only on success.
              (let* ((parser2 (treesit-parser-create language))
                     (root2 (treesit-parser-root-node parser2)))
                (unless root2
                  (error "Tree-sitter reparsing failed"))
                (when (treesit-search-subtree
                       root2
                       (lambda (node)
                         (or (treesit-node-check node 'has-error)
                             (treesit-node-check node 'missing))))
                  (error "Tree-sitter reparsing failed"))
                (write-region (point-min) (point-max) path nil 'silent)))))
      (set-buffer-modified-p nil))))

(gptel-make-tool
 :name "tree_sitter_update_nodes"
 :category "TreeSitter"
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

;;; rb-tools.el ends here
