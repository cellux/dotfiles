;;; rb-ts.el --- RB's Tree-sitter tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities around Tree-sitter.

;;; Code:

(require 'treesit)

(require 'rb-io)
(require 'rb-tools)

(defvar rb-ts-major-mode-to-tree-sitter-language-alist
  '((emacs-lisp-mode . elisp)
    (go-mode . go))
  "Maps Emacs major mode symbols to Tree-sitter language identifiers.")

(defvar rb-ts-format-function
  #'indent-region
  "Function used to format updated Tree-sitter nodes.
Called with START and END when formatting is requested.  Bind to nil to
disable formatting.")

(defun rb-ts--node-successfully-parsed? (node)
  "Determine if Tree-sitter could parse NODE without errors or missing parts.
Returns nil if the parse failed and a non-nil value otherwise."
  (and node (not (treesit-search-subtree
                  node
                  (lambda (node)
                    (or (treesit-node-check node 'has-error)
                        (treesit-node-check node 'missing)))))))

(defun rb-ts--node-parse-errors (node)
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

(defun rb-ts-parse-buffer (&optional success-fn)
  "Parse the current buffer with Tree-sitter and signal errors.
Before parsing, calls `set-auto-mode' to set buffer-local variable
`major-mode' according to the buffer contents.  If the parse succeeds,
calls optional SUCCESS-FN with Tree-sitter PARSER and root NODE as
arguments.  Returns non-nil when the parse succeeds; signals with
diagnostics otherwise."
  (unwind-protect
      (progn
        (delay-mode-hooks (set-auto-mode))
        (let ((language (alist-get major-mode rb-ts-major-mode-to-tree-sitter-language-alist)))
          (unless language
            (error "No Tree-sitter language configured for %s" major-mode))
          (unless (treesit-ready-p language t)
            (error "Tree-sitter not ready for language %s" language))
          (let* ((parser (treesit-parser-create language))
                 (root (treesit-parser-root-node parser)))
            (unless (rb-ts--node-successfully-parsed? root)
              (error (or (rb-ts--node-parse-errors root)
                         (format "Tree-sitter failed to parse %s" (or buffer-file-name "buffer")))))
            (or (funcall success-fn parser root) t))))
    (set-buffer-modified-p nil)))

(defun rb-ts-with-root (path require-writable fn)
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
    (rb-ts-parse-buffer fn)))

(defun rb-ts--validate-node (root spec)
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
      (unless (string= (plist-get spec :hash) hash)
        (error "Hash mismatch for node %s" idx))
      (list node text kind line hash))))

(defun rb-ts--collect-root-children (root)
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

(defun rb-ts--format-list-nodes (children &optional preview-lines)
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
               :hash (secure-hash 'md5 text)
               :preview preview)))
     children)))

(defun rb-ts--select-nodes-by-line (children line-numbers)
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
                 :hash (secure-hash 'md5 text)
                 :text text))))
     lines)))

(defun rb-ts--resolve-node (root selector)
  "Resolve a unique top-level node in ROOT matching SELECTOR.

SELECTOR is a plist that may contain :hash (string) and/or :line
 (integer).  At least one selector must be provided.  When both are
provided, both must refer to the same node.

Returns a plist with :node, :index, :kind, :line, :start, :end, :text,
and :hash.  Signals an error on missing selectors, missing matches,
or ambiguous matches."
  (let* ((line (plist-get selector :line))
         (hash (plist-get selector :hash)))
    (unless (or line hash)
      (error "Selector must provide :line and/or :hash"))
    (let ((child-count (treesit-node-child-count root t))
          line-match
          hash-matches)
      (dotimes (i child-count)
        (let* ((node (treesit-node-child root i t))
               (start (treesit-node-start node))
               (end (treesit-node-end node))
               (kind (treesit-node-type node))
               (text (treesit-node-text node))
               (node-line (line-number-at-pos start))
               (text-hash (secure-hash 'md5 text))
               (entry (list :node node
                            :index i
                            :kind kind
                            :line node-line
                            :start start
                            :end end
                            :text text
                            :hash text-hash)))
          (when (and line (= line node-line))
            (setq line-match entry))
          (when (and hash (string= hash text-hash))
            (push entry hash-matches))))
      (when (and line (not line-match))
        (error "No node starts on line %s" line))
      (when (and hash (null hash-matches))
        (error "No node matches hash %s" hash))
      (when (> (length hash-matches) 1)
        (error "Multiple nodes match hash %s; disambiguate with :line" hash))
      (let (candidate)
        (cond
         ((and line hash)
          (unless (and line-match
                       (string= hash (plist-get line-match :hash)))
            (error "Line %s and hash did not match the same node" line))
          (setq candidate line-match))
         (line
          (setq candidate line-match))
         (hash
          (setq candidate (car hash-matches))))
        (unless candidate
          (error "Failed to resolve node"))
        candidate))))

(defun rb-ts--list-nodes-pure (root &optional preview-lines)
  "Pure worker for `rb-ts-list-nodes'.
Accepts ROOT and returns formatted node plists.
PREVIEW-LINES controls preview length."
  (rb-ts--format-list-nodes
   (rb-ts--collect-root-children root)
   preview-lines))

(defun rb-ts--get-nodes-pure (root line-numbers)
  "Pure worker for `rb-ts-get-nodes'.
Accepts ROOT and LINE-NUMBERS."
  (rb-ts--select-nodes-by-line
   (rb-ts--collect-root-children root)
   line-numbers))

(defun rb-ts-list-nodes (path &optional preview-lines)
  "Parse PATH using Tree-sitter and return the list of top-level AST nodes.

Each AST node has the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- hash: string - hash of node text
- preview: string - first PREVIEW-LINES lines of node text

PREVIEW-LINES controls how many lines are included in each preview (default: 1)."
  (rb-ts-with-root
   path nil
   (lambda (_parser root)
     (rb-ts--list-nodes-pure root preview-lines))))

(defun rb-ts-get-nodes (path line-numbers)
  "Parse PATH using Tree-sitter and return the text of nodes on LINE-NUMBERS.

LINE-NUMBERS should be a list of integer line numbers referencing the start
of each target node.

Each element in the returned list contains the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- hash: string - hash of node text
- text: string - full node text"
  (rb-ts-with-root
   path nil
   (lambda (_parser root)
     (rb-ts--get-nodes-pure root line-numbers))))

(defun rb-ts-update-nodes (path nodes &optional skip-format dry-run)
  "Parse PATH using Tree-sitter and update specified NODES.

Each element in NODES must contain the following fields:

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- hash: string - hash of node text
- new_text: string - new node text

The target file is first loaded into a temporary buffer and the
requested updates are applied in decreasing line number order.  After
all changes have been made, the buffer is reparsed.  The target file is
updated only if the parse is successful.

Updated nodes are formatted via `rb-ts-format-function' (default:
`indent-region') unless SKIP-FORMAT is non-nil.

If DRY-RUN is non-nil, apply the edits in-memory, reparse to validate,
and return a report without writing PATH.  The report includes the
planned changes, the reparse result, and a flag indicating no write
occurred.

On success, returns the list of updated nodes (or the dry-run report):

- index: integer - node index
- kind: string - node kind
- line: integer - line number
- hash: string - hash of new node text"
  (rb-ts-with-root
   path t
   (lambda (_parser _root)
     (let* ((updates (sort (seq-into nodes 'list)
                           (lambda (a b)
                             (> (plist-get a :line)
                                (plist-get b :line)))))
            (changes '()))
       (dolist (spec updates)
         (unless (stringp (plist-get spec :new_text))
           (error "Missing new_text for node %s" (plist-get spec :index)))
         (let* ((latest-root (treesit-buffer-root-node)) ; root changes after each update
                (validated (rb-ts--validate-node latest-root spec))
                (node (nth 0 validated))
                (old-text (treesit-node-text node))
                (new-text (plist-get spec :new_text)))
           (let ((start (treesit-node-start node))
                 (end (treesit-node-end node)))
             (goto-char start)
             (delete-region start end)
             (insert new-text)
             (when-let ((fmt-fn (unless skip-format rb-ts-format-function)))
               (funcall fmt-fn start (point))))
           (push (list :index (plist-get spec :index)
                       :kind (plist-get spec :kind)
                       :line (plist-get spec :line)
                       :old_hash (secure-hash 'md5 old-text)
                       :new_hash (secure-hash 'md5 new-text)
                       :old_text old-text
                       :new_text new-text)
                 changes)))
       (let* ((root2 (treesit-buffer-root-node)))
         (unless (rb-ts--node-successfully-parsed? root2)
           (error (or (rb-ts--node-parse-errors root2)
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
                           :hash (secure-hash 'md5 text))
                     result)))
           (if dry-run
               (list :dry_run t
                     :changes (nreverse changes)
                     :result (nreverse result))
             (progn
               (rb-io-write-file path (buffer-string))
               (nreverse result)))))))))

(defun rb-ts--insert-relative (path selector new-text position &optional skip-format)
  "Helper to insert NEW-TEXT relative to a resolved node in PATH.
POSITION is either :before or :after.  SELECTOR is passed to
`rb-ts--resolve-node'.  Formats inserted region unless SKIP-FORMAT is
non-nil.  Writes the file only after Tree-sitter reparsing succeeds.  If
the insert was successful, returns plist (:inserted t)."
  (unless (member position '(:before :after))
    (error "POSITION must be :before or :after"))
  (unless (stringp new-text)
    (error "NEW-TEXT must be a string"))
  (rb-ts-with-root
   path t
   (lambda (_parser root)
     (let* ((resolved (rb-ts--resolve-node root selector))
            (insert-pos (if (eq position :before)
                            (plist-get resolved :start)
                          ;; :end points to last char of the node
                          (1+ (plist-get resolved :end)))))
       (goto-char insert-pos)
       (insert new-text)
       (when-let ((fmt-fn (unless skip-format rb-ts-format-function))
                  (insert-end (point)))
         (funcall fmt-fn insert-pos insert-end))
       (let ((root2 (treesit-buffer-root-node)))
         (unless (rb-ts--node-successfully-parsed? root2)
           (error (or (rb-ts--node-parse-errors root2)
                      "Tree-sitter reparsing failed")))
         (rb-io-write-file path (buffer-string))
         (list :inserted t))))))

(defun rb-ts-insert-before-node (path selector new-text &optional skip-format)
  "Insert NEW-TEXT before the node selected by SELECTOR in PATH.

SELECTOR is a plist that may contain :hash (string) and/or :line
 (integer) referring to a top-level node.  The insertion is performed in
memory, Tree-sitter is asked to reparse, and the file is written only if
parsing succeeds.  The inserted region is formatted unless SKIP-FORMAT
is truthy.

If the insert was successful, returns plist (:inserted t)."
  (rb-ts--insert-relative path selector new-text :before skip-format))

(defun rb-ts-insert-after-node (path selector new-text &optional skip-format)
  "Insert NEW-TEXT after the node selected by SELECTOR in PATH.

SELECTOR is a plist that may contain :hash (string) and/or :line
 (integer) referring to a top-level node.  The insertion is performed in
memory, Tree-sitter is asked to reparse, and the file is written only if
parsing succeeds.  The inserted region is formatted unless SKIP-FORMAT
is truthy.

If the insert was successful, returns plist (:inserted t)."
  (rb-ts--insert-relative path selector new-text :after skip-format))


;;; Test helpers

(defun rb-ts--line-number-of-substring (content substring)
  "Return the 1-based line number where SUBSTRING first occurs in CONTENT."
  (let ((pos (string-match (regexp-quote substring) content)))
    (unless pos
      (error "Substring %s not found in fixture content" substring))
    (1+ (cl-count ?\n (substring content 0 pos)))))

(defun rb-ts--multi-form-fixture ()
  "Return fixture metadata describing a file with multiple top-level forms."
  (let* ((defvar "(defvar alpha 1)")
         (nl "\n")
         (alpha-defun "(defun alpha ()\n  (message \"alpha\"))")
         (beta-defun "(defun beta ()\n  (message \"beta\"))")
         (content (concat ";;; fixture -*- mode: emacs-lisp -*-\n\n" defvar nl nl alpha-defun nl nl beta-defun nl)))
    (list :content content
          :alpha-line (rb-ts--line-number-of-substring content "(defun alpha")
          :beta-line (rb-ts--line-number-of-substring content "(defun beta")
          :beta-text beta-defun)))


;;; Unit tests
(require 'ert)

(ert-deftest rb-ts--format-list-nodes/basic-preview ()
  (let* ((children (list (list :index 0 :kind "func" :line 5 :text "line1\nline2\nline3")
                         (list :index 1 :kind "var" :line 10 :text "only1"))))
    (let ((result (rb-ts--format-list-nodes children 2)))
      (should (= 2 (length result)))
      (pcase-let ((`((:index 0 :kind "func" :line 5 :hash ,h1 :preview ,p1)
                     (:index 1 :kind "var" :line 10 :hash ,h2 :preview ,p2)) result))
        (should (string= h1 (secure-hash 'md5 "line1\nline2\nline3")))
        (should (string= h2 (secure-hash 'md5 "only1")))
        (should (string= p1 "line1\nline2"))
        (should (string= p2 "only1"))))))

(ert-deftest rb-ts--format-list-nodes/defaults-to-1-line ()
  (let* ((children (list (list :index 0 :kind "func" :line 5 :text "a\nb"))))
    (dolist (preview '(nil 0 -1))
      (let* ((result (rb-ts--format-list-nodes children preview))
             (first (car result)))
        (should (string= (plist-get first :preview) "a"))))))

(ert-deftest rb-ts--select-nodes-by-line/picks-and-validates ()
  (let* ((children (list (list :index 0 :kind "func" :line 5 :text "fn")
                         (list :index 1 :kind "var" :line 10 :text "var"))))
    (let ((result (rb-ts--select-nodes-by-line children '(10 5))))
      (pcase-let ((`((:index 1 :kind "var" :line 10 :hash ,h2 :text "var")
                     (:index 0 :kind "func" :line 5 :hash ,h1 :text "fn")) result))
        (should (string= h2 (secure-hash 'md5 "var")))
        (should (string= h1 (secure-hash 'md5 "fn"))))))
  ;; Missing node
  (should-error (rb-ts--select-nodes-by-line '() '(1)))
  ;; Non-integer line
  (should-error (rb-ts--select-nodes-by-line '() '("x"))))

(ert-deftest rb-ts-insert-before-node/basic ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((original ";;; -*- mode: emacs-lisp; -*-\n\n(defun foo ()\n  (message \"hi\"))\n")
           (selector (list :line 3))
           (new-text ";; inserted\n")
           (expected ";;; -*- mode: emacs-lisp; -*-\n\n;; inserted\n(defun foo ()\n  (message \"hi\"))\n"))
      (rb-io-write-file path original)
      (let* ((result (rb-ts-insert-before-node path selector new-text))
             (updated (rb-io-read-file path)))
        (should (eq t (plist-get result :inserted)))
        (should (equal updated expected))))))

(ert-deftest rb-ts-insert-before-node/skip-format ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (rb-io-write-file path ";;; -*- mode: emacs-lisp; -*-\n\n(defun foo ()\n  (message \"hi\"))\n")
    (let* (formatted
           (rb-ts-format-function (lambda (_s _e) (setq formatted t))))
      (rb-ts-insert-before-node path (list :line 1) ";; fmt\n")
      (should formatted))
    (rb-io-write-file path ";;; -*- mode: emacs-lisp; -*-\n\n(defun foo ()\n  (message \"hi\"))\n")
    (let* ((formatted nil)
           (rb-ts-format-function (lambda (_s _e) (setq formatted t))))
      (rb-ts-insert-before-node path (list :line 1) ";; nofmt\n" t)
      (should-not formatted))))

(ert-deftest rb-ts-insert-before-node/rejects-ambiguous-hash ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((node-text "(defvar x 1)\n")
           (content (concat node-text node-text))
           (hash (secure-hash 'md5 node-text)))
      (rb-io-write-file path content)
      (should-error (rb-ts-insert-before-node path (list :hash hash) ";; fail\n")))))

(ert-deftest rb-ts-insert-before-node/line-and-hash-selectors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((fixture (rb-ts--multi-form-fixture))
           (before-text ";; before alpha\n")
           (after-text ";; after beta\n")
           (beta-hash (secure-hash 'md5 (plist-get fixture :beta-text))))
      (rb-io-write-file path (plist-get fixture :content))
      (rb-ts-insert-before-node path
                                (list :line (plist-get fixture :alpha-line))
                                before-text)
      (rb-ts-insert-after-node path
                               (list :hash beta-hash)
                               after-text)
      (let ((expected (with-temp-buffer
                        (insert (plist-get fixture :content))
                        (goto-char (point-min))
                        (forward-line (1- (plist-get fixture :alpha-line)))
                        (insert before-text)
                        (goto-char (point-max))
                        (insert after-text)
                        (buffer-string))))
        (should (string= (rb-io-read-file path) expected))))))

(ert-deftest rb-ts-insert-before-node/fixture-parse-error-preserves-content ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture (rb-ts--multi-form-fixture)))
      (rb-io-write-file path (plist-get fixture :content))
      (should-error (rb-ts-insert-before-node path
                                              (list :line (plist-get fixture :alpha-line))
                                              "("))
      (should (string= (rb-io-read-file path) (plist-get fixture :content))))))

(ert-deftest rb-ts-insert-after-node/rejects-parse-errors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((original "(defun foo ()\n  (message \"hi\"))\n"))
      (rb-io-write-file path original)
      (should-error (rb-ts-insert-after-node path (list :line 1) "("))
      (should (string= original (rb-io-read-file path))))))

(ert-deftest rb-ts-insert-after-node/missing-selector ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture (rb-ts--multi-form-fixture)))
      (rb-io-write-file path (plist-get fixture :content))
      (should-error (rb-ts-insert-after-node path (list :line 999)
                                             ";; missing\n"))
      (should (string= (rb-io-read-file path) (plist-get fixture :content))))))

(ert-deftest rb-ts--resolve-node/selectors-and-errors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((fixture (rb-ts--multi-form-fixture))
           (alpha-text "(defun alpha ()\n  (message \"alpha\"))")
           (beta-text (plist-get fixture :beta-text))
           (beta-hash (secure-hash 'md5 beta-text)))
      (rb-io-write-file path (plist-get fixture :content))
      (rb-ts-with-root path nil
                       (lambda (_parser root)
                         (let ((alpha (rb-ts--resolve-node root (list :line (plist-get fixture :alpha-line))))
                               (beta (rb-ts--resolve-node root (list :hash beta-hash))))
                           (should (= (plist-get alpha :line) (plist-get fixture :alpha-line)))
                           (should (string= (plist-get alpha :text) alpha-text))
                           (should (= (plist-get beta :line) (plist-get fixture :beta-line)))
                           (should (string= (plist-get beta :hash) beta-hash)))
                         (rb-ts--resolve-node root (list :line (plist-get fixture :beta-line)
                                                         :hash beta-hash))
                         (should-error
                          (rb-ts--resolve-node root
                                               (list :line (plist-get fixture :alpha-line)
                                                     :hash beta-hash)))
                         (should-error (rb-ts--resolve-node root (list)))
                         (should-error (rb-ts--resolve-node root (list :line 999))))))))

(ert-deftest rb-ts--resolve-node/ambiguous-hash ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((node-text "(defvar x 1)\n")
           (content (concat ";;; -*- mode: emacs-lisp -*-\n\n" node-text node-text))
           (hash (secure-hash 'md5 node-text)))
      (rb-io-write-file path content)
      (rb-ts-with-root path nil
                       (lambda (_parser root)
                         (should-error (rb-ts--resolve-node root (list :hash hash))))))))

(ert-deftest rb-ts-update-nodes/updates-and-formats ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((fixture (rb-ts--multi-form-fixture))
           (formatted nil)
           (rb-ts-format-function (lambda (_s _e) (setq formatted t))))
      (rb-io-write-file path (plist-get fixture :content))
      (let* ((nodes (rb-ts-list-nodes path))
             (beta (seq-find (lambda (n)
                               (= (plist-get n :line) (plist-get fixture :beta-line)))
                             nodes))
             (new-text "(defun beta ()\n  (message \"beta+\"))"))
        (let* ((beta-spec (plist-put (copy-sequence beta) :new_text new-text))
               (result (rb-ts-update-nodes path (list beta-spec))))
          (should formatted)
          (should (string-match "beta+" (rb-io-read-file path)))
          (let ((beta-updated (seq-find (lambda (n)
                                           (= (plist-get n :index) (plist-get beta :index)))
                                         result)))
            (should beta-updated)
            (should (string= (plist-get beta-updated :hash)
                             (secure-hash 'md5 new-text)))))))))

(ert-deftest rb-ts-update-nodes/skip-format-and-dry-run ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((fixture (rb-ts--multi-form-fixture))
           (formatted nil)
           (rb-ts-format-function (lambda (_s _e) (setq formatted t))))
      (rb-io-write-file path (plist-get fixture :content))
      (let* ((nodes (rb-ts-list-nodes path))
             (beta (seq-find (lambda (n)
                               (= (plist-get n :line) (plist-get fixture :beta-line)))
                             nodes))
             (new-text "(defun beta ()\n(message \"beta\"))"))
        (rb-ts-update-nodes path (list (plist-put (copy-sequence beta) :new_text new-text)) t)
        (should-not formatted)
        (rb-io-write-file path (plist-get fixture :content))
        (setq formatted nil)
        (let* ((report (rb-ts-update-nodes path (list (plist-put (copy-sequence beta) :new_text new-text)) nil t)))
          (should (plist-get report :dry_run))
          (should (string= (rb-io-read-file path) (plist-get fixture :content)))
          (let ((change (car (plist-get report :changes))))
            (should (string= (plist-get change :new_hash) (secure-hash 'md5 new-text)))))))))

(ert-deftest rb-ts-update-nodes/validation-and-parse-errors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((fixture (rb-ts--multi-form-fixture)))
      (rb-io-write-file path (plist-get fixture :content))
      (let* ((nodes (rb-ts-list-nodes path))
             (alpha (seq-find (lambda (n)
                                (= (plist-get n :line) (plist-get fixture :alpha-line)))
                              nodes))
             (beta (seq-find (lambda (n)
                               (= (plist-get n :line) (plist-get fixture :beta-line)))
                             nodes)))
        (should-error (rb-ts-update-nodes path (list (list :index (plist-get beta :index)))))
        (let ((beta-bad (copy-sequence beta)))
          (setq beta-bad (plist-put beta-bad :hash "bogus"))
          (setq beta-bad (plist-put beta-bad :new_text "(defvar y 2)"))
          (should-error (rb-ts-update-nodes path (list beta-bad))))
        (let ((alpha-bad (plist-put (copy-sequence alpha) :new_text "(")))
          (should-error (rb-ts-update-nodes path (list alpha-bad)))
          ;; Parsing failed: file should remain unchanged and we should still be able to reparse it
          (should (string= (rb-io-read-file path) (plist-get fixture :content)))
          (rb-ts-with-root path nil (lambda (_ r) (should (rb-ts--node-successfully-parsed? r)))))))))

(ert-deftest rb-ts-insert-after-node/skip-format ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let* ((fixture (rb-ts--multi-form-fixture))
           (formatted nil)
           (rb-ts-format-function (lambda (_s _e) (setq formatted t))))
      (rb-io-write-file path (plist-get fixture :content))
      (rb-ts-insert-after-node path (list :line (plist-get fixture :alpha-line)) ";; fmt\n")
      (should formatted)
      (rb-io-write-file path (plist-get fixture :content))
      (setq formatted nil)
      (rb-ts-insert-after-node path (list :line (plist-get fixture :alpha-line)) ";; nofmt\n" t)
      (should-not formatted))))

(ert-deftest rb-ts--node-successfully-parsed?-reports-errors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"ok\")\n")
    (let* ((parser (treesit-parser-create 'elisp))
           (root (treesit-parser-root-node parser)))
      (should (rb-ts--node-successfully-parsed? root))
      (should-not (rb-ts--node-parse-errors root))))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun broke (\n  (message \"hi\")\n")
    (let* ((parser (treesit-parser-create 'elisp))
           (root (treesit-parser-root-node parser)))
      (should-not (rb-ts--node-successfully-parsed? root))
      (let ((errors (rb-ts--node-parse-errors root)))
        (should (stringp errors))
        (should (string-match "line" errors))
        (should (string-match "ERROR" errors))))))

(ert-deftest rb-ts-parse-buffer/parses-and-calls-success-fn ()
  (skip-unless (treesit-ready-p 'elisp t))
  (let ((buffer-file-name "test.el"))
    (insert ";;; -*- mode: emacs-lisp -*-\n\n(defvar foo 1)\n")
    (goto-char (point-max))
    (let ((saw-root nil)
          (root-kind nil))
      (should (rb-ts-parse-buffer
               (lambda (_parser root)
                 (setq saw-root t)
                 (setq root-kind (treesit-node-type root)))))
      (should saw-root)
      (should (string= root-kind "source_file")))))

(ert-deftest rb-ts-parse-buffer/rejects-unsupported-major-mode ()
  (skip-unless (treesit-ready-p 'elisp t))
  (with-temp-buffer
    (let ((buffer-file-name "unsupported.txt"))
      (insert "plain text\n")
      (let ((err (condition-case err
                     (rb-ts-parse-buffer)
                   (error err))))
        (should (string-match "No Tree-sitter language configured for" (error-message-string err)))))))

(ert-deftest rb-ts-parse-buffer/reports-diagnostics-on-failure ()
  (skip-unless (treesit-ready-p 'elisp t))
  (with-temp-buffer
    (let ((buffer-file-name "test.el"))
      (insert ";;; -*- mode: emacs-lisp -*-\n\n(defun broken (\n")
      (let ((err (condition-case err
                     (rb-ts-parse-buffer)
                   (error err))))
        (should (string-match "line 3: ERROR (defun broken ("
                              (error-message-string err)))))))

(ert-deftest rb-ts-with-root/parses-readable-file ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture (rb-ts--multi-form-fixture)))
      (rb-io-write-file path (plist-get fixture :content))
      (let ((result (rb-ts-with-root path nil
                                     (lambda (_parser root)
                                       (treesit-node-type root)))))
        (should (string= result "source_file"))))))

(ert-deftest rb-ts-with-root/require-writable-path ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (rb-io-write-file path ";;; -*- mode: emacs-lisp -*-\n")
    (let ((orig-mode (file-modes path)))
      (unwind-protect
          (progn
            (set-file-modes path #o400)
            (let ((err (condition-case err
                           (rb-ts-with-root path t (lambda (_ _)))
                         (error err))))
              (should (string= (error-message-string err)
                               (format "File is not writable: %s" path)))))
        (when orig-mode
          (set-file-modes path orig-mode))))))

(ert-deftest rb-ts-with-root/rejects-unreadable-path ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (rb-io-write-file path ";;; -*- mode: emacs-lisp -*-\n")
    (let ((orig-mode (file-modes path)))
      (unwind-protect
          (progn
            (set-file-modes path #o200)
            (let ((err (condition-case err
                           (rb-ts-with-root path nil (lambda (_ _)))
                         (error err))))
              (should (string= (error-message-string err)
                               (format "File is not readable: %s" path)))))
        (when orig-mode
          (set-file-modes path orig-mode))))))

(ert-deftest rb-ts--collect-root-children/returns-expected-fixture-data ()
  (skip-unless (treesit-ready-p 'elisp t))
  (let ((fixture (rb-ts--multi-form-fixture)))
    (with-temp-buffer
      (insert (plist-get fixture :content))
      (emacs-lisp-mode)
      (let* ((parser (treesit-parser-create 'elisp))
             (root (treesit-parser-root-node parser))
             (children (rb-ts--collect-root-children root)))
        (should (= 4 (length children)))
        (let ((comment (nth 0 children))
              (defvar (nth 1 children))
              (alpha (nth 2 children))
              (beta (nth 3 children)))
          (should (= (plist-get comment :line) 1))
          (should (string= (plist-get comment :kind) "comment"))
          (should (= (plist-get defvar :line) 3))
          (should (string= (plist-get defvar :kind) "special_form"))
          (should (= (plist-get alpha :line) (plist-get fixture :alpha-line)))
          (should (= (plist-get beta :line) (plist-get fixture :beta-line))))))))

(ert-deftest rb-ts-list-nodes/previews-are-configurable ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture (rb-ts--multi-form-fixture))
          (alpha-text "(defun alpha ()\n  (message \"alpha\"))"))
      (rb-io-write-file path (plist-get fixture :content))
      (let ((default (rb-ts-list-nodes path))
            (wide (rb-ts-list-nodes path 2)))
        (should (= 4 (length default)))
        (should (string= (plist-get (nth 0 default) :preview)
                         ";;; fixture -*- mode: emacs-lisp -*-"))
        (should (string= (plist-get (nth 2 default) :preview)
                         "(defun alpha ()"))
        (should (string= (plist-get (nth 2 wide) :preview)
                         alpha-text))))))

(ert-deftest rb-ts-get-nodes/returns-text-for-requested-lines ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture (rb-ts--multi-form-fixture))
          (alpha-text "(defun alpha ()\n  (message \"alpha\"))"))
      (rb-io-write-file path (plist-get fixture :content))
      (let* ((alpha-line (plist-get fixture :alpha-line))
             (beta-line (plist-get fixture :beta-line))
             (beta-text (plist-get fixture :beta-text))
             (nodes (rb-ts-get-nodes path (list beta-line alpha-line)))
             (first (nth 0 nodes))
             (second (nth 1 nodes)))
        (should (string= (plist-get first :text) beta-text))
        (should (string= (plist-get second :text) alpha-text))
        (should (string= (plist-get first :hash)
                         (secure-hash 'md5 beta-text)))
        (should (string= (plist-get second :hash)
                         (secure-hash 'md5 alpha-text)))
        (should (= (plist-get first :line) beta-line))
        (should (= (plist-get second :line) alpha-line))))))

(ert-deftest rb-ts-get-nodes/missing-line-errors ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture (rb-ts--multi-form-fixture)))
      (rb-io-write-file path (plist-get fixture :content))
      (let ((err (condition-case err
                     (rb-ts-get-nodes path '(999))
                   (error err))))
        (should (string-match "No node starts on line 999"
                              (error-message-string err)))))))

(provide 'rb-ts)
;;; rb-ts.el ends here
