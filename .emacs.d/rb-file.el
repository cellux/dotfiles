;;; rb-file.el --- RB's file tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for file editing and management.

;;; Code:

(require 'rb-io)
(require 'rb-rg)
(require 'rb-ts)

(defun rb-file-get-line-ranges (path ranges)
  "Return the exact text for each line range in RANGES from PATH.

RANGES is a list of plists with :start and :end (1-based, inclusive).
Returns a list of plists (:start :end :text :hash) in normalized
order (sorted descending by :start)."
  (unless (file-readable-p path)
    (error "File is not readable: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((total-lines (line-number-at-pos (point-max)))
           (normalized (rb-file--normalize-line-range-specs ranges total-lines nil nil))
           (result '()))
      (dolist (spec normalized)
        (let* ((start (plist-get spec :start))
               (end (plist-get spec :end))
               (text (rb-file--line-range-slice start end)))
          (push (list :start start
                      :end end
                      :text text
                      :hash (secure-hash 'md5 text))
                result)))
      (nreverse result))))

(defun rb-file-update-line-ranges (path ranges)
  "Update the specified line RANGES in PATH with verification.

Each element of RANGES contains the following fields:

- :start integer (1-based inclusive)
- :end integer (1-based inclusive)
- :hash string (MD5 of existing slice)
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
           (normalized (rb-file--normalize-line-range-specs ranges total-lines t t))
           (result '()))
      (dolist (spec normalized)
        (let* ((start (plist-get spec :start))
               (end (plist-get spec :end))
               (new-text (plist-get spec :new_text))
               (expected-hash (plist-get spec :hash))
               (existing (rb-file--line-range-slice start end))
               (existing-hash (secure-hash 'md5 existing)))
          (when (not (string= existing-hash expected-hash))
            (error "Hash mismatch for range %s-%s" start end))
          (pcase-let ((`(,start-pos . ,end-pos) (rb-file--line-range-boundaries start end)))
            (goto-char start-pos)
            (delete-region start-pos end-pos)
            (insert new-text))
          (push (list :start start :end end :updated t) result)))
      (rb-io-write-file path (buffer-string))
      (nreverse result))))

(defun rb-file--line-range-boundaries (start end)
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

(defun rb-file--line-range-slice (start end)
  "Return the exact buffer substring for inclusive lines START..END."
  (pcase-let ((`(,start-pos . ,end-pos) (rb-file--line-range-boundaries start end)))
    (buffer-substring-no-properties start-pos end-pos)))

(defun rb-file--normalize-line-range-specs (ranges total-lines require-new-text require-verification)
  "Validate and normalize line range RANGES for a file with TOTAL-LINES.
Returns the specs sorted descending by :start.  Signals an error on invalid
input (out-of-bounds, overlaps, missing fields).

When REQUIRE-NEW-TEXT is non-nil, each spec must contain a string :new_text.
When REQUIRE-VERIFICATION is non-nil, each spec must provide :hash."
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
             (hash (plist-get spec :hash)))
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
                   (not (plist-member spec :hash)))
          (error "Missing verification for range starting at %s" start))
        (when (and (plist-member spec :hash) (not (stringp hash)))
          (error "Invalid hash for range starting at %s" start))
        (when (and prev-start (>= end prev-start))
          (error "Overlapping ranges %s-%s and %s-%s" start end prev-start prev-end))
        (setq prev-start start
              prev-end end)))
    specs))


;;; Regex match helpers -------------------------------------

(defun rb-file--regex-match--parse-rg-line (line)
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

(defun rb-file--regex-match--closest (matches anchor-line)
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

(defun rb-file--regex-match--matches-for-file (abs-path regex)
  "Return the ripgrep matches for REGEX within ABS-PATH."
  (let* ((dir (file-name-directory abs-path))
         (filename (file-name-nondirectory abs-path)))
    (unless (and dir (not (string-empty-p filename)))
      (error "Invalid file path: %s" abs-path))
    (let* ((extra (format "--column --glob %s" (shell-quote-argument filename)))
           (output (rb-rg regex dir extra)))
      (if (string-empty-p output)
          nil
        (let ((lines (split-string output "\n" t))
              (matches '()))
          (dolist (line lines)
            (when-let ((match (rb-file--regex-match--parse-rg-line line)))
              (unless (string= (file-name-nondirectory (plist-get match :path))
                               filename)
                (error "Ripgrep returned %s when searching %s" (plist-get match :path) abs-path))
              (push match matches)))
          (nreverse matches))))))

(defun rb-file--regex-match--describe (abs-path match &optional anchor-line)
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
      (let ((bounds (rb-file--line-range-boundaries match-line match-line)))
        (list :path abs-path
              :line match-line
              :column (plist-get match :column)
              :match (plist-get match :text)
              :before (car bounds)
              :after (cdr bounds))))))

(defun rb-file--regex-match-info (path regex &optional anchor-line)
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
    (setq matches (rb-file--regex-match--matches-for-file abs-path regex))
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
             (rb-file--regex-match--closest matches anchor-line)))))
      (rb-file--regex-match--describe abs-path selected anchor-line))))


(defun rb-file--regex-insert (path regex new-text position &optional line validate)
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
         (match (rb-file--regex-match-info abs-path regex line))
         (insert-pos (if (eq position :before)
                         (plist-get match :before)
                       (plist-get match :after))))
    (with-temp-buffer
      (insert-file-contents abs-path)
      (goto-char insert-pos)
      (insert new-text)
      (when validate
        (unwind-protect
            (progn
              (setq buffer-file-name abs-path)
              (rb-ts-parse-buffer))
          (set-buffer-modified-p nil)))
      (rb-io-write-file abs-path (buffer-string))
      (list :inserted t))))

(defun rb-file-insert-before-regex (path regex new-text &optional line validate)
  "Insert NEW-TEXT immediately before the line matching REGEX in PATH.

REGEX follows ripgrep's default Rust regex syntax and is restricted to PATH.
When the expression matches multiple lines, supply the 1-based LINE anchor to
pick the occurrence closest to that line.  VALIDATE, when non-nil, re-parses
the edited buffer with Tree-sitter before writing so syntax errors are
reported instead of corrupting the file.  Returns (:inserted t) on success."
  (rb-file--regex-insert path regex new-text :before line validate))

(defun rb-file-insert-after-regex (path regex new-text &optional line validate)
  "Insert NEW-TEXT immediately after the line matching REGEX in PATH.

REGEX follows ripgrep's default Rust regex syntax and is restricted to PATH.
When the expression matches multiple lines, supply the 1-based LINE anchor to
pick the occurrence closest to that line.  VALIDATE, when non-nil, re-parses
the edited buffer with Tree-sitter before writing so syntax errors are
reported instead of corrupting the file.  Returns (:inserted t) on success."
  (rb-file--regex-insert path regex new-text :after line validate))


;; Unit tests --------------------------------------------------------------
(require 'ert)
(require 'cl-lib)

(ert-deftest rb-file-get-line-ranges/basic ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "l1\nl2\nl3\n")
    (let* ((ranges (list (list :start 3 :end 3)
                         (list :start 1 :end 2)))
           (result (rb-file-get-line-ranges path ranges)))
      (should (equal (mapcar (lambda (r) (plist-get r :start)) result)
                     '(3 1)))
      (should (string= (plist-get (nth 0 result) :text) "l3\n"))
      (should (string= (plist-get (nth 1 result) :text) "l1\nl2\n"))
      (should (string= (plist-get (nth 0 result) :hash)
                       (secure-hash 'md5 "l3\n"))))))

(ert-deftest rb-file-update-line-ranges/sorts-and-hash-ok ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "a\nb\nc\n")
    (let* ((hash-a (secure-hash 'md5 "a\n"))
           (hash-c (secure-hash 'md5 "c\n"))
           ;; Intentionally unsorted input; should be applied in descending order.
           (ranges (list (list :start 1 :end 1 :new_text "A\n" :hash hash-a)
                         (list :start 3 :end 3 :new_text "C\n" :hash hash-c))))
      (should (equal (rb-file-update-line-ranges path ranges)
                     '((:start 3 :end 3 :updated t)
                       (:start 1 :end 1 :updated t))))
      (should (string= (rb-io-read-file path) "A\nb\nC\n")))))

(ert-deftest rb-file-update-line-ranges/hash-ok-single ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "x\ny\n")
    (let* ((hash-x (secure-hash 'md5 "x\n"))
           (ranges (list (list :start 1 :end 1 :new_text "X\n" :hash hash-x))))
      (should (equal (rb-file-update-line-ranges path ranges)
                     '((:start 1 :end 1 :updated t))))
      (should (string= (rb-io-read-file path) "X\ny\n")))))

(ert-deftest rb-file-update-line-ranges/two-ranges-hash-ok ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "p\nq\n")
    (let* ((hash-p (secure-hash 'md5 "p\n"))
           (hash-q (secure-hash 'md5 "q\n"))
           (ranges (list (list :start 2 :end 2 :new_text "Q\n" :hash hash-q)
                         (list :start 1 :end 1 :new_text "P\n" :hash hash-p))))
      (should (equal (rb-file-update-line-ranges path ranges)
                     '((:start 2 :end 2 :updated t)
                       (:start 1 :end 1 :updated t))))
      (should (string= (rb-io-read-file path) "P\nQ\n")))))

(ert-deftest rb-file-update-line-ranges/hash-mismatch ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "u\nv\n")
    (let* ((ranges (list (list :start 2 :end 2 :new_text "V\n" :hash "deadbeef"))))
      (should-error (rb-file-update-line-ranges path ranges))
      (should (string= (rb-io-read-file path) "u\nv\n")))))

(ert-deftest rb-file-update-line-ranges/missing-verification-with-old-text ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "p\nq\n")
    (let* ((ranges (list (list :start 1 :end 1 :new_text "P\n" :old_text "p\n"))))
      (should-error (rb-file-update-line-ranges path ranges))
      (should (string= (rb-io-read-file path) "p\nq\n")))))

(ert-deftest rb-file-update-line-ranges/missing-verification ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "m\nn\n")
    (let* ((ranges (list (list :start 1 :end 1 :new_text "M\n"))))
      (should-error (rb-file-update-line-ranges path ranges))
      (should (string= (rb-io-read-file path) "m\nn\n")))))

(ert-deftest rb-file-update-line-ranges/overlap-and-bounds ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "a\nb\nc\n")
    (let* ((hash-c (secure-hash 'md5 "c\n"))
           (hash-bc (secure-hash 'md5 "b\nc\n")))
      ;; Overlapping ranges rejected
      (should-error (rb-file-update-line-ranges
                     path
                     (list (list :start 3 :end 3 :new_text "Z\n" :hash hash-c)
                           (list :start 2 :end 3 :new_text "Y\n" :hash hash-bc))))
      ;; Out-of-bounds rejected
      (should-error (rb-file-update-line-ranges
                     path
                     (list (list :start 5 :end 5 :new_text "oops" :hash "dummy"))))
      (should (string= (rb-io-read-file path) "a\nb\nc\n")))))

(ert-deftest rb-file-update-line-ranges/all-or-nothing ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "one\ntwo\nthree\n")
    (let* ((hash-two (secure-hash 'md5 "two\n"))
           (ranges (list (list :start 2 :end 2 :new_text "TWO\n" :hash hash-two)
                         (list :start 1 :end 1 :new_text "ONE\n" :hash "bad"))))
      (should-error (rb-file-update-line-ranges path ranges))
      (should (string= (rb-io-read-file path) "one\ntwo\nthree\n")))))

(ert-deftest rb-file-update-line-ranges/empty-new-text ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "keep\nremove\n")
    (let* ((hash-remove (secure-hash 'md5 "remove\n"))
           (ranges (list (list :start 2 :end 2 :new_text "" :hash hash-remove))))
      (should (equal (rb-file-update-line-ranges path ranges)
                     '((:start 2 :end 2 :updated t))))
      (should (string= (rb-io-read-file path) "keep\n")))))

(ert-deftest rb-file-update-line-ranges/multi-range-hash-only ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "l1\nl2\nl3\n")
    (let* ((hash-l3 (secure-hash 'md5 "l3\n"))
           (hash-l1 (secure-hash 'md5 "l1\n"))
           (ranges (list (list :start 3 :end 3 :new_text "L3\n" :hash hash-l3)
                         (list :start 1 :end 1 :new_text "L1\n" :hash hash-l1))))
      (should (equal (rb-file-update-line-ranges path ranges)
                     '((:start 3 :end 3 :updated t)
                       (:start 1 :end 1 :updated t))))
      (should (string= (rb-io-read-file path) "L1\nl2\nL3\n")))))

(ert-deftest rb-file-insert-before-regex/basic ()
  (rb-tools-with-temp-file path
    (let* ((fixture ";;; fixture\n\n(defvar foo 1)\n(defvar bar 2)\n")
           (expected ";;; fixture\n\n;; inserted\n(defvar foo 1)\n(defvar bar 2)\n"))
      (rb-io-write-file path fixture)
      (rb-file-insert-before-regex path "\\(defvar foo" ";; inserted\n")
      (should (string= (rb-io-read-file path) expected)))))

(ert-deftest rb-file-insert-before-regex/basic-2 ()
  "Inserting before a unique marker should succeed."
  (rb-tools-with-temp-file path
    (let* ((fixture ";;; fixture\nmarker\nline\n")
           (expected ";;; fixture\n;; before marker\nmarker\nline\n"))
      (rb-io-write-file path fixture)
      (rb-file-insert-before-regex path "marker" ";; before marker\n")
      (should (string= (rb-io-read-file path) expected)))))

(ert-deftest rb-file-insert-before-regex/anchor-line ()
  "Using an anchor line selects the closest match for inserting before."
  (rb-tools-with-temp-file path
    (let* ((fixture "alpha\nmarker\nbeta\nmarker\ngamma\n")
           (expected "alpha\nmarker\nbeta\n;; inserted near second marker\nmarker\ngamma\n"))
      (rb-io-write-file path fixture)
      (rb-file-insert-before-regex path "marker" ";; inserted near second marker\n" 5)
      (should (string= (rb-io-read-file path) expected)))))

(ert-deftest rb-file-insert-before-regex/missing-match ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path ";; nothing\n")
    (should-error (rb-file-insert-before-regex path "absent" ";; missing\n"))
    (should (string= (rb-io-read-file path) ";; nothing\n"))))

(ert-deftest rb-file-insert-before-regex/missing-match-2 ()
  "Missing regex matches must error and leave the file untouched."
  (rb-tools-with-temp-file path
    (let ((fixture "alpha\nbeta\n"))
      (rb-io-write-file path fixture)
      (should-error (rb-file-insert-before-regex path "missing" ";; inserted\n"))
      (should (string= (rb-io-read-file path) fixture)))))

(ert-deftest rb-file-insert-after-regex/basic ()
  "Inserting after a unique marker should succeed."
  (rb-tools-with-temp-file path
    (let* ((fixture "marker\nline\n")
           (expected "marker\n;; after marker\nline\n"))
      (rb-io-write-file path fixture)
      (rb-file-insert-after-regex path "marker" ";; after marker\n")
      (should (string= (rb-io-read-file path) expected)))))

(ert-deftest rb-file-insert-after-regex/anchor-line ()
  (rb-tools-with-temp-file path
    (let* ((fixture ";;; fixture\nmarker\nalpha\nmarker\nomega\n")
           (expected ";;; fixture\nmarker\nalpha\nmarker\n;; after second\nomega\n"))
      (rb-io-write-file path fixture)
      (rb-file-insert-after-regex path "marker" ";; after second\n" 4)
      (should (string= (rb-io-read-file path) expected)))))

(ert-deftest rb-file-insert-after-regex/anchor-line-2 ()
  "Using an anchor line selects the closest match for inserting after."
  (rb-tools-with-temp-file path
    (let* ((fixture "alpha\nmarker\nbeta\nmarker\ngamma\n")
           (expected "alpha\nmarker\nbeta\nmarker\n;; after second marker\ngamma\n"))
      (rb-io-write-file path fixture)
      (rb-file-insert-after-regex path "marker" ";; after second marker\n" 5)
      (should (string= (rb-io-read-file path) expected)))))

(ert-deftest rb-file-insert-after-regex/ambiguous-match ()
  (rb-tools-with-temp-file path
    (let ((fixture "marker\nmarker\n"))
      (rb-io-write-file path fixture)
      (should-error (rb-file-insert-after-regex path "marker" ";; fail\n"))
      (should (string= (rb-io-read-file path) fixture)))))

(ert-deftest rb-file-insert-after-regex/validate-detection ()
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture "(defun foo ()\n  (message \"hi\"))\n"))
      (rb-io-write-file path fixture)
      (should-error (rb-file-insert-after-regex path "\\(defun foo" "(\n" nil t))
      (should (string= (rb-io-read-file path) fixture)))))

(ert-deftest rb-file-insert-after-regex/rust-regex-syntax ()
  "Rust-flavored regexes such as inline flags should work through ripgrep."
  (rb-tools-with-temp-file path
    (let* ((fixture "hello\nHELLO\n")
           (expected "hello\nHELLO\n;; inserted after uppercase\n"))
      (rb-io-write-file path fixture)
      (rb-file-insert-after-regex path "(?i)hello" ";; inserted after uppercase\n" 2)
      (should (string= (rb-io-read-file path) expected)))))

(ert-deftest rb-file-test-insert-after-regex/ambiguous-match ()
  "Ambiguous regex matches must error and leave the file untouched."
  (rb-tools-with-temp-file path
    (let ((fixture "marker\nmarker\n"))
      (rb-io-write-file path fixture)
      (should-error (rb-file-insert-after-regex path "marker" ";; inserted\n"))
      (should (string= (rb-io-read-file path) fixture)))))

(ert-deftest rb-file-test-insert-after-regex/validate-failure ()
  "Validation prevents writing corrupted files when :validate is true."
  (skip-unless (treesit-ready-p 'elisp t))
  (rb-tools-with-temp-file path
    (let ((fixture "(defun foo ()\n  (message \"hi\"))\n"))
      (rb-io-write-file path fixture)
      (should-error (rb-file-insert-after-regex path "\\(defun foo" "(\n" nil t))
      (should (string= (rb-io-read-file path) fixture)))))


(ert-deftest rb-file--line-range-slice/middle-range ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((bounds (rb-file--line-range-boundaries 2 3)))
      (should (= (car bounds)
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line 1)
                   (point))))
      (should (= (cdr bounds) (point-max)))
      (should (string= (rb-file--line-range-slice 2 3)
                       "beta\ngamma\n")))))

(ert-deftest rb-file-get-line-ranges/overlapping-range-error ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "alpha\nbeta\n")
    (should-error (rb-file-get-line-ranges
                   path
                   (list (list :start 1 :end 2)
                         (list :start 2 :end 2))))))

(ert-deftest rb-file--normalize-line-range-specs/invalid-start ()
  (should-error (rb-file--normalize-line-range-specs
                 (list (list :start 0 :end 1 :new_text "x\n" :hash "h"))
                 1 t t)))

(ert-deftest rb-file--normalize-line-range-specs/end-beyond-total-lines ()
  (should-error (rb-file--normalize-line-range-specs
                 (list (list :start 1 :end 5 :new_text "x\n" :hash "h"))
                 3 t t)))

(ert-deftest rb-file--regex-match--matches-for-file/empty-output ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "one\n")
    (cl-letf (((symbol-function 'rb-rg)
               (lambda (&rest _)
                 "")))
      (should (null (rb-file--regex-match--matches-for-file path "one"))))))

(ert-deftest rb-file--regex-match--describe/out-of-bounds-line ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "a\nb\n")
    (should-error (rb-file--regex-match--describe
                   path
                   (list :line 0 :column 1 :text "a")))))

(ert-deftest rb-file--regex-match-info/path-required ()
  (should-error (rb-file--regex-match-info "" "pattern")))

(ert-deftest rb-file--regex-match-info/regex-required ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "x\n")
    (should-error (rb-file--regex-match-info path nil))))

(ert-deftest rb-file--regex-insert/invalid-position ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "foo\n")
    (should-error (rb-file--regex-insert path "foo" "bar\n" :invalid))))

(ert-deftest rb-file--regex-insert/new-text-must-be-string ()
  (rb-tools-with-temp-file path
    (rb-io-write-file path "foo\n")
    (should-error (rb-file--regex-insert path "foo" nil :before))))

(ert-deftest rb-file-insert-after-regex/validate-success ()
  (rb-tools-with-temp-file path
    (let ((fixture "(defun moxie ()\n  (message \"moose\"))\n")
          (validated nil))
      (rb-io-write-file path fixture)
      (cl-letf (((symbol-function 'rb-ts-parse-buffer)
                 (lambda ()
                   (setq validated t))))
        (rb-file-insert-after-regex path "\\(defun moxie" ";; inserted\n" nil t)
        (should validated)
        (should (string-match-p ";; inserted" (rb-io-read-file path)))))))

(provide 'rb-file)
;;; rb-file.el ends here
