;;; rb-rg.el --- RB's interface to ripgrep -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Interface to ripgrep.

;;; Code:

(defun rb-rg (pattern &optional directory extra-args)
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

(provide 'rb-rg)
;;; rb-rg.el ends here
