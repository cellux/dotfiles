;;; rb-fd.el --- RB's interface to fd -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Interface to fd.

;;; Code:

(defun rb-fd (query &optional directory extra-args)
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

(provide 'rb-fd)
;;; rb-fd.el ends here
