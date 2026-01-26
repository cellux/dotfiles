;;; rb-io.el --- RB's I/O tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for doing I/O.

;;; Code:

(defvar rb-io-read-file-function #'rb-io--default-read-file
  "Function used to read a whole file into a string.
It must accept a PATH argument and return the file contents as a string.")

(defvar rb-io-write-file-function #'rb-io--default-write-file
  "Function used to write a string to PATH.
It must accept PATH and CONTENT arguments.")

(defun rb-io-read-file (path)
  "Read PATH using `rb-tools-io-read-file-function'."
  (funcall rb-io-read-file-function path))

(defun rb-io-write-file (path content)
  "Write CONTENT to PATH using `rb-tools-io-write-file-function'."
  (funcall rb-io-write-file-function path content))

(defun rb-io--default-read-file (path)
  "Default implementation to read PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun rb-io--default-write-file (path content)
  "Default implementation to write CONTENT to PATH."
  (with-temp-file path
    (insert content)))

(provide 'rb-io)
;;; rb-io.el ends here
