;;; rb-tools.el --- RB's helper tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Generic helpers.

;;; Code:

(require 'project)

(defun rb-tools-project-root ()
  "Return the current project root, defaulting to `default-directory'."
  (or (when (and (featurep 'projectile)
                 (fboundp 'projectile-project-root))
        (ignore-errors (projectile-project-root)))
      (when (fboundp 'project-current)
        (when-let* ((proj (project-current nil default-directory)))
          (project-root proj)))
      default-directory))

(defun rb-tools-with-line-numbers (s &optional sep)
  "Return string S with each line prefixed with its line number and SEP.
SEP defaults to : (colon)."
  (unless sep
    (setq sep ":"))
  (with-temp-buffer
    (insert s)
    (let ((lines '())
          (line-number 1))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (push (format "%d%s%s" line-number sep line) lines))
        (forward-line 1)
        (setq line-number (1+ line-number)))
      (let ((result (string-join (nreverse lines) "\n")))
        (when (and (not (string-empty-p result))
                   (eq (char-before (point-max)) ?\n))
          (setq result (concat result "\n")))
        result))))

(defun rb-tools-ensure-keyword (x)
  "Coerces X into a keyword."
  (if (keywordp x)
      x
    (intern (format ":%s" x))))

(defun rb-tools-alist-to-keyword-plist (alist)
  "Return ALIST converted to a plist with keyword keys, preserving order."
  (let ((plist '()))
    (dolist (entry alist)
      (pcase-let ((`(,k . ,v) entry))
        (push (rb-tools-ensure-keyword k) plist)
        (push v plist)))
    (nreverse plist)))

;; Minimal harness helpers for filesystem/buffer isolation ----------------
(defmacro rb-tools-with-temp-dir (dir-sym &rest body)
  "Bind DIR-SYM to a fresh temp directory and evaluate BODY.
The directory is cleaned up on exit."
  (declare (indent 1))
  `(let ((,dir-sym (make-temp-file "rb-tools" t)))
     (unwind-protect
         (progn ,@body)
       (when (and ,dir-sym (file-directory-p ,dir-sym))
         (delete-directory ,dir-sym t)))))

(defmacro rb-tools-with-temp-file (file-sym &rest body)
  "Bind FILE-SYM to a fresh temp file path inside a temp dir and eval BODY.
Cleans up the directory afterwards."
  (declare (indent 1))
  (let ((dir (gensym "dir")))
    `(rb-tools-with-temp-dir ,dir
       (let ((,file-sym (expand-file-name "tmp" ,dir)))
         (write-region "" nil ,file-sym nil 'silent)
         ,@body))))

(provide 'rb-tools)
;;; rb-tools.el ends here
