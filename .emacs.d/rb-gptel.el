;;; rb-gptel.el --- RB's gptel extensions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for gptel.

;;; Code:

(defun rb-gptel-mark-user-response-regions (_info)
  "In current prompt buffer, mark regions between @rb/@ai lines for gptel.

Everything from an @rb line (inclusive of its following text) up to the
next @ai is \"user\" (no gptel response prop).  Everything from an @ai
line up to the next @rb is \"assistant\" (gets the `gptel response'
prop).  Markers are stripped before sending."
  (save-excursion
    ;; Clear old props
    (remove-text-properties (point-min) (point-max)
                            '(gptel nil front-sticky nil))
    (goto-char (point-min))
    (let (segments)
      ;; Collect markers (pos . role)
      (while (re-search-forward "^@\\(rb\\|ai\\)\\s-+" nil t)
        (let ((role (match-string 1))
              (pos (match-beginning 0)))
          (push (cons pos role) segments)))
      (setq segments (nreverse segments))
      ;; Walk segments, apply props to spans
      (while segments
        (pcase-let* ((`(,start-pos . ,role) (pop segments))
                     (end-pos (or (car (car segments)) (point-max))))
          (when (string= role "ai")
            (add-text-properties
             start-pos end-pos
             '(gptel response front-sticky (gptel)))))))))

;;; rb-gptel.el ends here
