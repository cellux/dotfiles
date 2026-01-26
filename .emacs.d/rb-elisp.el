;;; rb-elisp.el --- RB's Emacs Lisp tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for Emacs Lisp.

;;; Code:

(defun rb-elisp-eval (form)
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


;;; Unit tests ------------------------------------------------------------
(require 'ert)

(ert-deftest rb-elisp-eval/success ()
  (let ((result (rb-elisp-eval "(+ 1 2)")))
    (should (string= "OK\n3" result))))

(ert-deftest rb-elisp-eval/list-value ()
  (let ((result (rb-elisp-eval "(list :a 1 (cons 2 3))")))
    (should (string= "OK\n(:a 1 (2 . 3))" result))))

(ert-deftest rb-elisp-eval/runtime-error ()
  (let ((result (rb-elisp-eval "(error \"boom\")")))
    (should (string-prefix-p "ERROR\n" result))
    (should (string-match-p "boom" result))))

(ert-deftest rb-elisp-eval/invalid-input ()
  (should-error (rb-elisp-eval nil))
  (should-error (rb-elisp-eval "")))


(provide 'rb-elisp)
;;; rb-elisp.el ends here
