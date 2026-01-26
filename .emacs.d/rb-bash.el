;;; rb-bash.el --- RB's Bash tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for Bash.

;;; Code:

(defun rb-bash-eval (script)
  "Execute SCRIPT via Bash and return a plist describing the result.
SCRIPT must be a non-empty string.  The script is executed through `bash
-c' and stdout/stderr are captured.  The returned plist contains
:exit_code, :stdout, and :stderr."
  (unless (and (stringp script) (not (string-empty-p script)))
    (error "Script string is required"))
  (let* ((bash (or (executable-find "bash")
                   (error "Executable not found: bash")))
         (stdout (generate-new-buffer "*rb-bash-eval-stdout*"))
         (stderr (generate-new-buffer "*rb-bash-eval-stderr*")))
    (unwind-protect
        (let* ((process (make-process :name "rb-bash-eval"
                                      :buffer stdout
                                      :command (list bash "-c" script)
                                      :noquery t
                                      :stderr stderr
                                      :sentinel (lambda (_proc _event) nil)))
               (stderr-proc (get-buffer-process stderr)))
          (when (processp stderr-proc)
            (set-process-sentinel stderr-proc (lambda (_p _e) nil)))
          (while (process-live-p process)
            (accept-process-output process))
          (when (processp stderr-proc)
            (while (process-live-p stderr-proc)
              (accept-process-output stderr-proc)))
          (let* ((exit-code (process-exit-status process))
                 (stdout-str (with-current-buffer stdout (buffer-string)))
                 (stderr-str (with-current-buffer stderr (buffer-string))))
            (list :exit_code exit-code
                  :stdout stdout-str
                  :stderr stderr-str)))
      (dolist (buffer (list stdout stderr))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))


;;; Unit tests ------------------------------------------------------------
(require 'ert)

(ert-deftest rb-bash-eval/success ()
  (let ((result (rb-bash-eval "printf 'ok\n'; printf 'err\n' >&2")))
    (should (= 0 (plist-get result :exit_code)))
    (should (string= "ok\n" (plist-get result :stdout)))
    (should (string= "err\n" (plist-get result :stderr)))))

(ert-deftest rb-bash-eval/failure ()
  (let ((result (rb-bash-eval "printf 'warn\n' >&2; exit 42")))
    (should (= 42 (plist-get result :exit_code)))
    (should (string= "" (plist-get result :stdout)))
    (should (string= "warn\n" (plist-get result :stderr)))))

(ert-deftest rb-bash-eval/invalid-script ()
  (should-error (rb-bash-eval ""))
  (should-error (rb-bash-eval nil)))


(provide 'rb-bash)
;;; rb-bash.el ends here
