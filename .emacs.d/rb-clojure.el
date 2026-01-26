;;; rb-clojure.el --- RB's Clojure helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper utilities for Clojure.

;;; Code:

(require 'cider)

(defun rb-clojure-eval (input &optional ns)
  "Evaluate INPUT in the connected Clojure/ClojureScript REPL in namespace NS.
If NS is not given, evaluation happens in the current namespace of the REPL.
The first line of the response is OK if the call succeeded, ERROR if it did not.
The rest of the response contains the evaluation result or the error message."
  (let* ((response (cider-nrepl-sync-request:eval input nil ns)))
    (if (nrepl-dict-contains response "err")
        (format "ERROR\n%s" (nrepl-dict-get response "err"))
      (format "OK\n%s" (nrepl-dict-get response "value")))))

(provide 'rb-clojure)
;;; rb-clojure.el ends here
