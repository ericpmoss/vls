;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Common Lisp Specifics - Elisp support code

(provide 'cl-vls-elisp)

(load "cl-indent")
(setq lisp-indent-function 'common-lisp-indent-function)

; A given set of instruments for Common Lisp.
(defvar vls-cl-instruments
  '(("Breakpoint"
     (format "(cond (t (break \"%s\")))" (vlsi-def-unique)))
    ("Wrap selected instrument before" 
     (format "(progn\n%s\n%s)" (vlsi-select-instrument) (vlsi-next-sexp))
     replace-next-sexp)
    ("Wrap selected instrument after" 
     (format "(prog1\n%s\n%s)" (vlsi-next-sexp) (vlsi-select-instrument))
     replace-next-sexp)
    ("Print variables"
     (format "(cond (t (pprint %s)))" (vlsi-get-var-pairs)))
    ("Capture variables"
     (let ((place (vlsi-gvar-unique "capture"))
           (vars (vlsi-get-var-pairs)))
       (format "(cond (t (unless (boundp '%s) (setq %s nil))
                         (push %s %s)))" place place vars place)))
    ("Count passes"
     (let ((place (vlsi-gvar-unique "count")))
       (format "(cond (t (progn 
                          (unless (boundp '%s) (setq %s 0))
                          (incf %s))))" place place place)))))
