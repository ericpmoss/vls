;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Scheme Specifics - Elisp support code

(provide 'scheme-vls-elisp)

; A given set of instruments for Scheme.
(defvar vls-scheme-instruments
  '(("Breakpoint"
     (format "(cond (#t (error \"Breakpoint %s\")))" (vlsi-def-unique)))
    ("Wrap selected instrument before" 
     (format "(let ()\n%s\n%s)" (vlsi-select-instrument) (vlsi-next-sexp))
     replace-next-sexp)
    ("Wrap selected instrument after" 
     (format "(let ((//vls-val %s))\n%s\n//vls-val)"
       (vlsi-next-sexp) (vlsi-select-instrument))
     replace-next-sexp)
    ("Print variables" (format "(cond (#t (write-line %s)))"
                         (vlsi-get-var-pairs)))))
