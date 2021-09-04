;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Common Lisp Specifics

(require 'cl-vls-elisp "elisp/cl")

; Commands.
((c:apropos (format "(apropos '%s)" (car args)))
 (c:compile (format "(compile '%s)" arg))
 (c:compile-file (format "(compile-file \"%s\" :verbose t :print t)" arg))
 (c:describe (format "(describe '%s)" (car args)))
 (c:in-package (format "(in-package %s)" arg))
 (c:load-file (format "(load \"%s\")" arg))
 (c:load-file-print (format "(load \"%s\" :print t)" arg))
 (c:message (format "(progn (format t  \"%s\") (values))" arg))
 (c:reset-top-message "(progn (format t \"Reset to top level\") (values))")
 (c:var-print (format "(pprint %s)" arg))
 (c:var-reset
  (format 
    "(let ((sym '%s))
       (setf (symbol-value sym)
             (if (boundp sym)
                 (cond ((numberp (symbol-value sym)) 0)
                       ((stringp (symbol-value sym)) \"\"))))
       (format t \"Variable ~s reset to: ~s\" sym (symbol-value sym))
       (values))"
    arg))
 (c:what-package
  "(progn (format t \"~%Current package: ~a\"
          (package-name *package*))
          (values))"))

; Probes.
((p:bound-var-test "(boundp '%s)")
 (p:current-package "(package-name *package*)")
 (p:package-predicate "(if (find-package '%s) t)"))

; Variables.
((v:has-packages t)
 (v:in-package-re "(in-package\\s-")
 (v:end-expression "\n")
 (v:global-debug-variable-prefix "/")
 (v:instruments vls-cl-instruments)
 (v:instrument-visual "\n;; *Instrument* \n%s\n\n"))
