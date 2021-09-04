;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Action Groups VLS templates.

; Create an agroups action template for Vanilla Lisp Shell.
(agroups-add-action-template
 '((action vls)
   (id "Vanilla Lisp Shell")
   (keys "ls")
   (afun (vls-shell (lisp-type lisp-executable elisp-load-file)))
   (slots
    (lisp-type
     ("A unique lisp shell identifying symbol"
      "Lisp shell buffer name will also be derived from this symbol")
     (type symbol))
    (lisp-executable "Lisp executable (shell command) for this lisp")
    (elisp-load-file "Pathname of Elisp load file for this Lisp"))))

; Create an agroups action template for Vanilla Lisp Shell evaluation.
(agroups-add-action-template
 '((action vls-eval)
   (id "Vanilla Lisp Shell: Evaluate a Lisp expression")
   (keys "le")
   (afun (vls-send-lisp-string lisp-expression))
   (slots
    (lisp-expression "A Lisp s-expression for evaluation" (mode lisp-mode)))))
