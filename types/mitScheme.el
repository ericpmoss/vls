;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - MIT Scheme Specifics

"./scheme.el"

; Mechanism.
((m:replace-doc
  ((vls-return-frame
    "Return a value from the current stack frame and continue."))))

; Commands.
((c:apropos (format "(apropos '%s)" (car args)))
 (c:back-trace '("(debug)" "H" "Q"))
 (c:compile-file (format "(cf \"%s\")" arg))
 (c:current-frame '("(debug)" "Y" "Q"))
 (c:down-stack '("(debug)" "D" "H" "Q"))
 (c:exit-lisp '("(exit)" "y"))
 (vls-help '("(debug)" "?"))
 (c:pop-listener
  (let ((a "(cmdl-interrupt/abort-previous)"))
    (if (= arg 1) a (make-list arg a))))
 (vls-return-frame "(restart 3)")
 (c:top-listener "(cmdl-interrupt/abort-top-level)")
 (c:up-stack '("(debug)" "U" "H" "Q"))
 (c:what-error '("(debug)" "I" "Q")))

; Probes.
((p:bound-var-test "(environment-bound? user-initial-environment '%s)"))

; Variables.
((v:output-proem-regexp ".*;Value.*:"))


