;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - GCL Specifics

"./cl.el"

; Mechanism.
((m:more-doc
  ((vls-back-trace
    "ARG > 0 = print ARG stack frames from top of stack.
ARG = C-u = print short version of stack frames."))))

; Commands.
((c:back-trace (if (consp arg) ":b" (format ":bt %s" arg)))
 (c:current-frame ":bl")
 (c:down-stack (format "(:up %s)" (or (consp arg) arg 1)))
 (c:exit-lisp "(lisp::bye)")
 (c:help ":h")
 (c:pop-listener (format ":q %d" arg))
 (c:step-next "n")
 (c:step-over "s")
 (c:top-listener ":q")
 (c:up-stack (if (consp arg) ":fr" (format "(:down %s)" (or arg 1))))
 (c:var-value (format "(system:vs %s)" arg))
 (c:what-error ":m"))

; Variables.
((v:eval-print-separator "\n"))
