;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Guile Specifics

"./scheme.el"

; Commands.
((c:back-trace '("(debug)" "backtrace" "quit"))
 (c:current-frame '("(debug)" "frame" "quit"))
 (c:down-stack '("(debug)" "down" "quit"))
 (c:exit-lisp '("(exit)"))
 (vls-help '("(debug)" "help"))
 (c:top-listener '("quit" "(format #t \"~%Reset to top level~%\")"))
 (c:up-stack '("(debug)" "up" "quit")))
