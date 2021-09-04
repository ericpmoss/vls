;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - CMUCL Specifics

"./cl.el"

; Mechanism.
((m:more-doc
  ((vls-back-trace "ARG > 0 = print ARG stack frames down from current."))))

; Commands.
((c:back-trace
  (format "backtrace %s" (if arg (if (consp arg) (car arg) arg) "")))
 (c:current-frame '("P" "L"))
 (c:down-stack (let ((d "down"))
                 (cond ((consp arg) "bottom")
                       (arg (make-list arg d))
                       (t d))))
 (c:exit-lisp "(quit)")
 (c:focus-process
  (cond ((consp arg)
         "(progn
            (format t \"~%Process States:~%\") (mp:show-processes)
            (format t \"~2%Process List:\") (pprint mp:*all-processes*)
            (format t \"~2%Current Process:\") (print mp:*current-process*)
            (terpri) (values))")
        (t "(format t \"~%Don't know how to focus to a process yet\")")))
 (c:help "help")
 (c:pop-listener (format "restart %d" (1- arg)))
 (c:step-next (if arg (format "step %d" arg) "step"))
 (c:step-over "step")
 (c:top-listener (list "abort" (vls-spec 'c:reset-top-message)))
 (c:up-stack (let ((u "up"))
               (cond ((consp arg) "top")
                     (arg (make-list arg u))
                     (t u))))
 (c:var-value (format "(debug:var '%s)" arg))
 (c:what-error "error"))

; Variables.
((v:eval-print-separator "\n"))
