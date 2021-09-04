;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Lispworks Specifics

"./cl.el"


; Mechanism.
((m:more-doc
  ((vls-back-trace
    "ARG > 0 = print ARG stack frames down from current frame.
ARG = 0 = print all objects found in stack frame.
ARG = C-u = print very detailed version of stack frames."))))

; Commands.
((c:back-trace
  (cond ((numberp arg) (cond ((> arg 0) (format ":b %d" arg))
                             (t ":show")))
        ((consp arg) ":bb")
        (t ":bq")))
 (c:current-frame ":v")
 (c:down-stack (if (consp arg) ":>" (format ":n %d" (or arg 1))))
 (c:exit-lisp "(lispworks::quit)")
 (c:focus-process
  (cond ((consp arg)
         "(progn
            (format t \"~%Process States:~%\") (mp:ps)
            (format t \"~2%Process List:\") (pprint (mp:list-all-processes))
            (format t \"~2%Current Process:\") (print mp:*current-process*)
            (terpri) (values))")
        (t "(format t \"~%Don't know how to focus to a process yet\")")))
 (c:help ":help")
 (c:pop-listener (nconc (make-list arg ":a") (list ":error")))
 (c:return-frame (format ":ret %s" arg))
 (c:step-next (format ":scont %d" arg))
 (c:step-over ":sover")
 (c:top-listener (list ":top" (vls-spec 'c:reset-top-message)))
 (c:up-stack (if (consp arg) ":<" (format ":p %d" (or arg 1))))
 (c:what-error ":error"))

; Variables.
((v:eval-print-separator "\n"))
