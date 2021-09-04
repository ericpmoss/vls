;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Allegro Specifics

"./cl.el"

(require 'allegro-vls-elisp "elisp/allegro")

; Mechanism.
((m:more-doc
  ((vls-back-trace
    "ARG > 0 = print ARG stack frames around current.
ARG = C-u = print all significant stack frames.
ARG = 0 = print all stack frames."))))

; Commands.
((c:back-trace
  (cond ((numberp arg) (cond ((> arg 0) (format ":zoom :count %d" arg))
                             (t ":zoom :all t")))
        ((consp arg) ":zoom :count t :all nil :moderate t")
        (t ":zoom")))
 (c:current-frame ":frame")
 (c:down-stack (cond ((consp arg) ":bottom")
                     (t (format ":dn %d" (or arg 1)))))
 (c:exit-lisp "(excl::exit)")
 (c:focus-process
  (cond ((consp arg)
         "(progn
            (format t \"~%Process States:~%\") (tpl:do-command \"processes\")
            (format t \"~2%Process List:\") (pprint mp:*all-processes*)
            (format t \"~2%Current Process:\") (print mp:*current-process*)
            (terpri) (values))")
        ((numberp arg) (format ":focus (nth %d mp:*all-processes*)" arg))
        (t ":focus (car mp:*all-processes*)")))
 (c:help ":help")
 (c:pop-listener (format ":pop %d" arg))
 (c:return-frame (if arg (format ":ret %s" arg) ":return"))
 (c:step-next (format ":scont %d" arg))
 (c:step-over ":sover")
 (c:top-listener (list ":reset" (vls-spec 'c:reset-top-message)))
 (c:up-stack (cond ((consp arg) ":top")
                   (t (format ":up %d" (or arg 1)))))
 (c:var-value (format ":local %s" arg))
 (c:what-error ":error"))

; Variables.
((v:eval-print-separator "\n"))
