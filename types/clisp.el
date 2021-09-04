;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Clisp Specifics

"./cl.el"

(require 'clisp-vls-elisp "elisp/clisp")

; Mechanism.
((m:more-doc
  ((vls-back-trace
    "ARG = N = 1-5 = set mode to mode-N then back trace.
ARG = C-u = display maximum stack frames.
ARG = 0 = display only apply stack frames.")))
 (m:replace-doc
  ((vls-return-frame
    "Return a value from the current stack frame and continue."))))

; Commands.
((c:back-trace (cond ((equal arg 0) ":bt5")
                     ((and (numberp arg) (> arg 0) (< arg 6))
                      (list (format ":m%d" arg) ":bt"))
                     ((consp arg) ":bt1")
                     (t ":bt")))
 (c:current-frame ":w")
 (c:down-stack (let ((u "Up")) (cond ((consp arg) "Top")
                                     (arg (make-list arg u))
                                     (t u))))
 (c:exit-lisp "(ext::bye)")
 (c:help "Help")
 (c:pop-listener (let ((a "Abort")) (if (> arg 1) (make-list arg a) a)))
 (vls-return-frame "Return")
 (c:step-next ":s")
 (c:step-over ":n")
 (c:top-listener (nconc (make-list (clisp-vls-break-count) "Abort")
                        (list (vls-spec 'c:reset-top-message))))
 (c:up-stack (let ((d "Down")) (cond ((consp arg) "Bottom")
                                     (arg (make-list arg d))
                                     (t d))))
 (c:var-value arg)
 (c:what-error "Error"))

; Variables.
((v:eval-print-separator "\n"))
