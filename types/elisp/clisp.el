;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Clisp Specifics - Elisp support code

(provide 'clisp-vls-elisp)

; Return the system break count.
(defun clisp-vls-break-count ()
  (vls-lisp-evalue "system::*break-count*"))
