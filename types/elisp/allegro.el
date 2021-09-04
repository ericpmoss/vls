;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Allegro Specifics - Elisp support code

(provide 'allegro-vls-elisp)

(define-abbrev-table 'lisp-mode-abbrev-table
    '(
      ("dc" "(tpl:do-command \"\")" uc:back-to-empty-string 0)
      ("wc" "(xref:who-calls ')" uc:backward-char1)
      ))

(defun uc:backward-char1 () (backward-char 1))
(defun uc:back-to-empty-string () (search-backward "\""))

(defvar vls-acl-manuals nil)
