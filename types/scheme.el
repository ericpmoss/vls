;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell - Scheme Specifics

(require 'scheme-vls-elisp "elisp/scheme")

; Mechanism.
((m:replace-doc
  ((vls-help
    "Enter debugger, print a list of commands, and stay in debugger."))))

; Commands.
((c:load-file (format "(load \"%s\")" arg)))

; Variables.
((v:end-expression "\n")
 (v:eval-print-separator "\n")
 (v:global-debug-variable-prefix "/")
 (v:instruments vls-scheme-instruments)
 (v:instrument-visual "\n;; *Instrument* \n%s\n\n"))
