;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vanilla Lisp Shell

(provide 'vls)
(require 'comint) (require 'lisp-mode)

(defconst *vls-version* "1.2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Customization variables.

(defcustom vls-compile-instrumented nil
  "After instrumenting a definition, nil means don't ask don't compile, t
means don't ask just compile, and 'ask means ask to compile")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VLS modes and mode maps.

(defcustom vls-modes '(lisp-mode) "All potential modes used by VLS")

(defvar vls:*modes*
  (if (member 'vls-mode vls-modes) vls-modes (cons 'vls-mode vls-modes)))

(defvar vls-mode-map
  (let ((shared-map-var (if (boundp 'shared-lisp-mode-map)
                            'shared-lisp-mode-map 'lisp-mode-shared-map)))
    (copy-keymap (eval shared-map-var))))

; Return the standard mode map symbol of a given mode symbol.
(defun vls:mode-map (mode) (intern (concat (symbol-name mode) "-map")))

(defvar vls:*mode-maps* (mapcar 'vls:mode-map vls:*modes*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utility functions.

; Add and merge a list of new associations onto a given association list.
(defun vls:add-assocs (alist new)
  (while new
    (let* ((item (car new)) (exist (assoc (car item) alist)))
      (if exist (setcdr exist (cdr item))
        (setq alist (nconc alist (list item))))
      (setq new (cdr new))))
  alist)

; Returns the position of given char in given string if found or NIL.
(defun vls:char-pos (char string)
  (catch 'found
    (let ((i 0) (max (length string)))
      (while (< i max)
        (if (= char (aref string i)) (throw 'found i) (setq i (1+ i)))))))

; Predicate: Is string an all whitepace string?
(defun vls:white-string (string) (not (string-match "[^ \n\t\f]" string)))

; Return the next buffer string to end of buffer.
(defun vls:next-buffer-string () (buffer-substring (point) (point-max)))

; Return the next s-expression length
(defun vls:next-sexp-length ()
  (save-excursion (let ((start (point))) (forward-sexp 1) (- (point) start))))

; Return the inner-most s-expression that point is on.
(defun vls:point-sexpr ()
  (let (end)
    (save-excursion
      (forward-sexp) (setq end (point)) (backward-sexp)
      (buffer-substring (point) end))))

; Inserts the given string at the front of the kill ring unless the given
; string is equal to the string at the front. DISPLAY arg means display string.
(defun vls:string-to-kill-ring (string &optional display)
  (kill-new string)
  (if display (message "Top of kill ring = %s" string)))

; Return the real file name of a file name with possible special characters.
; If DIRECTORY is given merges the DIRECTORY FILE pair.
(defun vls:real-file-name (file &optional directory)
  (expand-file-name (substitute-in-file-name file)
                    (if directory (expand-file-name directory))))

; Acts on the end of the lisp buffer in another window
(defun vls:with-lisp-buffer-end-other-window (&optional dont-mark-output)
  (vls:with-lisp-buffer-other-window)
  (cond ((not (vls:lisp-buffer-p))
         (goto-char (point-max))
         (recenter 1)))
  (set-marker (process-mark (get-buffer-process (vls-spec 'buffer)))
              (point-max))
  (if (not dont-mark-output)
      (vls:put-spec 'begin-of-last-output (point-max))))

; Acts on the lisp buffer in another window.
(defun vls:with-lisp-buffer-other-window ()
  (vls:put 'source-buffer (current-buffer))
  (let ((lisp-buffer (vls-spec 'buffer)))
    (if (not (eq (current-buffer) lisp-buffer))
        (switch-to-buffer-other-window lisp-buffer))))

; Done acting on lisp buffer other window.
(defun vls:done-with-lisp-buffer-other-window ()
  (let ((source-buffer (vls:get 'source-buffer))
        (lisp-buffer (vls-spec 'buffer)))
    (goto-char (process-mark (get-buffer-process lisp-buffer)))
    (if (not (eq source-buffer lisp-buffer))
        (switch-to-buffer-other-window source-buffer))))

; Predicate: Is current buffer current Lisp buffer?
(defun vls:lisp-buffer-p () (eq (current-buffer) (vls-spec 'buffer)))

; Return the current buffer's file or if none prompts for one.
(defun vls:buffer-file ()
  (let ((file (buffer-file-name (current-buffer))))
    (or file (read-file-name "Filename: "))))

; Predicate: Is point in a Lisp comment?
(defun vls:point-in-comment ())

; Send a string as a readable message to the Lisp process.
(defun vls:lisp-message (string) (vls:do-lisp-command 'c:message string))

; Is given variable bound in current Lisp?
(defun vls:boundp (variable)
  (vls:lisp-evalue-p (format (vls-spec 'p:bound-var-test) variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lisp shell accessors.

; Accessing top level VLS attributes.
(defun vls:get (attribute) (get 'vls:get attribute))
(defun vls:put (attribute value) (put 'vls:get attribute value))

; Accessing the collection of all Lisp shell specifics.
(defun vls:get-specs (type) (cadr (assoc type (vls:get 'specs))))
(defun vls:put-specs (type specs)
  (let ((exist (assoc type (vls:get 'specs))))
    (if exist (setcar (cdr exist) specs)
      (vls:put 'specs (nconc (list (list type specs)) (vls:get 'specs))))))

; Accessing Lisp shell specific of given type and spec attribute.
(defun vls:type-spec (type spec) (cadr (assoc spec (vls:get-specs type))))

; Accessing the current Lisp shell specifics.
(defun vls-spec (spec) (cadr (assoc spec (vls:get 'current-specs))))
(defun vls:put-spec (spec value)
  (let* ((current-specs (vls:get 'current-specs))
         (exist (assoc spec current-specs)))
    (if exist (setcar (cdr exist) value)
      (vls:put 'current-specs
               (nconc current-specs (list (list spec value)))))))

; Predicate: Is current buffer a VLS shell buffer?
(defun vls:shell-p () (eq major-mode 'vls-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Handling Lisp shell specifics.

; Load the file of specs for a specific Lisp type and return the specs.
(defun vls:load-specific-lisp-type (type type-file)
  (let* (specs (file (vls:real-file-name type-file))
               (default-directory (file-name-directory file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max)) (insert (symbol-name nil))
      (goto-char (point-min))
      (catch 'done
        (while t
          (let ((expr (read (current-buffer))))
            (if expr
                (if (and (consp expr) (symbolp (car expr))) (eval expr)
                  (setq specs
                        (nconc specs
                               (if (stringp expr)
                                   (vls:load-specific-lisp-type type expr)
                                 expr))))
              (throw 'done specs))))))))

; Set up the a current Lisp shell given requested specifics.
(defun vls:set-shell-specs (specifics)
  (let ((type (car specifics)) (file (nth 2 specifics)))
    (let ((load-path (cons (substitute-in-file-name (file-name-directory file))
                           load-path)))
      (vls:put-specs
       type (vls:add-assocs
             (list (list 'buffer nil)
                   (list 'process-name (symbol-name type))
                   (list 'probe-output nil)
                   (list 'm:replace-doc nil)
                   (list 'm:more-doc nil)
                   (list 'type (car specifics))
                   (list 'idefs nil)
                   (list 'executable (cadr specifics))
                   (list 'specifics specifics))
             (vls:load-specific-lisp-type type file))))
    (vls:put 'current-specs (vls:get-specs type))))

; Return the Lisp shell specifics associated with current buffer or nil.
(defun vls:current-buffer-specs ()
  (let ((current (current-buffer)) (specs (vls:get 'specs)))
    (catch 'search
      (while specs
        (let* ((spec (car specs))
               (buffer (cadr (assoc 'buffer (cadr spec)))))
          (if (and buffer (eq buffer current)) (throw 'search spec)
            (setq specs (cdr specs))))))))

; Return the Lisp buffer of given type or NIL under all conditions.
(defun vls:lisp-type-buffer (type)
  (let ((buffer (vls:type-spec type 'buffer)))
    (if (and buffer (buffer-name buffer)) buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VLS mode.

(defun vls-mode ()
  "Major mode for Vanilla Lisp Shell.
Commands:
\\{vls-mode-map}"
  (kill-all-local-variables)
  (comint-mode)
  (use-local-map vls-mode-map)
  (setq major-mode 'vls-mode)
  (setq mode-name "VLS")
  (lisp-mode-variables t)
  (set-syntax-table lisp-mode-syntax-table)
  (run-hooks 'vls-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Run a Lisp shell.

; Run a Lisp shell based on given specifics.  Specifics is a list of the
; form (<lisp-type> <lisp executable> <specifics file>).
(defun vls-shell (specifics)
  (let ((lisp-buffer (vls:lisp-type-buffer (car specifics))))
    (cond (lisp-buffer (switch-to-buffer lisp-buffer))
          (t (vls:set-shell-specs specifics)
             (vls:put-spec 'input-ring nil)
             (vls:put-spec 'begin-of-last-output 1)
             (switch-to-buffer
              (make-comint (vls-spec 'process-name) (vls-spec 'executable)))
             (vls:put-spec 'buffer (current-buffer))
             (add-hook 'comint-preoutput-filter-functions 'vls:output-hook)
             (vls-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sending strings to the current Lisp process.

(defcustom vls-input-ring-size 16 "Maximum size of a VLS input ring")

; Push the last input to the input stack.
(defun vls:push-input (input)
  (let* ((stack (cons input (vls-spec 'input-ring))) (new stack) (count 2))
    (while stack (cond ((> count vls-input-ring-size)
                        (setcdr stack nil) (setq stack nil))
                       (t (setq stack (cdr stack) count (1+ count)))))
    (vls:put-spec 'input-ring new)))

; Yank an input from the input ring front or next if repeat is non-nil.
(defun vls:yank-an-input (repeat)
  (let ((stack (vls-spec 'input-ring)))
    (if stack
        (let ((begin (point))
              (last (or (cdr (get 'vls:yank-an-input 'last)) stack)))
          (cond (repeat (delete-region (get 'vls:yank-an-input 'begin)
                                       (min (get 'vls:yank-an-input 'end)
                                            (point-max)))
                        (setq begin (point))
                        (insert (car last)))
                (t (insert (car stack)) (setq last stack)))
          (put 'vls:yank-an-input 'begin begin)
          (put 'vls:yank-an-input 'end (point))
          (put 'vls:yank-an-input 'last last))
      (message "No last input!"))))

; Send a string to the current lisp process.
(defun vls-send-lisp-string (string)
  (if (not (equal (vls-spec 'probe-output) t))
      (vls:put-spec 'probe-output 'wait))
  (let ((process (vls-spec 'process-name)))
    (process-send-string process string)
    (process-send-string process (vls-spec 'v:end-expression))))

; Send a string or strings to the Lisp process.
(defun vls:send-lisp-strings (stringc)
  (if (consp stringc) (while stringc
                        (vls-send-lisp-string (car stringc))
                        (setq stringc (cdr stringc)))
    (vls-send-lisp-string stringc)))

; Send given lisp string and do standard buffer niceties.
(defun vls:do-lisp-string (string)
  (vls:with-lisp-buffer-other-window)
  (vls-send-lisp-string string)
  (vls:done-with-lisp-buffer-other-window))

; This is a generic function for translating a vanilla Lisp command to a
; specific Lisp command and sending to a lisp process.  The argument
; "command" determines the type of command to send and "arg" is an argument
; that may be used by that command.
(defun vls:send-lisp-command (command arg)
  (let ((stringc (let ((expr (vls-spec command)))
                   (if expr (if (stringp expr) expr (eval expr))))))
    (if (not (equal stringc 'no-command))
        (if stringc (vls:send-lisp-strings stringc)
          (message "Did not find a command string for: %s" command)))))

; Send given lisp command with argument and do standard buffer niceties.
(defun vls:do-lisp-command (command &optional arg &rest args)
  (vls:with-lisp-buffer-other-window)
  (vls:send-lisp-command command arg)
  (vls:done-with-lisp-buffer-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Probing the current Lisp process.

; VLS output hook only fires on certain conditions.
(defun vls:output-hook (string)
  (cond ((equal (vls-spec 'probe-output) t)
         (vls:put-spec 'probe-output string) "")
        (t (vls:put-spec 'probe-output nil) string)))

; Probe current Lisp with given string and return the value as Elisp.
(defun vls:probe-lisp (string)
  (while (equal (vls-spec 'probe-output) 'wait) (sleep-for 0.001))
  (sleep-for 0.001)
  (vls:put-spec 'probe-output t)
  (vls-send-lisp-string string)
  (while (equal (vls-spec 'probe-output) t) (sleep-for 0.001))
  (vls-spec 'probe-output))

; Return string stripped of output proem if exists for current Lisp.
(defun vls:strip-output-proem (string)
  (let ((proem (vls-spec 'v:output-proem-regexp)))
    (cond (proem (string-match proem string)
                 (substring string (match-end 0)))
          (t string))))

; Probe current Lisp with given string and return the value as Elisp.
(defun vls-lisp-evalue (string)
  (car (read-from-string (vls:strip-output-proem (vls:probe-lisp string)))))

; Return an Elisp predicate from probe-lisp return string.
(defun vls:lisp-evalue-p (string)
  (car (read-from-string
        (downcase (vls:strip-output-proem (vls:probe-lisp string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Common command execution.

; Evaluate a region in current buffer or lisp buffer.
(defun vls:eval-region (begin end)
  (vls:with-lisp-buffer-end-other-window)
  (vls:done-with-lisp-buffer-other-window)
  (vls:push-input (buffer-substring begin end))
  (let ((process-name (vls-spec 'process-name)))
    (process-send-region process-name (min begin end) (max begin end))
    (process-send-string process-name (vls-spec 'v:end-expression)))
  (if (vls:lisp-buffer-p) (goto-char (point-max))))

; Load a file of given type.
(defun vls:load-file (file-name load-type)
  (vls:with-lisp-buffer-end-other-window t)
  (if (consp load-type)
      (vls:send-lisp-command 'c:load-file-print file-name)
    (vls:send-lisp-command 'c:load-file file-name))
  (vls:done-with-lisp-buffer-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display utility functions.

(defconst vls:*buffer* "*vls*")

; Show given sexp string with a header.
(defun vls:show-sexp (header sexp-string)
  (save-current-buffer
    (with-output-to-temp-buffer vls:*buffer*
      (set-buffer vls:*buffer*)
      (princ header) (princ sexp-string)
      (lisp-mode) (backward-sexp) (indent-sexp))))

; Display a given list numbered with given header.
(defun vls:show-list-numbered (list header &optional access)
  (with-output-to-temp-buffer vls:*buffer*
    (buffer-disable-undo standard-output)
    (princ header) (terpri)
    (let ((n 0))
      (while list
        (let ((x (funcall (or access 'identity) (car list))))
          (princ " ") (princ n) (princ " = ")
          (princ (if (consp x) (car x) x)) (terpri)
          (cond ((consp x)
                 (setq x (cdr x))
                 (while x
                   (princ "     ") (if (> n 9) (princ " "))
                   (princ (car x)) (terpri) (setq x (cdr x))))))
        (setq n (1+ n) list (cdr list))))))

; Show that point is not contained in a definition.
(defun vls:show-not-defining-form ()
  (message "Point is not in a definition!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Choosing items from lists.

; An interactive function for aborting VLS operations.
(defun vls:abort (&optional message)
  (interactive)
  (message (or message "Aborted VLS operation!"))
  (throw 'vls:condition nil))

; Define a minibuffer key map for single key entries.
(defconst vls:*minibuffer-map*
  (let ((keymap (make-sparse-keymap)) (c-g (string-to-char "\C-g")) (char 0))
    (while (< char 128)
      (define-key keymap (char-to-string char)
        (if (eq char c-g) 'vls:abort 'self-insert-and-exit))
      (setq char (1+ char)))
    keymap))

; Define a prompt user for string keymap with vls:abort for C-g.
(defconst vls:*minibuffer-string-map*
    (let ((map (copy-keymap minibuffer-local-map)))
      (define-key map "\C-g" 'vls:abort)
      map))

; Prompt the user for an id string and return the entered string or NIL.
(defun vls:prompt-user (prompt &optional initial)
  (let* ((minibuffer-local-map vls:*minibuffer-string-map*)
         (string (read-string prompt initial)))
    (if (string= string "") nil string)))

; Returns digit when User enters a single digit in 0-9.
(defun vls:enter-0-9 (prompt)
  (catch 'result
    (while t
      (let ((digit (read-from-minibuffer prompt nil vls:*minibuffer-map*)))
        (cond ((vls:char-pos (aref digit 0) "0123456789") (throw 'result digit))
              ((string= digit "\C-m") (throw 'result nil)))))))

; Prompt the user for a number between 0 and less than given max.
(defun vls:prompt-user-numeric (prompt max default)
  (let (found)
    (while (not found)
      (let* ((choice (if (> max 10)
                         (vls:prompt-user prompt) (vls:enter-0-9 prompt)))
             (n (if choice (car (read-from-string choice)) (or default 0))))
        (if (and (numberp n) (>= n 0) (< n max)) (setq found n))))
    found))

; User chooses an index into a given list, list header and prompt.
(defun vls:choose-index (list header prompt &optional access default)
  (vls:show-list-numbered list header access)
  (vls:prompt-user-numeric prompt (length list) default))

; User chooses an item from given list, list header and prompt.
(defun vls:choose-item (list header prompt &optional access default)
  (nth (vls:choose-index list header prompt access default) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up proper indentation for rest (non-elisp) of Common Lisp functions.

(mapcar
 (function (lambda (symbol) (put symbol 'common-lisp-indent-function 1)))
 '(format))

(mapcar
 (function (lambda (symbol) (put symbol 'common-lisp-indent-function 2)))
 '(if))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pre-checks for unusual conditions before executing VLS commands.

; If current shell buffer is not current Lisp shell, make it so.
(defun vls:check-current-current ()
  (if (not (eq (vls-spec 'buffer) (current-buffer)))
      (let ((specs (vls:current-buffer-specs)))
        (if specs (vls:put 'current-specs (cadr specs))))))

; If current Lisp shell process no longer exists, abort with error.
(defun vls:pre-check ()
  (vls:check-current-current)
  (let ((process-name (vls-spec 'process-name)))
    (cond ((not process-name) (error "No current Lisp process exists!"))
          ((not (get-process process-name))
           (error "Lisp shell process no longer exists: %s" process-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Deal with Lisp specific type commands that want to be totally specific.

; Execute a totally specific Lisp type command.
(defun vls:totally-specific (command arg)
  (vls:with-lisp-buffer-end-other-window)
  (vls:send-lisp-command command arg)
  (vls:done-with-lisp-buffer-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Advise documentation to add specific vls command help if any.

(defadvice documentation (after vls:documentation last (function) activate)
  (cond
    ((member major-mode vls:*modes*)
     (vls:check-current-current)
     (let ((more (cadr (assoc function (vls-spec 'm:more-doc))))
           (replace (cadr (assoc function (vls-spec 'm:replace-doc)))))
       (cond (more (setq ad-return-value (concat ad-return-value "\n" more)))
             (replace (setq ad-return-value replace)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A macro for Lisp Shell buffer process dependent VLS commands.

(defmacro defvlsc (command arg doc &rest body)
  `(defun ,command ,arg ,doc
    (interactive "P")
    (vls:pre-check)
    (cond ((assoc ',command (vls-spec 'm:replace-doc))
           (vls:totally-specific ',command ,(car arg)))
          (t ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VLS command support functions.

; Positions backwards for N+1th (-Nth foreword if N < 0) occurrence of given
; regular expression at the front of the pattern.  Returns nil if not found.
(defun vls:regexp-bsearch (n regexp)
  (cond ((< n 0) (if (re-search-forward regexp nil t (- n))
                     (re-search-backward regexp nil t 1)))
        (t (re-search-backward regexp nil t (1+ n)))))

; Toggle the binding of RETURN key beteen COMMAND1 COMMAND2.
(defun vls:toggle-return-key (command1 command2)
  (local-set-key "\C-m" (if (eq (key-binding "\C-m") command1)
                            command2 command1))
  (message "RETURN key is now %s" (key-binding "\C-m")))

; Is given symbol a Lisp defining symbol (eg def*)?
(defun vls:lisp-def-symbol (symbol)
  (eq (compare-strings "def" 0 3 (symbol-name symbol) 0 3 t) t))

; Return the enclosing Lisp definition id or nil.
(defun vls:def-id ()
  (save-excursion
    (beginning-of-defun)
    (forward-char 1)
    (if (vls:lisp-def-symbol (read (current-buffer)))
        (read (current-buffer)))))

; Return the enclosing Lisp definition.
(defun vls:def-form ()
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (forward-sexp) (buffer-substring start (point)))))

; Return the offset into enclosing Lisp definition.
(defun vls:def-offset ()
  (let ((here (point)))
    (save-excursion (beginning-of-defun) (- here (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VLS command package support functions.

; Predicate: Does the current Lisp have packages? 
(defun vls:has-packages () (vls-spec 'v:has-packages))

; Return the current Lisp process package.
(defun vls:current-package ()
  (let ((command (vls-spec 'p:current-package)))
    (if command (vls-lisp-evalue command))))

; Searches backwards for Nth in-package expression. Returns nil if not found.
(defun vls:in-package-search (n)
  (if (vls-spec 'v:has-packages)
      (vls:regexp-bsearch n (vls-spec 'v:in-package-re))
      (error "A symbol package system does not appear in Lisp process: %s"
             (vls-spec 'process-name))))

; When at beginning of in-package return package name.
(defun vls:in-package-name ()
  (re-search-forward (vls-spec 'v:in-package-re) nil t)
  (read (current-buffer)))

; Check if given name is a valid package.
(defun vls:check-valid-package (name)
  (if (and (vls:has-packages)
           (not (vls:lisp-evalue-p
                 (format (vls-spec 'p:package-predicate) name))))
      (error "Package %s has not been defined!" name)))

; Send an in-package of given package to current Lisp process.
(defun vls:do-in-package (package)
  (if (vls:has-packages)
      (vls:do-lisp-command 'c:in-package package)))

; Do an in-packge in current Lisp process from original source.
(defun vls:source-in-package ()
  (save-excursion
    (save-excursion (vls:check-valid-package (vls:in-package-name)))
    (mark-sexp 1) (vls:eval-region (point) (mark))))

; If not in a Lisp shell buffer eval previous in-package.
(defun vls:in-package ()
  (if (and (vls:has-packages) (not (vls:lisp-buffer-p)))
      (save-excursion (if (vls:in-package-search 0) (vls:source-in-package)))))

; Return the buffer effective package name.
(defun vls:effect-package-name ()
  (if (vls:has-packages)
      (save-excursion
        (let ((bpackage (vls:in-package-search 0)))
          (if bpackage (vls:in-package-name)
            (vls:current-package))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Expression editor.

; Edit continuation accessors.
(defun vls:edit-continuation () (vls-spec 'edit-continuation))
(defun vls:set-edit-continuation (continuation)
 (vls:put-spec 'edit-continuation continuation))

(defconst vls:*edit-entry-value-header*
  "    ---- Edit or enter below and type C-cC-c when done ----")

; Continue edit post processing.
(defun vls:continue-post-edit ()
  (let ((continuation (vls:edit-continuation)))
    (apply (car continuation) (cdr continuation))))

; Send vls edit buffer value when C-cC-c is typed.
(defun vls:send-edit-buffer-value ()
  (interactive)
  (let ((buffer (current-buffer)))
    (cond ((not (eq (get-buffer vls:*buffer*) buffer))
           (message "Current buffer is not the Vls Edit buffer!"))
          (t (goto-char (point-min))
             (cond
              ((search-forward  vls:*edit-entry-value-header* nil t)
               (forward-line 1)
               (vls:continue-post-edit))
              (t (message (concat "Could not find your edit! "
                                  "Did you mistakenly edit header?"))))))))

; Compute an vls edit buffer mode map from given mode map.
(defun vls:edit-mode-map ()
  (or (vls:get 'edit-mode-map)
      (vls:put 'edit-mode-map
               (let ((map (copy-keymap lisp-mode-map)))
                 (define-key map "\C-c\C-c" 'vls:send-edit-buffer-value)
                 map))))

; Given input mode data set the edit buffer mode and return the mode hook.
(defun vls:set-edit-buffer-mode ()
  (funcall 'lisp-mode) (use-local-map (vls:edit-mode-map)) 'lisp-mode-hook)

; Mode for editing a vls sexp.  The continuation is a list of the continue
; function followed by the  args to be applied when edit is completed.
(defun vls:edit-mode (continuation sexp header)
  (interactive)
  (vls:set-edit-continuation continuation)
  (let ((vls-buffer (get-buffer vls:*buffer*)))
    (if (eq vls-buffer (current-buffer)) (other-window 1))
    (if vls-buffer (kill-buffer vls:*buffer*)))
  (pop-to-buffer vls:*buffer*)
  (let ((hook-var (vls:set-edit-buffer-mode)))
    (cond (header (cond ((consp header)
                         (while header
                           (insert (car header)) (insert "\n")
                           (setq header (cdr header))))
                        (t (insert header) (insert "\n")))))
    (insert vls:*edit-entry-value-header*) (insert "\n")
    (insert sexp) (backward-sexp 1) (indent-sexp)
    (run-hooks hook-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Instrumenting Lisp code.

; Instrument definitions accessors.
(defun vls:idefs () (vls-spec 'idefs))
(defun vls:clear-idefs () (vls:put-spec 'idefs nil))
(defun vls:add-idef (idef) (vls:put-spec 'idefs (cons idef (vls:idefs))))
(defun vls:remove-idef (idef) (vls:put-spec 'idefs (delq idef (vls:idefs))))

; Instrument definition accessors.
(defun vls:idef-id (idef) (car idef))
(defun vls:idef-name (idef)
  (let ((id (vls:idef-id idef))) (if (consp id) (car id) id)))
(defun vls:set-idef-prop (idef prop value)
  (let ((exist (assoc prop (cadr idef))))
    (if exist (setcdr exist value)
      (setcar (cdr idef) (cons (cons prop value) (cadr idef))))))
(defun vls:idef-prop (idef prop) (cdr (assoc prop (cadr idef))))
(defun vls:idef-instruments (idef) (cddr idef))
(defun vls:set-idef-instruments (idef instruments)
  (setcdr (cdr idef) instruments))
(defun vls:idef-package (idef) (vls:idef-prop idef 'package))
(defun vls:idef-form (idef) (vls:idef-prop idef 'form))
(defun vls:idef-unique (idef) (vls:idef-prop idef 'unique))
(defun vls:set-idef-unique (idef value) (vls:set-idef-prop idef 'unique value))
(defun vls:idef-gvars (idef) (vls:idef-prop idef 'gvars))
(defun vls:idef-set-gvars (idef gvars) (vls:set-idef-prop idef 'gvars gvars))

; Instrument accessors.
(defun vls:instrument-offset (instrument) (car instrument))
(defun vls:instrument-form (instrument) (cadr instrument))
(defun vls:instrument-props (instrument) (nth 2 instrument))
(defun vls:set-instrument-form (instrument form)
  (setcar (cdr instrument) form))

; Instrument template accessors.
(defun vls:itemplet-id (itemplate) (car itemplate))
(defun vls:itemplet-form (itemplate) (cadr itemplate))
(defun vls:itemplet-props (itemplate) (nthcdr 2 itemplate))

; Return the current instrument templates.
(defun vls:instrument-templates ()
  (let ((templates (vls-spec 'v:instruments)))
    (if (symbolp templates) (eval templates) templates)))

; Select and return an instrument template.
(defun vls:select-itemplate (&optional prompt)
  (vls:choose-item (vls:instrument-templates) "VLS Instruments"
                   (or prompt "Select a VLS instrument: ") 'car))

; Return the existing instrument of id and package.
(defun vls:idef-of (id package)
  (let ((idef (assoc id (vls:idefs))))
    (if (and idef (equal package (vls:idef-package idef))) idef)))

; Assure that an idef of existing id and package exists and return.
(defun vls:assure-idef (id package form)
  (or (vls:idef-of id package)
      (let ((idef (list id nil)))
        (vls:add-idef idef) (vls:set-idef-prop idef 'package package)
        (vls:set-idef-prop idef 'form form) (vls:set-idef-prop idef 'unique 0)
        idef)))

; Add a nil instrument at offset in given idef and return.
(defun vls:add-to-idef (idef offset props)
  (let* ((inst (list offset nil props)) (last (cdr idef)) (insts (cdr last)))
    (if (not insts) (setcdr last (list inst))
      (while insts
        (cond ((< offset (caar insts))
               (setcdr last (cons inst insts)) (setq insts nil))
              ((not (cdr insts))
               (setcdr insts (list inst)) (setq insts nil))
              (t (setq last insts insts (cdr insts))))))
    inst))

; Add an instrument and return.
(defun vls:add-instrument (id package form offset props)
  (let ((idef (vls:idef-of id package)))
    (if idef (let ((idef-form (vls:idef-form idef)))
               (cond ((not (string= idef-form form))
                      (vls:remove-idef idef) (setq idef nil)))))
    (vls:add-to-idef
     (or idef (vls:assure-idef id package form)) offset props)))

; Return a composed instrument form given an instrument template.
(defun vls:compose-iform (idef itemplate)
  (vls:put-spec 'current-idef idef)
  (eval (vls:itemplet-form itemplate)))

; If the instrument has a replace next sexp return it's length or NIL.
(defun vls:replace-next-sexp-length (instrument)
  (cdr (assoc 'replace-next-sexp (vls:instrument-props instrument))))

; Compose and return the instrument form properties if any.
(defun vls:compose-iform-props (itemplate)
  (if (member 'replace-next-sexp (vls:itemplet-props itemplate))
      (list (cons 'replace-next-sexp (vls:next-sexp-length)))))

; Signal if point is not on a valid instrument place?
(defun vls:check-valid-instrument-point ()
  (let ((symbol-syntax '(?w ?_)))
    (if (or (vls:point-in-comment)
            (and (member (char-syntax (following-char)) symbol-syntax)
                 (member (char-syntax (preceding-char)) symbol-syntax)))
        (vls:abort "Invalid place for an instrument!"))))

; Signal if given offset is inside any wrapped instrument of given id.
(defun vls:check-instrument-inside (id package offset)
  (let ((instruments (vls:idef-instruments (vls:idef-of id package))))
    (while instruments
      (let* ((instrument (car instruments))
             (rlength (vls:replace-next-sexp-length instrument)))
        (if rlength
            (let ((ioffset (vls:instrument-offset instrument)))
              (if (and (>= offset ioffset) (< offset (+ ioffset rlength)))
                  (vls:abort (concat "Can not instrument inside a "
                                     "previous wrapped s-expression!"))))))
      (setq instruments (cdr instruments)))))

; Signal if about to wrap a previous instrument.
(defun vls:check-instrument-outside (id package offset props)
  (let ((rlength (cdr (assoc 'replace-next-sexp props))))
    (if rlength
        (let ((instruments
               (vls:idef-instruments (vls:idef-of id package)))
              (end (+ offset rlength)))
          (while instruments
            (let* ((instrument (car instruments))
                   (ioffset (vls:instrument-offset instrument)))
              (if (and (>= ioffset offset) (<= ioffset end))
                  (vls:abort "Can not wrap a previous instrument!")))
            (setq instruments (cdr instruments)))))))

; Show that given definition name is not instrumented.
(defun vls:show-not-instrumented (name)
  (if name (message "Definition is not instrumented: %s" name)
    (message "No instrumented definition here!")))

; Execute a given function with the idef of definition point is on.
(defun vls:with-idef (function)
  (let* ((id (vls:def-id))
         (idef (vls:idef-of id (vls:effect-package-name))))
    (if idef (funcall function idef)
      (vls:show-not-instrumented id))))

; Display a given instrumented definition.
(defun vls:show-instrumented-def (form)
  (vls:show-sexp "Instrumented definition:\n\n" form))

; Compile instrumented code.
(defun vls:compile-instrumented (name)
  (if (or (and (eq vls-compile-instrumented 'ask) (y-or-n-p "Compile code? "))
          (eq vls-compile-instrumented t))
      (let ((name (symbol-name name)))
        (vls:lisp-message (format "\nCompiling %s ...\n" name))
        (vls:do-lisp-command 'c:compile name))))

; Install an instrumented definition of given id and package.
(defun vls:install-instrumented (id package)
  (vls:do-in-package package)
  (let ((instrumented (vls:instrumented-form (vls:idef-of id package))))
    (vls:show-instrumented-def instrumented)
    (vls:do-lisp-string instrumented)
    (vls:compile-instrumented id)))

; Re-install instruments of form containing point.
(defun vls:reinstall-instrumented ()
  (let* ((id (vls:def-id)) (package (vls:effect-package-name))
         (idef (vls:idef-of id package)))
    (if idef
        (cond
          ((or
            (string= (vls:idef-form idef) (vls:def-form))
            (y-or-n-p
             "Defintion has changed; reinstall old one with instruments? "))
           (vls:check-valid-package package)
           (vls:install-instrumented id package)
           (message "Reinstalled intrumented form: %s" id))
          (t (message "Did not reinstall old definition!")))
      (vls:show-not-instrumented id))))

; Return and intrumented form with one instrument's parameters.
(defun vls:instrumented-form-1 (form iform offset soffset rlength)
  (concat (substring form 0 soffset)
          iform
          (substring form (+ soffset rlength))))

; Insert instruments and return the instrumented form of given idef.
(defun vls:instrumented-form (idef)
  (let ((form (vls:idef-form idef)) (instruments (vls:idef-instruments idef))
        (ilengths 0))
    (while instruments
      (let* ((instrument (car instruments))
             (offset (vls:instrument-offset instrument))
             (rlength (or (vls:replace-next-sexp-length instrument) 0))
             (iform (format (vls-spec 'v:instrument-visual)
                      (vls:instrument-form instrument)))
             (soffset (+ offset ilengths)))
        (setq form (vls:instrumented-form-1 form iform offset soffset rlength)
              ilengths (- (+ ilengths (length iform)) rlength)
              instruments (cdr instruments))))
    form))

; Redefine the definition that contains point.
(defun vls:redefine-point-def ()
  (vls:in-package)
  (vls:do-lisp-string (vls:def-form))
  (vls:compile-instrumented (vls:def-id)))

; Remove instrument from idef.
(defun vls:remove-instrument (instrument idef)
  (vls:set-idef-instruments
   idef (delete instrument (vls:idef-instruments idef))))

; Clear all instruments from definition containing point.
(defun vls:clear-instruments ()
  (let ((id (vls:def-id)))
    (if id
        (let ((idef (vls:idef-of id (vls:effect-package-name))))
          (cond (idef (vls:instrument-restart-variables idef)
                      (vls:remove-idef idef)
                      (vls:redefine-point-def)
                      (message
                       "Cleared instruments and redefined from source: %s"
                       id))
                (t (vls:show-not-instrumented id))))
      (vls:show-not-defining-form))))

; Revert the given idef back to saved form and remove from idefs.
(defun vls:revert-idef (idef)
  (vls:do-in-package (vls:idef-package idef))
  (vls:do-lisp-string (vls:idef-form idef))
  (vls:compile-instrumented (vls:idef-id idef))
  (vls:instrument-restart-variables idef)
  (vls:remove-idef idef))

; Clear all existing instruments.
(defun vls:clear-all-instruments ()
  (vls:show-instrumented-all)
  (if (y-or-n-p "Clear all definitions instruments: ")
      (let ((idefs (vls:idefs)))
        (while idefs
          (let ((idef (car idefs)))
            (vls:revert-idef idef))
          (setq idefs (cdr idefs))))
    (message "Cleared instruments in ALL definitions")))

; Continue updating an instrument after user edits it.
(defun vls:continue-instrument-edit (idef instrument)
  (let ((string (vls:next-buffer-string)))
    (if (vls:white-string string) (vls:remove-instrument instrument idef)
      (vls:set-instrument-form instrument string))
    (vls:install-instrumented (vls:idef-id idef) (vls:idef-package idef))
    (other-window 1)))

; Compute and return a list of closest instrument candidates to point.
(defun vls:instrument-edit-candidates (idef)
  (let ((here (vls:def-offset)) (instruments (vls:idef-instruments idef))
        candidates)
    (while instruments
      (let* ((instrument (car instruments))
             (offset (vls:instrument-offset instrument)))
        (if candidates
            (let ((this (abs (- here offset)))
                  (best (abs (- here (vls:instrument-offset
                                      (car candidates))))))
              (cond ((< this best) (setq candidates (list instrument)))
                    ((= this best)
                     (setq candidates (cons instrument candidates)))))
          (setq candidates (list instrument)))
        (setq instruments (cdr instruments))))
    (reverse candidates)))

; User chooses an instrument if more than one is closest to point.
(defun vls:instrument-edit-choose-candidates (instruments)
  (if (< (length instruments) 2) (car instruments)
    (vls:choose-item instruments 
     "The following Instruments are all just as close to point"
     "Choose an instrument to edit: "
     'vls:instrument-form)))

; Edit the instrument closest to point in given idef.
(defun vls:instrument-edit (idef)
  (let ((name (vls:idef-id idef))
        (chosen (vls:instrument-edit-choose-candidates
                 (vls:instrument-edit-candidates idef))))
    (vls:edit-mode
     (list 'vls:continue-instrument-edit idef chosen)
     (vls:instrument-form chosen)
     (format "Editing instrument in definition: %s" name))))

(defvar vls:*show-instrumented-all-info*
  "The command vls-instrument-clear-all will clear all instruments in all
definitions listed below but will install all the definitions that existed
before instrumentation.  Use vls-instrument-clear to clear all instruments
in a definition and reinstall the definition containing point.")

; Display all instrumented definitions and related information.
(defun vls:show-instrumented-all ()
  (save-current-buffer
    (with-output-to-temp-buffer vls:*buffer*
      (set-buffer vls:*buffer*)
      (princ vls:*show-instrumented-all-info*)
      (terpri) (terpri)
      (let ((idefs (vls:idefs)))
        (princ "Existing instrumented definitions:")
        (cond
          ((not idefs) (princ " NONE"))
          (t (terpri)
             (while idefs
               (let ((idef (car idefs)))
                 (princ "  ") (princ (or (vls:idef-package idef) ""))
                 (princ " ") (princ (vls:idef-name idef)) (terpri)
                 (setq idefs (cdr idefs))))))))))

; Show instruments of definition containing point.
(defun vls:show-instrumented ()
  (let* ((id (vls:def-id))
         (idef (vls:idef-of id (vls:effect-package-name))))
    (if idef (vls:show-instrumented-def (vls:instrumented-form idef))
      (vls:show-not-instrumented id))))

; Show a global instrument variable.
(defun vls:show-gvar (gvar)
  (let ((prefix "Instrument global variable"))
    (cond
      ((vls:boundp gvar)
       (vls:lisp-message (format "%s: %s =" prefix gvar))
       (vls:do-lisp-command 'c:var-print gvar))
      (t (vls:lisp-message (format "%s unbound: %s" prefix gvar))))))

; Show no global instrument variables set in given name.
(defun vls:show-no-instrument-variables (name)
  (message "No global variables set by instruments in %s" name))

; View any global variables set by instrumentments of given idef.
(defun vls:instrument-view-variables (idef)
  (let ((gvars (vls:idef-gvars idef)))
    (cond (gvars (vls:do-in-package (vls:idef-package idef))
                 (while gvars
                   (vls:show-gvar (car gvars))
                   (setq gvars (cdr gvars))))
          (t (vls:show-no-instrument-variables name)))))

; Restart a global instrument variable.
(defun vls:restart-gvar (gvar) (vls:do-lisp-command 'c:var-reset gvar))

; Restart any global variables set by instrumentments of given idef.
(defun vls:instrument-restart-variables (idef)
  (let ((gvars (vls:idef-gvars idef)) (name (vls:idef-name idef)))
    (cond (gvars
           (vls:do-in-package (vls:idef-package idef))
           (while gvars
             (vls:restart-gvar (car gvars))
             (setq gvars (cdr gvars)))
           (message "Restarted instrument variables of: %s" name))
          (t (vls:show-no-instrument-variables name)))))

; Continue adding an instrument after user edits it.
(defun vls:continue-instrument (name package form offset props)
  (let ((instrument (vls:add-instrument name package form offset props)))
    (vls:set-instrument-form instrument (vls:next-buffer-string))
    (vls:install-instrumented name package)
    (other-window 1)))

; Given an instrument template add a new instrument in current def-form.
(defun vls:instrument (itemplate)
  (save-excursion
    (let ((name (vls:def-id)))
      (if name
          (let ((form (vls:def-form))  (offset (vls:def-offset))
                (package (vls:effect-package-name)))
            (vls:check-valid-instrument-point)
            (vls:check-instrument-inside name package offset)
            (vls:check-valid-package package)
            (let ((idef (vls:assure-idef name package form))
                  (props (vls:compose-iform-props itemplate)))
              (vls:check-instrument-outside name package offset props)
              (vls:edit-mode
               (list 'vls:continue-instrument name package form offset props)
               (vls:compose-iform idef itemplate)
               (format "Instrumenting definition: %s" name))))
        (message "Point is not in a definition!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Instrument utility functions.

; Return a new idef unique.
(defun vls:new-idef-unique (idef)
  (vls:set-idef-unique idef (1+ (vls:idef-unique idef))))

; Return a unique string for given idef.
(defun vlsi-def-unique ()
  (let ((idef (vls-spec 'current-idef)))
    (format "%s %s %d"
      (let ((package (vls:idef-package idef))) (or package ""))
      (vls:idef-name idef) (vls:new-idef-unique idef))))

; Return a unique global debug variable.
(defun vlsi-gvar-unique (descriptor)
  (let* ((idef (vls-spec 'current-idef))
         (gvar
          (format "%s%s-%s-%d" (vls-spec 'v:global-debug-variable-prefix)
                  (vls:idef-name idef) descriptor (vls:new-idef-unique idef))))
    (vls:idef-set-gvars idef (nconc (vls:idef-gvars idef) (list gvar)))
    gvar))

; Get variables from user and return var pairs string.
(defun vlsi-get-var-pairs ()
  (let ((getting t) (pairs "")
        (prompt "Repeat entering variable RET then just RET when done: "))
    (while getting
      (let ((var (read-string prompt)))
        (if (string= var "") (setq getting nil)
          (setq pairs (format "%s (list '%s %s)" pairs var var)))))
    (format "(list%s)" pairs)))

; Return the next s-expression after point.
(defun vlsi-next-sexp ()
  (save-excursion
    (forward-sexp 1) (let ((end (point)))
                       (backward-sexp 1) (buffer-substring (point) end))))

; Selects an instrument and return its composed form.
(defun vlsi-select-instrument ()
  (let ((idef (vls-spec 'current-idef))
        (itemplate
         (vls:select-itemplate "Select a VLS instrument again to wrap: ")))
    (if (assoc 'vlsi-select-instrument (cdr (cadr itemplate))) nil
      (vls:compose-iform idef itemplate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VLS Commands that depend on Lisp process and Lisp shell buffer.

(defvlsc vls-apropos (arg)
  "Do an apropos of symbol that point is on."
  (vls:do-lisp-command 'c:apropos arg (vls:point-sexpr)))

(defvlsc vls-back-trace (arg)
  "Print out a back trace of the run time stack."
  (vls:do-lisp-command 'c:back-trace arg))

(defvlsc vls-compile-file (arg)
  "Compiles the file that the current buffer is visiting.
ARG = C-u = Eval and compile current definition that point is in"
  (if arg (let ((name (vls:def-id)))
            (cond (name (vls-eval-definition nil)
                        (vls:do-lisp-command 'c:compile name))
                  (t (vls:show-not-defining-form))))
    (let ((file-name (vls:buffer-file)))
      (vls:do-lisp-command 'c:compile-file file-name))))

(defvlsc vls-current-frame (arg)
  "Print current stack frame information."
  (vls:do-lisp-command 'c:current-frame arg))

(defvlsc vls-describe (arg)
  "Describe the symbol that point is on."
  (vls:do-lisp-command 'c:describe arg (vls:point-sexpr)))

(defvlsc vls-down-stack (arg)
  "Move down the run time stack one stack frame.
ARG = N = move down N frames
ARG = C-u = move to bottom of stack."
  (vls:do-lisp-command 'c:down-stack arg))

(defvlsc vls-exit-lisp (arg)
  "Exit Lisp."
  (vls:do-lisp-command 'c:exit-lisp)
  (let ((buffer (vls-spec 'buffer)))
    (if (y-or-n-p (format "Kill buffer: %s " (buffer-name buffer)))
        (kill-buffer buffer))))

(defvlsc vls-focus-process (arg)
  "Focus the listener on the first process in process list.
ARG = N = focus on the Nth process in process list
ARG = C-u = print the process list"
  (vls:do-lisp-command 'c:focus-process arg))

(defvlsc vls-help (arg)
  "Print out a list of Lisp commands."
  (vls:do-lisp-command 'c:help))

(defvlsc vls-in-package (arg)
  "Search current buffer backwards for an in-package expression and send
to the cuurent Lisp process.
ARG = N = same but searches for Nth previous in-package expression.
ARG = -N = same as positive N but searches forward.
ARG = C-u = pushes the previous in-package expression on the kill ring."
  (save-excursion
    (cond
      ((vls:in-package-search (if (numberp arg) arg 0))
       (cond ((consp arg) (mark-sexp 1)
              (vls:string-to-kill-ring (buffer-substring (point) (mark)) t))
             (t (vls:source-in-package))))
      (t (beep) (message "Could not find an in-package expression!")))))

(defvlsc vls-interrupt-lisp-process (arg)
  "Interrupt the Lisp process."
  (interrupt-process (get-process (vls-spec 'process-name))))

(defvlsc vls-instrument (arg)
  "Choose an instrument and apply at point."
  (catch 'vls:condition (vls:instrument (vls:select-itemplate))))

(defvlsc vls-instrument-display (arg)
  "Display the instruments of definition containing point."
  (vls:show-instrumented))

(defvlsc vls-instrument-display-all (arg)
  "Show all instrumented definitions and related information."
  (vls:show-instrumented-all))

(defvlsc vls-instrument-clear (arg)
  "Clear instruments of definition containing point
and redefine from source."
  (vls:clear-instruments))

(defvlsc vls-instrument-clear-all (arg)
  "Clear all instruments associated with current Lisp process.
But instrumented code will remain until you redefine from source."
  (vls:clear-all-instruments))

(defvlsc vls-instrument-again (arg)
  "Reinstall instruments of definition containing point."
  (vls:reinstall-instrumented))

(defvlsc vls-instrument-edit (arg)
  "Edit the instrument closest to point in current definition."
  (catch 'vls:condition (vls:with-idef 'vls:instrument-edit)))

(defvlsc vls-instrument-view-variables (arg)
  "View any variables set by instruments of definition containing point."
  (vls:with-idef 'vls:instrument-view-variables))

(defvlsc vls-instrument-restart-variables (arg)
  "Restart any variables set by instruments of definition containing point."
  (vls:with-idef 'vls:instrument-restart-variables))

(defvlsc vls-kill-last-output (arg)
  "Kill last lisp printer output up to the end of the buffer."
  (vls:with-lisp-buffer-other-window)
  (kill-region (vls-spec 'begin-of-last-output) (point-max))
  (vls:done-with-lisp-buffer-other-window))

(defvlsc vls-load-source (arg)
  "Loads the source file that the current buffer is visiting.
ARG = C-u = print results of each expression."
  (vls:load-file (vls:buffer-file) arg))

(defvlsc vls-load-source-or-binary (arg)
  "Loads the source file or binary file (if exists)
associated with the file the current buffer is visiting.
ARG = C-u = print results of each expression."
  (vls:load-file (file-name-sans-extension (vls:buffer-file)) arg))

(defvlsc vls-make-current (arg)
  "Make current buffer the current Lisp shell."
  (let ((specs (vls:current-buffer-specs)))
    (cond (specs (vls:put 'current-specs (cadr specs))
                 (message (format "Current Lisp Shell: %s" (car specs))))
          (t (message "Current buffer is not a Lisp shell!")))))

(defvlsc vls-reload-specifics (arg)
  "Reload the current Lisp Mode Specifics."
  (let ((specifics (vls-spec 'specifics))
        (buffer (vls-spec 'buffer)))
    (message "Reloading Lisp Specifics: %s" (car specifics))
    (vls:set-shell-specs specifics)
    (vls:put-spec 'buffer buffer)
    (message "Reloaded Lisp Specifics: %s" (car specifics))))

(defvlsc vls-reset-listener (arg)
  "Reset the Lisp listener to the top level listener.
ARG = N = Reset N listeners."
  (vls:with-lisp-buffer-end-other-window t)
  (cond (arg (vls:send-lisp-command 'c:pop-listener
                                    (prefix-numeric-value arg)))
        (t (vls:send-lisp-command 'c:top-listener nil)))
  (vls:done-with-lisp-buffer-other-window))

(defvlsc vls-return-frame (arg)
  "Return nil from the current stack frame and continue.
ARG = C-u = Enter a returned value and continue."
  (vls:with-lisp-buffer-end-other-window t)
  (let ((value (if (consp arg) (read-string "Enter frame return value: "))))
    (vls:send-lisp-command 'c:return-frame value))
  (vls:done-with-lisp-buffer-other-window))

(defvlsc vls-step (arg)
  "Step Lisp stepper.
ARG = N = step over next N expressions.
ARG = C-u = evaluate current expression in non-stepping mode."
  (vls:with-lisp-buffer-other-window)
  (if (not arg) (setq arg 1))
  (if (consp arg)
      (vls:send-lisp-command 'c:step-over nil)
    (vls:send-lisp-command 'c:step-next arg))
  (vls:done-with-lisp-buffer-other-window))

(defvlsc vls-up-stack (arg)
  "Move up the run time stack one stack frames.
ARG = N = move up N frames
ARG = C-u = move to top of stack."
  (vls:do-lisp-command 'c:up-stack arg))

(defvlsc vls-var-value (arg)
  "Print the value of a local variable referenced by name."
  (vls:do-lisp-command 'c:var-value
    (read-from-minibuffer "Enter variable name: ")))

(defvlsc vls-what-error (arg)
  "Print the current error."
  (vls:do-lisp-command 'c:what-error))

(defvlsc vls-what-package (arg)
  "Displays the current Lisp current package."
  (vls:do-lisp-command 'c:what-package arg))

(defvlsc vls-yank-last-input (arg)
  "Yank at point the last input sent to the current Lisp process.
Repeated yanks will cycle through input ring."
  (vls:yank-an-input (eq last-command 'vls-yank-last-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VLS Commands that do not depend on VLS Lisp shell buffer or Lisp process.

(defun vls-instrument-toggle-compile (arg)
  "Toggle compiling or not during install of instrumented code.
ARG = C-u = Ask to compile."
  (interactive "P")
  (cond (arg (setq vls-compile-instrumented 'ask))
        (t (setq vls-compile-instrumented (not vls-compile-instrumented))))
  (message (format "%sompile instrumented code during install"
             (if vls-compile-instrumented
                 (if (eq vls-compile-instrumented 'ask) "Ask to c" "C")
               "Do not c"))))

(defun vls-version ()
  "Show the running VLS version"
  (interactive)
  (message "Running VLS version = %s" *vls-version*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eval collection of commands support functions.

; Evaling the previous s-expression.
(defun vls:eval-previous ()
  (let* ((eval-print-separator (vls-spec 'v:eval-print-separator))
         (eps (and (vls:lisp-buffer-p) (= (point) (point-max))
                   (not (zerop (length eval-print-separator))))))
    (if eps (insert-string eval-print-separator))
    (let (start end)
      (save-excursion (mark-sexp -1) (setq start (mark) end (point)))
      (vls:in-package)
      (vls:eval-region start (- end (if eps 1 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eval collection of commands.

(defvlsc vls-eval-previous (arg)
  "Send the previous sexp to the current Lisp process."
  (vls:eval-previous))

(defvlsc vls-send-input (arg)
  "Send the last input in a Lisp shell buffer to the current Lisp process."
  (let ((buffer (vls-spec 'buffer)))
    (if (vls:shell-p) (comint-send-input)
      (message "vls-send-input only works in a VLS shell buffer!"))))

(defvlsc vls-send-region (arg)
  "Send currrent region to the current Lisp process."
  (vls:in-package) (vls:eval-region (point) (mark)))

(defvlsc vls-eval-definition (arg)
  "Eval the definition that point is in."
  (save-excursion
    (end-of-defun)
    (vls:eval-previous)))

(defcustom vls-evals
  '(vls-eval-previous vls-send-input  vls-send-region vls-eval-definition)
  "The commands executed by vls-eval")

(defvlsc vls-eval (arg)
  "Execute the 0th command from the list vls-evals.
ARG = C-u = Show the vls-evals list.
ARG = N = Execute the Nth command."
  (if (consp arg)
      (vls:show-list-numbered vls-evals "ARG bindings for vls-eval")
    (let* ((n (if (not arg) 0 arg)) (command (nth n vls-evals)))
      (apply (if (consp command) (car command) command)
             (if (consp command) (cdr command) '(nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make keys for lisp mode.

; Define a key for all specified Lisp mode maps.
(defun vls:define-lisp-mode-key (key command)
  (let ((maps vls:*mode-maps*))
    (while maps
      (define-key (eval (car maps)) key command)
      (setq maps (cdr maps)))))

; Define keys for VLS Lisp mode uisng C-c as a preix.
(vls:define-lisp-mode-key "\C-c!" 'vls-exit-lisp)
(vls:define-lisp-mode-key "\M-\C-m" 'vls-eval)
(vls:define-lisp-mode-key "\M-\C-x" 'vls-eval-definition)
(vls:define-lisp-mode-key "\C-c\C-m" 'vls-send-input)
(vls:define-lisp-mode-key "\C-cc" 'vls-compile-file)
(vls:define-lisp-mode-key "\C-c\C-c" 'vls-interrupt-lisp-process)
(vls:define-lisp-mode-key "\C-c\C-b" 'vls-back-trace)
(vls:define-lisp-mode-key "\C-c\C-d" 'vls-down-stack)
(vls:define-lisp-mode-key "\C-c\C-e" 'vls-what-error)
(vls:define-lisp-mode-key "\C-ch" 'vls-help)
(vls:define-lisp-mode-key "\C-cl" 'vls-load-source)
(vls:define-lisp-mode-key "\C-c\C-l" 'vls-load-source-or-binary)
(vls:define-lisp-mode-key "\C-c\C-o" 'vls-kill-last-output)
(vls:define-lisp-mode-key "\C-cp" 'vls-in-package)
(vls:define-lisp-mode-key "\C-c\C-p" 'vls-what-package)
(vls:define-lisp-mode-key "\C-c\C-r" 'vls-reset-listener)
(vls:define-lisp-mode-key "\C-c\C-s" 'vls-step)
(vls:define-lisp-mode-key "\C-c\C-u" 'vls-up-stack)
(vls:define-lisp-mode-key "\C-c\C-v" 'vls-var-value)
(vls:define-lisp-mode-key "\C-c\C-y" 'vls-yank-last-input)

; Define keys for VLS Lisp mode information commands using C-c i prefix.
(vls:define-lisp-mode-key "\C-cia" 'vls-apropos)
(vls:define-lisp-mode-key "\C-cid" 'vls-describe)
(vls:define-lisp-mode-key "\C-civ" 'vls-version)

; Define keys for VLS Lisp mode instruments using C-c SPC prefix.
(vls:define-lisp-mode-key "\C-c  " 'vls-instrument)
(vls:define-lisp-mode-key "\C-c c" 'vls-instrument-clear)
(vls:define-lisp-mode-key "\C-c C" 'vls-instrument-clear-all)
(vls:define-lisp-mode-key "\C-c d" 'vls-instrument-display)
(vls:define-lisp-mode-key "\C-c D" 'vls-instrument-display-all)
(vls:define-lisp-mode-key "\C-c e" 'vls-instrument-edit)
(vls:define-lisp-mode-key "\C-c\C-f" 'vls-current-frame)
(vls:define-lisp-mode-key "\C-c r" 'vls-instrument-again)
(vls:define-lisp-mode-key "\C-c t" 'vls-instrument-toggle-compile)
(vls:define-lisp-mode-key "\C-c v" 'vls-instrument-view-variables)
(vls:define-lisp-mode-key "\C-c V" 'vls-instrument-restart-variables)

; Define keys for VLS Lisp mode extended commands using C-cC-x as a prefix.
(vls:define-lisp-mode-key "\C-c\C-xc" 'vls-make-current)
(vls:define-lisp-mode-key "\C-c\C-xf" 'vls-focus-process)
(vls:define-lisp-mode-key "\C-c\C-xr" 'vls-return-frame)
(vls:define-lisp-mode-key "\C-c\C-xR" 'vls-reload-specifics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A sample interactive Lisp shell command using VLS.

(defcustom vlsc-shells nil "VLS shells used by vlsc")

; List Lisp shells with associated args.
(defun vlsc-list ()
  (let ((shells vlsc-shells) (n 0))
    (with-output-to-temp-buffer "*vlsc*"
      (princ "Lisp shells invoked by args\n\n")
      (while shells
        (let ((shell (car shells)))
          (princ n) (princ " = ") (princ (car shell)) (terpri)
          (setq n (1+ n) shells (cdr shells)))))))

(defun vlsc (arg)
  "Invoke the first Lisp shell in the variable vlsc-shells.
ARG = N = Invoke the Nth shell.
ARG = C-u = List the Lisp shells invoked by ARGs."
  (interactive "P")
  (cond ((not arg) (vls-shell (car vlsc-shells)))
        ((and (numberp arg) (>= arg 0) (<= arg (length vlsc-shells)))
         (vls-shell (nth arg vlsc-shells)))
        (t (vlsc-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
