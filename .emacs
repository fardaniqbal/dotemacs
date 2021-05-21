;;
;; Fardan's ~/.emacs file
;; Last updated 2017.01.04
;;
;; NOTES:
;;
;; - See function 'enlarge-window' to adjust horizontal window size.
;;
;; TODO:
;;
;; - Add a minor mode similar to auto-fill-mode, except that after every
;;   command, it checks if the current line is longer than the fill column, and
;;   if it is, automatically do a fill-paragraph or something similar. In other
;;   words, just basically make the word wrapping behave more like every single
;;   text editor other than Emacs.
;;
;; - Automatically clear the message buffer a few seconds after anything is
;;   printed to it.
;;
;; - Find a better color for variable names (font-lock-variable-name-face)
;;   rather than just disabling it altogether.
;;
;; - For C and C++ modes, make sure 'TYPENAME' is highlighted correctly (as
;;   'font-lock-type-face') in declarations of the following form:
;;
;;   typedef MODIFIER* ( BLOCKTYPE BASETYPE | BLOCKTYPE | BASETYPE ) \{ ... \} MODIFIER* REFPTR* TYPENAME ARINDEX* (\, REFPTR* TYPENAME ARINDEX*)*;
;;   // Where 'MODIFIER'    = (const|extern|long|short|signed|unsigned|static|volatile|register|inline|etc.)
;;   //       'BLOCKTYPE'   = (struct|union|enum|class|etc.)
;;   //       'BASETYPE'    = ([a-zA-Z_][a-zA-Z_0-9]* (\<...\>)? ( \:\: [a-zA-Z_][a-zA-Z_0-9]* (\<...\>)?)*)
;;   //       'REFPTR'      = ([\*\&] (const( const)*)?)
;;   //       'TYPENAME'    = ([a-zA-Z_][a-zA-Z_0-9]*)
;;   //       'ARINDEX'     = (\[...\])
;;
;;   Make sure that nested {curly braces}, <angle brackets>, and [square
;;   brackets] are handled correctly. If possible, try to support arbitrarily
;;   deep nesting. NOTE: Watch out for '<<' and '>>' - consecutive angle
;;   brackets with no separating whitespace. C++ treats these as bitshift
;;   operators, NOT consecutive angle brackets.
;;
;; - For cc-modes, add hilighting for javadoc-style @param, @return, etc.
;;   inside block comments.
;;
;; - Acceleration for the M-n and M-p scrolling commands.
;;
;; - Add a redo command that "redo"s the undo command. Modify the undo command
;;   so it doesn't start redoing stuff after we reach the beginning of the undo
;;   history, or after a save. Variable 'buffer-undo-list' might be useful.
;;
;; - Make the forward-word and backward-word commands a bit more lenient. That
;;   is, make them move to boundaries that aren't necessarily "word"
;;   boundaries. The default behavior is to move strictly to boundaries of
;;   "words", which gets really annoying because it's easy to accidentally skip
;;   over a large chunk of text containing only non-alphanumeric characters. In
;;   particular, make sure it never moves up or down a line (unless it's
;;   at the beginning or end of a line).
;;
;; - Make forward-delete-word and backward-delete-word treat word boundaries
;;   the same way. Accidentally erasing huge chunks of text is never fun.
;;
;; - For cc-modes, add an after-save-hook that does things like automatically
;;   generate tags data, or possible compile, etc.
;;
;; - Make a "session saver" type thing similar to Firefox's session saver. This
;;   should remember all the buffers that were open, how the windows were laid
;;   out, etc. when you exit emacs. It should automatically restore the session
;;   when emacs is restarted. The 'current-window-configuration' function might
;;   be useful for this. Also, search google for Emacs's "Desktop Save Mode".
;;
;; - This might cause problems when running multiple Emacs instances, all
;;   sharing the same saved session file. Be careful: Two separate Emacs
;;   instances could be running on two different machines, but share the same
;;   session file if home directory is mounted on NFS!!!
;;
;; - Possible ways to handle a session file shared by multiple instances:
;;   - On startup, if we can detect that the session file is NOT shared by any
;;     other currently running instances, the current instance will restore
;;     that saved session. This instance now has the "active" session. On the
;;     other hand, if we detect that the session file IS being used by another
;;     session, Emacs just starts up normally (without restoring the saved
;;     saved session). This instance's session will NOT be "active".
;;   - One possible way to mark which instance has the active session:
;;     Use a file that simply stores (1) the process ID of the active-session
;;     instance, and (2) the hostname of the machine on which it's running.
;;     See 'emacs-pid' built-in function.
;;
;; - Provide an interactive "make-session-active" command that makes the
;;   current instance's session the active one. If current session is already
;;   active, remind the user with a little message or something.
;;
;; - Provide an interactive "save-session" command so the user can save at any
;;   time (although this really shouldn't be necessary if the session saver is
;;   robust and working correctly).
;;
;; - Add an interactive 'refactor' command for C/C++/Java. This command should
;;   correctly rename variables, functions, types, preprocessor defines/macros,
;;   etc. Don't forget to rename subclasses' virtual member function overrides,
;;   and (optionally) overloaded functions. Also provide the option to replace
;;   text in comments and strings.
;;
;; - Add a mode (called "highlight-misspellings" or such) that makes spelling
;;   mistakes visible as you type, much like a word processor.
;;

;; add ~/.emacs-custom directory to load path
(setq load-path (cons "~/.emacs.d/lisp" load-path))

;; tweak a few specific things early because they can shorten start up time
(if (null window-system)
    (menu-bar-mode -1)  ; don't need the top menu bar in a terminal
  (tool-bar-mode -1)
  (if (<= emacs-major-version 19) (progn)
    ;; scrollbars look horrendous unless we're using gtk or win32 gui libs
    (if (and window-system (or (memq window-system '(w32 win32 winnt))
                               (condition-case nil gtk-version-string (error nil))))
        (progn (scroll-bar-mode 1)
               (set-scroll-bar-mode 'right))
      (scroll-bar-mode -1))))

; makes split-window scrolling significantly smoother when using emacs over ssh
(setq baud-rate 2457600)    ; 1228800 (default seems to be 38400)

; PuTTY fix for <end> key (without this emacs thinks <end> is <select> when
; using emacs over PuTTY.)  From http://emacswiki.org/emacs/PuTTY
(define-key global-map [select] 'move-end-of-line)

; Add a y-or-n prompt to confirm exit, so we don't quit emacs by accident.
(setq confirm-kill-emacs 'y-or-n-p)

;; ----------------------------
;; Misc. utility functions.

;; Emacs 23.2 replaced c-subword-mode with subword-mode.  These wrappers help
;; us use subword-mode if it's available, else c-subword-mode otherwise.
(defun fi-subword-mode-is-active ()
  "Checks if either subword-mode or c-subword-mode is active."
  (or (and (boundp 'subword-mode) subword-mode)
      (and (boundp 'c-subword-mode) c-subword-mode)))

(defun fi-subword-mode (&optional arg)
  "Calls subword-mode if available, else calls c-subword-mode."
  (interactive)
  (if (fboundp 'subword-mode) (subword-mode arg)
    (if (fboundp 'c-subword-mode) (c-subword-mode arg))))

(defun fi-subword-forward (&optional arg)
  "Calls subword-forward if available, else calls c-forward-subword."
  (interactive "p")
  (if (fboundp 'subword-forward) (subword-forward arg)
    (if (fboundp 'c-forward-subword) (c-forward-subword arg))))

(defun fi-subword-backward (&optional arg)
  "Calls subword-backward if available, else calls c-backward-subword."
  (interactive "p")
  (if (fboundp 'subword-backward) (subword-backward arg)
    (if (fboundp 'c-backward-subword) (c-backward-subword arg))))

;; ----------------------------
;; Begin actual customization stuff.

;; disable dangerous/annoying key bindings
(global-unset-key (kbd "C-o"))          ; open-line
(global-unset-key (kbd "C-q"))          ; quoted-insert
(global-unset-key (kbd "C-t"))          ; transpose-chars
(global-unset-key (kbd "C-j"))          ; newline-and-indent
(global-unset-key (kbd "M-j"))          ; indent-new-comment-line
(global-unset-key (kbd "ESC j"))        ; indent-new-comment-line
(global-unset-key (kbd "M-t"))          ; transpose-words
(global-unset-key (kbd "ESC t"))        ; transpose-words
(global-unset-key (kbd "C-M-o"))        ; split-line
(global-unset-key (kbd "ESC C-o"))      ; split-line
(global-unset-key (kbd "ESC ESC ESC"))  ; keyboard-escape-quit
(global-unset-key (kbd "M-~"))          ; not-modified
(global-unset-key (kbd "ESC ~"))        ; not-modified

;; change behavior of 'home' and 'C-a' key bindings
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

;; prevent alt-backspace and alt-d from modifying the kill ring
(defun delete-word-no-kill (&optional ARG)
  "Same as kill-word, but doesn't save deleted word into kill ring."
  (interactive "p*")
  (if (null ARG) (setq ARG 1))
  (if (< ARG 0) (backward-delete-word-no-kill (- ARG))
    (barf-if-buffer-read-only)
    (while (> ARG 0)
      (let ((old-point (point)))
        (if (fi-subword-mode-is-active)
            (fi-subword-forward 1)
          (forward-word 1))
        (delete-backward-char (- (point) old-point) nil))
      (setq ARG (1- ARG)))))

(defun backward-delete-word-no-kill (&optional ARG)
  "Same as backward-kill-word, but doesn't save deleted word into kill ring."
  (interactive "p*")
  (if (null ARG) (setq ARG 1))
  (if (< ARG 0) (delete-word-no-kill (- ARG))
    (barf-if-buffer-read-only)
    (while (> ARG 0)
      (let ((old-point (point)))
        (if (fi-subword-mode-is-active)
            (fi-subword-backward 1)
          (backward-word 1))
        (delete-char (- old-point (point)) nil))
      (setq ARG (1- ARG)))))

(global-set-key "\M-d"                  'delete-word-no-kill)
(global-set-key (kbd "<C-backspace>")   'delete-word-no-kill)
(global-set-key (kbd "<M-DEL>")         'backward-delete-word-no-kill)
(global-set-key (kbd "<C-delete>")      'backward-delete-word-no-kill)
(global-set-key (kbd "M-h")             'backward-delete-word-no-kill)
(global-set-key (kbd "ESC h")           'backward-delete-word-no-kill)

;; undo/redo for n00bs (provided by 'redo.el')
(require 'redo)
(global-set-key (kbd "ESC C-_") 'redo)
(global-set-key (kbd "ESC /")   'redo)
(global-set-key (kbd "C-M-/")   'redo)

;; friendlier navigation (provided by 'fi-scroll.el')
(require 'fi-scroll)
(global-set-key "\M-n"               'interactive-scroll-up)
(global-set-key "\M-p"               'interactive-scroll-down)
(global-set-key (kbd "ESC <down>")   'interactive-scroll-up)
(global-set-key (kbd "<M-down>")     'interactive-scroll-up)
(global-set-key (kbd "ESC <up>")     'interactive-scroll-down)
(global-set-key (kbd "<M-up>")       'interactive-scroll-down)
;(global-set-key (kbd "<wheel-down>") 'interactive-scroll-up)
;(global-set-key (kbd "<wheel-up>")   'interactive-scroll-down)
(global-set-key (kbd "M-{")         'backward-paragraph)
(global-set-key (kbd "ESC {")       'backward-paragraph)
(global-set-key (kbd "M-}")         'forward-paragraph)
(global-set-key (kbd "ESC }")       'forward-paragraph)
;; note: don't bind "M-[" or "ESC [" because it'll break emacs's handling of
;; certain terminal escape codes.

(setq hscroll-margin 1)             ; start hscrolling when in this margin; default 5
(setq hscroll-step 1)               ; one-line horizontal scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(4 ((shift) . 1) ((control))))
(setq scroll-step 1)                ; do only one-line scrolling. however, for
(setq scroll-conservatively 100)    ; this to work correctly, scroll-
                                    ; conservatively must also be set to a
                                    ; miniumum of about 50. otherwise, it
                                    ; scrolls much more than one line when
                                    ; emacs's redrawing can't keep up with the
                                    ; scrolling speed

;; buffer management
;(require 'tabbar)

(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if this buffer will be considered." t)

(global-set-key [?\M-k]  (lambda () (interactive) (kill-buffer (current-buffer))))

(global-set-key [?\M-\;] 'cycle-buffer-backward)
(global-set-key [?\M-']  'cycle-buffer)

;; alt + pgup/pgdn to cycle buffers permissively
(global-set-key (kbd "ESC <prior>") 'cycle-buffer-backward-permissive)
(global-set-key (kbd "<M-prior>")   'cycle-buffer-backward-permissive)
(global-set-key (kbd "ESC <next>")  'cycle-buffer-permissive)
(global-set-key (kbd "<M-next>")    'cycle-buffer-permissive)

(global-set-key (kbd "ESC <right>") 'cycle-buffer)
(global-set-key (kbd "<M-right>")   'cycle-buffer)
(global-set-key (kbd "ESC <left>")  'cycle-buffer-backward)
(global-set-key (kbd "<M-left>")    'cycle-buffer-backward)

;; window management
(global-set-key [?\M-1] (quote delete-other-windows))
(global-set-key [?\M-2] (quote split-window-vertically))
(global-set-key [?\M-3] (quote split-window-horizontally))
(global-set-key [?\M-0] (quote delete-window))

(global-set-key (kbd "<C-up>")    'windmove-up)
(global-set-key (kbd "<C-down>")  'windmove-down)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-left>")  'windmove-left)

(global-set-key [?\M-o] (quote other-window))
; Don't set [?\M-O] b/c it messes up the arrow keys (and possibly other keys)
; for some reason. It might do the same for other Meta + capital letter
; combinations.

(defun window-height-increase ()
  "Increases the current window's height by a single line."
  (interactive)
  (set-window-text-height nil (1+ (window-text-height))))
(defun window-height-decrease ()
  "Decreases the current window's height by a single line."
  (interactive)
  (set-window-text-height nil (1- (window-text-height))))

(global-set-key [?\M-=] (quote window-height-increase))
(global-set-key [?\M--] (quote window-height-decrease))

(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "C-M-o")   'other-window-backward)
(global-set-key (kbd "ESC C-o") 'other-window-backward)

(defun my-window-layout ()
  "Splits windows up the way I like it."
  (interactive)
  (let ((my-start-window (selected-window))
        (col-max (window-width))
        (col-min 90)
        (col-tgt nil)
        (cnt 1))
    (while (> (/ col-max (1+ cnt)) col-min)
      (setq cnt (1+ cnt)))
    (setq col-tgt (/ col-max cnt))
    (if (= cnt 1) nil
      (select-window (split-window-horizontally col-tgt))
      (select-window (previous-window (split-window-vertically -10)))
      (setq cnt (- cnt 1))
      (while (> cnt 1)
        (select-window (split-window-horizontally col-tgt))
        (setq cnt (- cnt 1)))
      (select-window (next-window))
      (switch-to-buffer "*compilation*")
      (select-window my-start-window)
      (select-window (previous-window (split-window-vertically 10)))
      (switch-to-buffer "*grep*")
      (select-window (next-window)))))

;; disable backups/auto-save
(setq backup-inhibited t)
(setq auto-save-default nil)

;; terminal/shell stuff
(setq explicit-shell-file-name "/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(global-set-key "\C-cs" 'shell)

;; shell mode hook
(defun my-shell-mode-common-hook ()
  (rename-buffer "shell" t)
  (ansi-color-for-comint-mode-on)
  (setq comint-highlight-prompt nil)
  (setq comint-highlight-input nil)
  (setq comint-input-ignoredups t)
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (local-set-key (quote [up])   'comint-previous-input)
  (local-set-key (quote [down]) 'comint-next-input)
  (local-set-key "\M->"  (lambda () (interactive) (end-of-buffer) (comint-show-maximum-output)))
  (local-set-key "\M-n"  (lambda () (interactive) (scroll-up   2)))
  (local-set-key "\M-p"  (lambda () (interactive) (scroll-down 2))))
(add-hook 'shell-mode-hook 'my-shell-mode-common-hook)

; extra functions that can't be added directly to comint-output-filter-functions
(defun my-output-filter-hook (str)
  ;(if (not (string= (mode-line-mode-name) "Shell"))
  ;    (null t)
  ;  (font-lock-mode -1))
  (comint-show-maximum-output))

; discard control-M characters from output
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
; truncate buffer to a certain max number of lines, specified by comint-buffer-maximum-size
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
; automatically check for password prompts
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
; my other output filter functions
(add-hook 'comint-output-filter-functions 'my-output-filter-hook)

;; compilation
(require 'compile)

(defvar fi-compilation-buffer nil
  "Buffer used for compilation.")
(setq-default fi-compilation-buffer nil)

(defun my-easy-compile (command)
  "Like the regular 'compile' command, except you can use the same key to
execute the compile command instead of having to reach all the way down to the
ENTER key."
  (interactive
   (list
    (if (eq (get-buffer-window (current-buffer)) (minibuffer-window)) "i"
      (let ((command (eval compile-command)))
        (if (or compilation-read-command current-prefix-arg)
            (read-from-minibuffer "Compile command: "
                                  command nil nil
                                  (if (equal (car compile-history) command)
                                      '(compile-history . 1)
                                    'compile-history))
          command)))))
  (if (eq (get-buffer-window (current-buffer)) (minibuffer-window))
      (exit-minibuffer)
    (compile command)))

(defvar fi-compilation-loop-next-error nil
  "If non-nil, then `next-error-loop' will loop back to the first compilation
error.  This is used internally, so don't set it manually.")

(defun next-error-loop (&optional ARG)
  "Like `next-error', but loops back to the first error after
reaching the last one."
  (interactive "p")
  (let ((fi-lerr nil))
    (condition-case fi-lerr
        (progn (if (null fi-compilation-loop-next-error) (next-error ARG)
                 (next-error ARG t))
               (setq fi-compilation-loop-next-error nil))
      (error (setq fi-compilation-loop-next-error t)
             (error "%s" (car (cdr fi-lerr)))))))

(defun compilation-error-loop-internal (FUNC BASE-PT &optional N
                                        DIFFERENT-FILE PT)
  "Internal use only."
  (let ((fi-lerr nil)
        (fi-lold-buffer (current-buffer)))
    ;(save-excursion
      (set-buffer (compilation-find-buffer nil))
      (setq fi-compilation-buffer (current-buffer))
      (condition-case fi-lerr
          (progn (if fi-compilation-loop-next-error (goto-char BASE-PT))
                 (funcall FUNC N DIFFERENT-FILE PT)
                 (setq fi-compilation-loop-next-error nil))
        (error (setq fi-compilation-loop-next-error t)
               (error "%s" (car (cdr fi-lerr)))))
     ;)
      ;;;
      ;;; !!! TODO !!!
      ;;;
    (set-buffer fi-lold-buffer)
  ))

(defun compilation-next-error-loop (N &optional DIFFERENT-FILE PT)
  "Like `compilation-next-error', but loops back to the first
error after reaching the last one."
  (interactive "p")
  (compilation-error-loop-internal
   'compilation-next-error (point-min) N DIFFERENT-FILE PT))

(defun compilation-previous-error-loop (N &optional DIFFERENT-FILE PT)
  "Like `compilation-previous-error', but loops back to the last
error after reaching the first one."
  (interactive "p")
  (compilation-error-loop-internal
   'compilation-previous-error (point-max) N DIFFERENT-FILE PT))

(defun my-compilation-mode-hook ()
  (setq compilation-search-path (list "." ".." "../.." "../../.."))
  (setq compilation-skip-threshold 0)     ; don't skip any messages
  (setq compilation-skip-to-next-location nil)
  (setq fi-compilation-buffer (current-buffer))
  (local-set-key "\M-n" 'interactive-scroll-up)
  (local-set-key "\M-p" 'interactive-scroll-down))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun my-compilation-finish-function (BUFFER DESCRIPTION)
  ;; Makes compilation window show as much output as possible.
  (let ((fi-lwindow-start 1)
        (fi-lfinal-point 1)
        (fi-lcomp-window nil)
        (fi-lrunaway 5000)
        (fi-lold-buffer (current-buffer)))
    (set-buffer fi-compilation-buffer)
    (setq fi-lcomp-window (and fi-compilation-buffer (get-buffer-window fi-compilation-buffer)))
    (goto-char (point-max))
    (setq fi-lfinal-point (point))
    (save-excursion
      (goto-char fi-lwindow-start)
      (set-window-start fi-lcomp-window (point) t)
      (while (and (not (pos-visible-in-window-p fi-lfinal-point fi-lcomp-window t))
                  (> fi-lrunaway 0)
                  (= (forward-line 1) 0))
        (set-window-start fi-lcomp-window (point) t)
        (setq fi-lrunaway (1- fi-lrunaway)))
      (setq fi-lwindow-start (point)))
    (set-window-start fi-lcomp-window fi-lwindow-start nil)
    (set-buffer fi-lold-buffer)))

(if (and emacs-major-version (<= emacs-major-version 21))
    (setq compilation-finish-function 'my-compilation-finish-function)
  (setq compilation-finish-functions (cons 'my-compilation-finish-function compilation-finish-functions)))

(setq next-error-highlight t)
(setq next-error-highlight-no-select t)
(setq compilation-scroll-output t)
(setq compilation-window-height 10)
(setq compile-command "for i in $(seq 10 -1 1); do \
path=$(perl -e 'print(\"../\" x '$i')'); \
while [ -f ${path}Makefile ]; do cd $path; done; \
done; [ -f Makefile ] && make")

(global-set-key (quote [f6])  'my-easy-compile)
(global-set-key (quote [f7])  'my-easy-compile)
(global-set-key (kbd "ESC `") 'next-error-loop)

;; debugging
(setq gdb-many-windows t)       ; auto-switch to multi-window layout on 'gdb' command.

;; c mode tweaks
(condition-case nil (require 'cc-subword) (error nil))

(setq auto-mode-alist (cons '("\\.[hH]\\'" . c++-mode) auto-mode-alist)) ; c++-mode for .h

(defun my-toggle-indent-size ()
  "toggle indent width between 2 and 4 spaces"
  (interactive)
  (let ((new-width (if (= tab-width 4) 2 4)))
    (setq tab-width new-width)
    (setq c-basic-offset new-width)
    (setq sh-basic-offset new-width))
  (message "indent size is now %s" tab-width))
(global-set-key (kbd "C-c TAB") 'my-toggle-indent-size)

(defun c-fill-paragraph-justified ()
  "Equivalent to calling `c-fill-paragraph' with 1 as the argument."
  (interactive)
  (c-fill-paragraph 1))

(defun my-c-mode-hook ()
  (c-set-style "bsd")
  (auto-fill-mode 1)      ; auto-fill-mode only fills comments in c-mode
  (fi-subword-mode 1)
  (if (not (boundp 'c-cleanup-list)) (setq c-cleanup-list (list)))
  (setq c-cleanup-list (cons 'scope-operator c-cleanup-list))
  ;;(setq c-cleanup-list (cons 'space-before-funcall c-cleanup-list))
  (setq c-tab-always-indent nil)
  (setq font-lock-maximum-decoration t)
  (font-lock-mode 1)
  (setq font-lock-auto-fontify t)
  (setq c-basic-offset 2)
  (setq c-backslash-max-column 78) ; max backslash alignment in multiline macros
  (setq c-backslash-column 8)      ; min backslash alignment (default 48)
  (local-set-key (kbd "C-M-q") 'c-fill-paragraph-justified))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(require 'fi-cc-mode)
(add-hook 'c-mode-common-hook 'fi-cc-mode-hook)
(setq c-default-style "bsd")

;; makefile mode tweaks
(defun my-makefile-mode-hook ()
  (setq tab-width 8)
  (local-set-key "\M-n" 'interactive-scroll-up)
  (local-set-key "\M-p" 'interactive-scroll-down))
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

;; replace the default makefile filename regexp in auto-mode-alist
(if (<= emacs-major-version 21)
    (progn
      (setq auto-mode-alist
            (remove '("[Mm]akefile\\'" . makefile-mode) auto-mode-alist))
      (setq auto-mode-alist
            (cons '("[Mm]akefile\\(?:\\(?:\\.|-\\).+\\)?" . makefile-mode) auto-mode-alist)))
  (setq auto-mode-alist
        (remove '("[Mm]akefile\\'" . makefile-gmake-mode) auto-mode-alist))
  (setq auto-mode-alist
        (cons '("[Mm]akefile\\(?:\\(?:\\.|-\\).+\\)?" . makefile-gmake-mode) auto-mode-alist)))

;; assembly mode tweaks
(defconst my-asm-pp-regexp
  "^[ \t]*#[ \t]*\\(?:i\\(?:nclude\\|f\\(?:n?def\\)?\\)\\|else\\|elif\\|endif\\|define\\|undef\\)\\>"
  "Regexp matches preprocessor directives in asm-mode.")

(defun my-find-eol (LIMIT)
  "Searches forward for the end-of-line, starting at `point'.
Returns the end-of-line position if found, else returns the
smaller of LIMIT and end-of-buffer.  Does not search past LIMIT."
  (let ((md nil) (res nil))
    (setq md (match-data))
    (save-excursion (re-search-forward ".*?$" LIMIT 'limit-on-fail)
                    (setq res (point)))
    (set-match-data md) res))

(defun my-asm-match-preprocessor (LIMIT)
  "Matcher for asm-mode preprocessor directives."
  (re-search-forward my-asm-pp-regexp LIMIT 'limit-on-fail))

(defun my-asm-match-comment (LIMIT)
  "Matcher for asm-mode comments that start with `#'."
  (let ((case-fold-search nil))
    (and (re-search-forward "#.*$" LIMIT 'limit-on-fail)
         (save-excursion
           (goto-char (match-beginning 0))
           (beginning-of-line)
           (if (null (my-asm-match-preprocessor (my-find-eol LIMIT)))
               (progn (goto-char (match-beginning 0))
                      (re-search-forward ".*?$" LIMIT 'limit-on-fail))
             (re-search-forward ".*?$" LIMIT 'limit-on-fail)
             (re-search-forward ".*?[\r\n]" LIMIT 'limit-on-fail))))))

(defun my-asm-mode-hook ()
  (font-lock-add-keywords
   nil '((my-asm-match-preprocessor . font-lock-preprocessor-face)
         (my-asm-match-comment 0 font-lock-comment-face prepend)) nil)
  (setq comment-start-skip "\\(?:\\s<+\\|/[/*]+\\|#+\\)[ \t]*") ; add '#' as a comment starter
  (local-set-key "\C-m" 'newline)) ; RET key should do newline, _not_ newline-and-indent
(add-hook 'asm-mode-hook 'my-asm-mode-hook)

;; shell script mode tweaks
(defun my-sh-mode-hook ()
  (local-set-key (kbd "C-c TAB") 'my-toggle-indent-size)
  (setq tab-width 2)
  (setq sh-basic-offset 2))
(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; octave/matlab mode tweaks
(autoload 'fi-octave-mode-hook "fi-octave")
(add-hook 'octave-mode-hook 'fi-octave-mode-hook)

;; text mode tweaks
(defun my-text-mode-hook ()
  (local-set-key (quote [f7]) 'ispell)
  ;(flyspell-mode 1)                  ; hilight misspelled words while typing
  ;(flyspell-buffer)                  ; flyspell what's already in the buffer
  (setq fill-column 70)
  (auto-fill-mode 1))
(add-hook 'text-mode-hook 'my-text-mode-hook)
(setq default-major-mode 'text-mode)    ; unknown file types open in text mode

;; conf mode tweaks
(setq auto-mode-alist (cons '("\\.conf\\'" . conf-unix-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.htaccess\\'" . conf-unix-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.htpasswd\\'" . conf-unix-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ini\\'" . conf-windows-mode) auto-mode-alist))

;; css mode tweaks
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; protobuf mode tweaks
(autoload 'protobuf-mode "protobuf-mode" "Major mode for editing Protocol Buffers." t)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; php mode tweaks
(condition-case nil
    (progn
      (require 'php-mode)
      (setq auto-mode-alist (cons '("\\.php[0-9]*\\'" . php-mode) auto-mode-alist))
      (setq auto-mode-alist (cons '("\\.phtml\\'" . php-mode) auto-mode-alist)))
  (error nil))

;; groovy mode tweaks
(defun my-groovy-mode-hook ()
  ;(require 'groovy-electric)
  ;(groovy-electric-mode)
  nil)
(autoload 'groovy-mode "fi-groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy\\'" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode)) ;; file starts with #!/bin/groovy
(add-hook 'groovy-mode-hook 'my-groovy-mode-hook)

;; lisp stuff
(defun my-lisp-mode-hook ()
  (setq tab-width 2)
  (local-set-key (quote [f6]) 'eval-region)
  (local-set-key (quote [f7]) 'eval-region)
  (local-set-key (kbd "C-M-q") 'fill-paragraph-justified))
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(setq lisp-body-indent 2)

;; version control
(defun my-vc-update ()
  "Calls `vc-update' on the current buffer.  If the buffer was in
read-only mode when this function was called, then the buffer is
switched back to read-only mode after the update."
  (interactive)
  (let ((my-read-only-open buffer-file-read-only)
        (my-read-only-now buffer-read-only))
    (if (and my-read-only-now my-read-only-open)
        (error (concat "File opened in read-only mode.  "
                       "Toggle read-only and try again"))
      (vc-update)
      (if my-read-only-now (toggle-read-only 1)))))

(defun my-vc-update-all ()
  "Calls `my-vc-update' on all open files under source control."
  (interactive)
  (let ((my-entry-buffer (current-buffer)))
    (mapcar (lambda (elem)
              (set-buffer elem)
              (if (or (not (buffer-file-name elem))
                      (not (vc-backend (buffer-file-name elem))))
                  nil
                (condition-case nil (my-vc-update) (error nil))))
            (buffer-list))
    (set-buffer my-entry-buffer)))

(global-set-key (quote [f9]) 'my-vc-update)

;; buffer-selection mode
(defun my-bs-is-point-on-buffer ()
  "Returns t if point is currently on a buffer in bs-mode's
  buffer list.  Returns nil if not."
  (condition-case nil (progn (bs--current-buffer) t) (error nil)))

(defun my-bs-beginning-of-buffer ()
  "Moves point to topmost buffer in bs-mode's buffer list."
  (interactive)
  (goto-char (point-min))
  (if (car (bs-buffer-list)) ; ensure buffer list is non-empty
      (while (not (my-bs-is-point-on-buffer))
        (forward-line))))

(defun my-bs-end-of-buffer ()
  "Moves point to bottommost buffer in bs-mode's buffer list."
  (interactive)
  (goto-char (point-max))
  (forward-line 0) ; move to beginning of line
  (if (car (bs-buffer-list)) ; ensure buffer list is non-empty
      (while (not (my-bs-is-point-on-buffer))
        (forward-line -1))))

(defun my-bs-mode-hook ()
  (local-set-key "\C-p" 'bs-up)
  (local-set-key "\C-n" 'bs-down)
  (local-set-key (kbd "M-<"  ) 'my-bs-beginning-of-buffer)
  (local-set-key (kbd "ESC <") 'my-bs-beginning-of-buffer)
  (local-set-key (kbd "M->"  ) 'my-bs-end-of-buffer)
  (local-set-key (kbd "ESC >") 'my-bs-end-of-buffer))
(add-hook 'bs-mode-hook 'my-bs-mode-hook)

(global-set-key "\C-x\C-b" 'bs-show)  ; bind C-x C-b to bs-show, which is a bit
                                      ; friendlier than list-buffers (default)
(custom-set-variables '(bs-default-sort-name "by name")) ; sort buffers by name

;; info mode
(defun my-info-mode-hook ()
  (local-set-key "\M-n" 'interactive-scroll-up)
  (local-set-key "\M-p" 'interactive-scroll-down)
  (local-set-key "v" 'Info-history-back) ; bound to 'l' (ell) by default
  (local-set-key "l" 'forward-char)
  (local-set-key "h" 'backward-char)
  (local-set-key "j" 'next-line)
  (local-set-key "k" 'previous-line)
  (local-set-key "/" 'isearch-forward-regexp)
  (local-set-key "0" 'beginning-of-line)
  (local-set-key "$" 'end-of-line)
  (local-set-key "g" 'beginning-of-buffer)
  (local-set-key "G" 'end-of-buffer)
  (local-set-key (kbd "M-g M-g") 'Info-goto-node))
(add-hook 'Info-mode-hook 'my-info-mode-hook)

;; start in info-mode if `-info' is passed on the command line
(defun my-info-command-line-handler (&optional SWITCH-STRING)
  (add-hook 'Info-mode-hook (lambda () (local-unset-key "q")))
  (global-set-key "q" (lambda () (interactive) (kill-emacs 0)))
  (setq inhibit-startup-message t)
  (info (car command-line-args-left))
  (setq command-line-args-left nil))
(setq command-switch-alist (cons '("-info" . my-info-command-line-handler) command-switch-alist))

;; indentation/formatting
(setq tab-width 2)
(setq default-tab-width 2)
(setq tab-always-indent nil)
(setq-default fill-column 79)

;; set to "nil" to insert spaces instead of tabs, "t" to use actual tabs
(setq-default indent-tabs-mode nil)

(defun fill-paragraph-justified ()
  "Equivalent to calling `fill-paragraph' with 1 as the argument."
  (interactive)
  (fill-paragraph 1))
(global-set-key (kbd "C-M-q") 'fill-paragraph-justified)

;; auto date insertion stuff
(defun insert-date-string ()
  "Insert a nicely formatted date string."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

;; ui display settings
(column-number-mode 1)                  ; show column number in mode line
(setq line-number-mode t)               ; show line number in mode line
;(setq display-time-day-and-date t)     ; display date in mode line
;(display-time)                         ; display time in mode line

;; font lock (syntax highlighting) settings
(global-font-lock-mode t)
(global-set-key "\M-g\M-g" 'font-lock-fontify-buffer)
(setq font-lock-maximum-size nil)

;; make font-lock more responsive
(setq jit-lock-stealth-time 0.125)      ; fontification occurs if idle for this long (default 3)
(setq jit-lock-stealth-nice 0.0625)     ; seconds to pause between chunks of fontification (default 0.125)
(setq jit-lock-stealth-load 500)        ; min system load to stop fontification (default 200)
(setq jit-lock-chunk-size 1000)         ; fontifies this many characters at a time (default 500)
(setq show-paren-delay 0.0625)          ; delay before hilighting matching parenthesis (default 0.125)

;; more font-lock tweaks, added in emacs >= 22.x
(setq jit-lock-context-time 0.0625)     ; contextually refontifies when idle for this long (default 0.5)
(setq jit-lock-defer-time nil)          ; idle time after which deferred fontification runs (default nil)

;; fidle-lock -- full refontify on idle
(defvar fidle-lock-seconds 0.125
  "If idle fontification is enabled (with `fidle-lock-toggle'),
then the buffer will be fully refontified whenever emacs goes
idle for the number of seconds indicated by this variable.")

(defvar fidle-lock-enabled nil
  "Non-nil if idle refontification is turned on.  Do not set this
variable directly.  Use `fidle-lock-toggle' instead.")
(make-variable-buffer-local 'fidle-lock-enabled)
(setq-default fidle-lock-enabled nil)

(defvar fidle-lock-last-timer nil
  "Used internally by fidle-lock implementation.  Stores the
timer object returned by the last `run-with-idle-timer' call.")
(make-variable-buffer-local 'fidle-lock-last-timer)
(setq-default fidle-lock-last-timer nil)

(defvar fidle-lock-dirty-buffers '()
  "Used internally by fidle-lock implementation.  List of buffers
that have changed since they were last refontified." )

(defun fidle-lock-callback ()
  (let ((message-log-max nil) ; suppress log messages
        (my-buf-list fidle-lock-dirty-buffers))
    (save-excursion
      (while (car my-buf-list)
        (set-buffer (car my-buf-list))
        (setq my-buf-list (cdr my-buf-list))
        (setq fidle-lock-last-timer nil)
        (if (not (window-minibuffer-p)) (font-lock-fontify-buffer)))))
  (setq fidle-lock-dirty-buffers '()))

(defun fidle-lock-after-change (&rest unused)
  (when fidle-lock-enabled
    (if fidle-lock-last-timer (cancel-timer fidle-lock-last-timer))
    (add-to-list 'fidle-lock-dirty-buffers (current-buffer))
    (setq fidle-lock-last-timer
          (run-with-idle-timer fidle-lock-seconds nil 'fidle-lock-callback))))

(defun fidle-lock-toggle (&optional enabled)
  "If ENABLED is nil, then toggle idle refontification.  If
ENABLED is a non-positive number, turn off idle refontification.
If any other value, turn on idle refontification.  When idle
refontification is turned on, the buffer will be fully
refontified (using `font-lock-fontify-buffer') when emacs goes
idle for the time given by `fidle-lock-seconds'."
  (interactive "P")
  (setq enabled (if (or (and enabled (numberp enabled) (<= enabled 0))
                        (and (null enabled) fidle-lock-enabled)) nil t))
  (setq fidle-lock-enabled enabled)
  (setq after-change-functions
        (remove 'fidle-lock-after-change after-change-functions))
  (if enabled
      (add-to-list 'after-change-functions 'fidle-lock-after-change))
  (if (called-interactively-p)
      (message "Idle fontify %s" (or (and enabled "enabled") "disabled"))))
(add-hook 'find-file-hook (lambda () (fidle-lock-toggle t)))

;; check for files on command line
(defun fiu-command-line-files (&optional arg-list)
  "Return a list of filenames specified in command line arguments
ARG-LIST.  If ARG-LIST is nil, use variable `command-line-args'."
  (let (ldouble-dash larg lres)
    (setq arg-list (cdr (or arg-list command-line-args)))
    (setq arg-list (remove t (mapcar (lambda (s) (if (stringp s) s t)) arg-list)))
    (while arg-list
      (setq larg (car arg-list))
      (setq arg-list (cdr arg-list))
      (if ldouble-dash (setq lres (cons larg lres))
        (if (and (>= (length larg) 1) (string= (substring larg 0 1) "-"))
            (setq ldouble-dash (string= larg "--"))
          (setq lres (cons larg lres))))) (reverse lres)))

;; disable startup help screen if files were specified on command line
(setq inhibit-startup-message
      (condition-case nil
          (not (null (fiu-command-line-files command-line-args)))
        (error nil)))

;; remove trailing whitespace on save
;(add-hook 'write-file-hooks (lambda () (delete-trailing-whitespace) nil))

;; blasphemy!
(defun my-viper-load-hook ()
  (viper-set-expert-level 1))
(add-hook 'viper-load-hook 'my-viper-load-hook)
(setq viper-inhibit-startup-message t)
(setq viper-expert-level 1)

;; start in viper-mode if `-vi' or `-view' is passed on the command line
(defun my-viper-command-line-handler (&optional switch-string)
  (if (and switch-string (string-equal switch-string "-view"))
      (add-hook 'find-file-hook (lambda () (toggle-read-only 1))))
  (setq viper-mode t) ; must be set before viper is loaded
  (require 'viper))
(setq command-switch-alist (cons '("-vi" . my-viper-command-line-handler) command-switch-alist))
(setq command-switch-alist (cons '("-view" . my-viper-command-line-handler) command-switch-alist))

;; buffer display settings
(setq default-truncate-lines 1)         ; disable line-wrapping by default
(setq transient-mark-mode 1)            ; highlight marked/selected regions
(show-paren-mode 1)                     ; highlight matching parenthesis

;; misc tweaks
(delete-selection-mode 1)                   ; Delete selection if region is active.
(setq ring-bell-function 'ignore)           ; Turn off system bell.
(setq flyspell-delay 0.5)                   ; Spell checker delay (seconds).
(setq dired-listing-switches "-Alh")        ; Options passed to 'ls' command.

(setq even-window-heights nil)              ; If nil, commands that open a buffer
                                            ; in a non-selected window (for example
                                            ; many help commands) won't change the
                                            ; height of other vertically adjacent
                                            ; windows.

(setq cursor-in-non-selected-windows nil)           ; Only show active window's cursor.
(setq default-cursor-in-non-selected-windows nil)   ; Only show active window's cursor.
(setq cursor-type 'box)                             ; Use bar-style cursor.
(setq default-cursor-type 'box)                     ; Use bar-style cursor.

;; modes to auto-switch to for misc file types
(setq auto-mode-alist (cons '("\\.vcproj\\'" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (remove '("\\.m\\'" . objc-mode) auto-mode-alist)) ; don't want objc-mode for .m
(setq auto-mode-alist (cons '("\\.m\\'" . octave-mode) auto-mode-alist)) ; octave/matlab mode for .m

(global-set-key (kbd "C-c d")   'insert-date-string)
(global-set-key (kbd "C-l")     'redraw-display)
(global-set-key (kbd "M-g M-g") 'goto-line)

;; window title format
(setq frame-title-format "%b - GNU Emacs")

;; don't open buffer-list window if multiple files were given on the command line
(add-hook 'emacs-startup-hook 'delete-other-windows)

;; auto complete
(setq completion-ignore-case t)                 ; case-insensitive completion.
(setq read-file-name-completion-ignore-case t)  ; case-insensitive completion, Emacs >= 22.
(setq read-buffer-completion-ignore-case t)     ; case-insensitive completion, Emacs >= 23.
(setq tags-case-fold-search nil)                ; case-sensitive completion for tags

(require 'etags-select)
(setq etags-select-no-select-for-one-match nil) ; open selection window even if only 1 match
(setq etags-select-use-short-name-completion t) ; foo<TAB> completes Class::foo
(global-set-key (kbd "M-?") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-.") 'etags-select-find-tag)
(global-set-key (kbd "<backtab>") 'complete-tag)

(defun my-etags-select-mode-hook ()
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  ; enable word wrap in tag selection windows
  (if (fboundp 'visual-line-mode)
      (visual-line-mode t))
  ; shrink tag selection window to fit the tag selection buffer
  (if (shrink-window-if-larger-than-buffer)
      (set-window-text-height nil (1+ (window-text-height))))
  (local-set-key (kbd "RET")   'etags-select-goto-tag)
  (local-set-key (kbd "M-RET") 'etags-select-goto-tag-other-window)
  (local-set-key (kbd "C-M-j") 'etags-select-goto-tag-other-window)
  (local-set-key (kbd "C-g")   'etags-select-quit))
(add-hook 'etags-select-mode-hook 'my-etags-select-mode-hook)

;; misc stuff
(global-set-key (kbd "<mouse-2>") 'mouse-major-mode-menu)
(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)

;; my settings for fonts and colors
(require 'fi-my-faces)
