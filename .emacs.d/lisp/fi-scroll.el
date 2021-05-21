;;
;; fi-scroll.el -- Friendlier interactive scrolling and point movement
;;
;; Author(s)    : Fardan Iqbal
;; Updated      : 2010.03.25
;;
;; http://people.cs.vt.edu/~fiqbal/public/emacs/
;; http://fardan.cachethrash.net/
;;
;; Commentary:
;; -----------
;; Makes interactive scrolling (page-up/page-down) easier by keeping the
;; position of point consistent during scrolls.  In particular, the point's
;; vertical position relative to the window will stay the same when scrolling
;; up or down.  The point's horizontal position will also be kept as fixed as
;; possible while scrolling, as long as the line is long enough.
;;
;; For example, while writing a line of code, we might have to scroll up three
;; screens to take a quick glance at a few declarations.  Then, we scroll back
;; down three screens to continue writing.  The point returns exactly to its
;; original position as if no scrolling had been done at all, allowing us to
;; continue coding uninterrupted.
;;
;; This differs from Emacs's default scrolling, which often leaves the point
;; near the top or bottom edge of the window.  This behavior is undesirable
;; because it forces us to manually go back to the original line of code we
;; were editing, which is quite disorienting on larger displays, especially if
;; we have to scroll up/down very frequently.
;;
;; NOTE: It's possible to make emacs somewhat like this without all this code,
;; just by setting "scroll-preserve-screen-position" to non-nil.  I didn't use
;; this built-in functionality because it seems a bit half-assed to me.  This
;; fi-scroll feels much more robust and intuitive.
;;
;; Installation:
;; -------------
;; Place this file in a directory that's listed in your load-path.  File name
;; must be "fi-scroll.el".  Add the following line to your ~/.emacs file:
;;   (require 'fi-scroll)
;;
;; Optionally, include these lines to scroll up/down a few lines at a time:
;;   (global-set-key "\M-n" 'interactive-scroll-up)
;;   (global-set-key "\M-p" 'interactive-scroll-down)
;;
;; Todo:
;; -----
;; - Test if Emacs's horizontal scrolling suffers from the same problems as its
;;   (default) vertical scrolling, and if it does, fix it so it behaves
;;   similarly to the new vertical scrolling.
;;
;; - Make scrolling and cursor movement more intuitive when truncate-lines
;;   (wrap lines longer than window width) is enabled.  This is how `next-line'
;;   and `previous-line' are expected to behave when `line-move-visual' is
;;   non-nil.
;;
;; - Support `auto-window-vscroll' in `next-line' and `previous-line'.
;;
;; - Support `next-line-add-newlines' in `next-line' and `previous-line'.
;;
;; - Show error when calling next-line or previous-line but we're already at
;;   the end or beginning of the buffer, respectively (see `ding' function).
;;
;; Done:
;; -----
;; - Support set-goal-column in `next-line' and `previous-line' replacements.
;;

(provide 'fi-scroll)

;; ----------------------------
;; Point Tracking Variables

;; point's column number after the last non-column-preserving command
(defvar saved-scroll-column nil
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'saved-scroll-column)
(set-default 'saved-scroll-column nil)

;; buffer's modification count after the last column-preserving command
(defvar saved-post-scroll-mod-count 0
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'saved-post-scroll-mod-count)
(set-default 'saved-post-scroll-mod-count 0)

;; point's modification count after the last column-preserving command
(defvar saved-post-scroll-pmod-count 0
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'saved-post-scroll-pmod-count)
(set-default 'saved-post-scroll-pmod-count 0)

;; if non-nil, most recent command was column-preserving
(defvar last-command-was-column-preserving nil
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'last-command-was-column-preserving)
(set-default 'last-command-was-column-preserving nil)

;; number of commands executed in this buffer; may occasionally wrap
(defvar commands-executed-count 0
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'commands-executed-count)
(set-default 'commands-executed-count 0)

;; value of commands-executed-count for the most recent fi-scroll command
(defvar last-column-preserving-command-count -1
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'last-column-preserving-command-count)
(set-default 'last-column-preserving-command-count -1)

;; ----------------------------
;; Scroll State Variables

;; value of commands-executed-count for the most recent scroll up/down command
(defvar last-scroll-command-count -1
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'last-scroll-command-count)
(set-default 'last-scroll-command-count -1)

;; if non-nil, last scroll couldn't complete all the way (stuck at beginning or
;; end of buffer)
(defvar last-scroll-got-stuck nil
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'last-scroll-got-stuck)
(set-default 'last-scroll-got-stuck nil)

;; direction of the most recent scroll that got stuck
(defvar last-stuck-scroll-direction 1
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'last-stuck-scroll-direction)
(set-default 'last-stuck-scroll-direction 1)

;; position of point after the most recent scroll that got stuck
(defvar last-stuck-scroll-point 1
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'last-stuck-scroll-point)
(set-default 'last-stuck-scroll-point 1)

;; position of window start after the most recent scroll that got stuck
(defvar last-stuck-scroll-window-start 1
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'last-stuck-scroll-window-start)
(set-default 'last-stuck-scroll-window-start 1)

;; ----------------------------
;; Post Command State Variables

;; value of point before the most recent command.
(defvar saved-pre-command-point 0
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'saved-pre-command-point)
(set-default 'saved-pre-command-point 0)

;; value of buffer-modified-tick before most recent command.
(defvar saved-pre-command-mod-count 0
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'saved-pre-command-mod-count)
(set-default 'saved-pre-command-mod-count 0)

;; number of times the point has been modified; may wrap on occasion.
(defvar point-modified-count 0
    "Used internally by custom scrolling functions to keep the point's
horizontal position consistent between scrolls.")
(make-variable-buffer-local 'point-modified-count)
(set-default 'point-modified-count 0)

;; returns point-modified-count, for constistency with buffer-modified-tick.
(defun point-modified-tick ()
    "Number of times the point has moved.  May wrap around occassionaly."
    point-modified-count)


;; -------------------------------
;; Helper Subroutines

(defun points-are-on-same-line (POINT1 POINT2)
    "Return non-nil if POINT1 and POINT2 both lie on the same line of the
current buffer."
    (let ((case-fold-search nil)
          (lcreturn  (string-to-char "\r"))
          (llinefeed (string-to-char "\n"))
          (lstrchar   nil) (lreturnval t)
          (lsubstring nil) (lstrlength nil)
          (lstrindex  nil) (lnextindex nil))
        (setq lsubstring (buffer-substring-no-properties POINT1 POINT2))
        (setq lstrlength (length lsubstring))
        (setq lstrindex 0)
        (while (< lstrindex lstrlength)
            (setq lnextindex (1+ lstrindex))
            (setq lstrchar (string-to-char (substring lsubstring lstrindex lnextindex)))
            (if (null (or (char-equal lstrchar lcreturn)
                          (char-equal lstrchar llinefeed)))
                    (setq lstrindex lnextindex)
                (setq lstrindex lstrlength)
                (setq lreturnval nil))) lreturnval))

(defun count-lines-between-points (POINT1 POINT2)
    "Return the number of line breaks in the current buffer between POINT1 and
POINT2."
    (let ((case-fold-search nil)
          (llinefeed (string-to-char "\n"))
          (lstrchar   nil) (lcount 0)
          (lsubstring nil) (lstrlength nil)
          (lstrindex  nil) (lnextindex nil))
        (setq lsubstring (buffer-substring-no-properties POINT1 POINT2))
        (setq lstrlength (length lsubstring))
        (setq lstrindex 0)
        (while (< lstrindex lstrlength)
            (setq lnextindex (1+ lstrindex))
            (setq lstrchar (string-to-char (substring lsubstring lstrindex lnextindex)))
            (if (char-equal lstrchar llinefeed)
                    (setq lcount (1+ lcount)))
            (setq lstrindex lnextindex)) lcount))

(defun pre-command-fi-scroll-check-state ()
    "Called before every command to keep fi-scroll's internal state up to date."
    (setq saved-pre-command-point (point))
    (setq saved-pre-command-mod-count (buffer-modified-tick)))
(add-hook 'pre-command-hook 'pre-command-fi-scroll-check-state)

(defun post-command-fi-scroll-check-state ()
    "Called after every command to keep fi-scroll's internal state up to date."
    (let ((fi-lpoint (point))
          (fi-lmod-count (buffer-modified-tick))
          (fi-lold-column-preserving last-command-was-column-preserving))
        (setq last-command-was-column-preserving nil)
        (if (= fi-lpoint saved-pre-command-point)
                (progn (setq last-command-was-column-preserving fi-lold-column-preserving))
            (if (or
                 ;; if the command explicitly preserved the point's column
                 (= last-column-preserving-command-count commands-executed-count)
                 ;; if the command moved the point to a different line AND left
                 ;; the buffer unmodified
                 (and nil   ; !!! EDIT: disregard that last comment !!!
                      (= fi-lmod-count saved-pre-command-mod-count)
                      (not (points-are-on-same-line fi-lpoint saved-pre-command-point))))
                    (setq last-command-was-column-preserving t))
            (setq point-modified-count (1+ point-modified-count)))
        (setq commands-executed-count (1+ commands-executed-count))))
(add-hook 'post-command-hook 'post-command-fi-scroll-check-state)

(defun column-preserve-begin ()
    "Common initialization routine used by column-preserving commands.  Sets
saved-scroll-column to the column to which the point should attempt to go
after the command finishes"
    (if (or
         ;; if this is the first time running in this buffer
         (null saved-scroll-column)
         ;; if last command wasn't column-preserving AND it either modified
         ;; the buffer or moved the point
         (and (not last-command-was-column-preserving)
              (or (not (= saved-post-scroll-mod-count (buffer-modified-tick)))
                  (not (= saved-post-scroll-pmod-count (point-modified-tick))))))
            (setq saved-scroll-column (current-column))))

(defun column-preserve-finish ()
    "Some clean-up code common to the column-preserving commands."
    (end-of-line)
    ;; this is a bit tricky because some characters (such as tabs) are
    ;; more than a single column wide
    (while (> (current-column) saved-scroll-column)
        (backward-char))
    (setq saved-post-scroll-mod-count (buffer-modified-tick))
    (setq saved-post-scroll-pmod-count (point-modified-tick))
    (setq last-column-preserving-command-count commands-executed-count))


;; ----------------------------
;; Scroll Function Replacements

;; page down without losing the point position relative to the window.
(defun scroll-up (&optional ARG)
    "Scroll text of current window upward ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
    (interactive "P")
    (column-preserve-begin)
    (let ((max-lines 1) (scroll-direction 1)
          (check-for-stuck t)
          (window-line-changed nil)
          (window-primal (window-start))
          (final-point   (point)) (original-point (point))
          (lines-left 0) (w-lines-left 0))
        (if (null (numberp ARG))
                (progn (setq scroll-direction (if (null ARG) 1 -1))
                       (setq max-lines (- (window-text-height) next-screen-context-lines))
                       (if (< max-lines 0) (setq max-lines 0)))
            (if (> ARG 0) (setq scroll-direction 1)
                (setq scroll-direction -1)
                (setq ARG (- ARG)))
            (setq max-lines ARG))
        (if (not (= (1- commands-executed-count) last-scroll-command-count))
                (setq last-scroll-got-stuck nil))
        (setq last-scroll-command-count commands-executed-count)
        (save-excursion
            (goto-char window-primal)
            (beginning-of-line)
            (setq window-primal (point)))
        (if last-scroll-got-stuck (progn)
            (save-excursion
                (setq lines-left (forward-line (* max-lines scroll-direction)))
                (if (= lines-left 0) (setq final-point (point))))
            (goto-char final-point))
        (save-excursion
            (goto-char window-primal)
            (setq w-lines-left (forward-line (* max-lines scroll-direction)))
            (beginning-of-line)
            (setq new-window-start (point)))
        ;; if last scroll got stuck, check if this scroll "unstuck" it
        (if (and last-scroll-got-stuck
                 (not (= scroll-direction last-stuck-scroll-direction))
                 (or (and (= scroll-direction 1)
                          (>= new-window-start last-stuck-scroll-window-start))
                     (and (= scroll-direction -1)
                          (<= new-window-start last-stuck-scroll-window-start))))
                (progn (setq check-for-stuck nil)
                       (setq last-scroll-got-stuck nil)
                       (setq final-point last-stuck-scroll-point)
                       (setq new-window-start last-stuck-scroll-window-start)))
        (set-window-start (selected-window) new-window-start t)
        (goto-char final-point)
        (if (not check-for-stuck) (progn)
            (if (and (not (and (= window-primal (window-start))
                               (= original-point final-point)))
                     (not (= (count-lines-between-points window-primal original-point)
                             (count-lines-between-points (window-start) final-point))))
                    (setq window-line-changed t))
            ;; if we couldn't scroll as far as requested, next scroll command  will
            ;; behave slightly differently
            (if (not window-line-changed)
                    (setq last-scroll-got-stuck nil)
                ;; if previous scroll was already stuck, no need to update
                (if last-scroll-got-stuck (progn)
                    (setq last-stuck-scroll-direction scroll-direction)
                    (setq last-stuck-scroll-point original-point)
                    (setq last-stuck-scroll-window-start window-primal)
                    (setq last-scroll-got-stuck t)))))
    (column-preserve-finish))

;; page up without losing the point position relative to the window.
(defun scroll-down (&optional ARG)
    "Scroll text of current window down ARG lines; or near full screen if no ARG.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
    (interactive "P")
    (scroll-up (if (null ARG) "-" (if (numberp ARG) (- ARG) nil))))

;; helper functions used by next-line and previous-line
(defun fi-scroll-check-goal-column ()
    (if (or (null goal-column)
            (and (not (numberp goal-column))
                 (not (markerp goal-column)))) (progn)
        (setq saved-scroll-column goal-column)))

;; column-preserving versions of next-line command
(defun next-line (ARG)
    "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer.

The command C-x C-n can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
    (interactive "p")
    (fi-scroll-check-goal-column)
    (column-preserve-begin)
    (forward-line ARG)
    (column-preserve-finish))

;; column-preserving version of previous-line command
(defun previous-line (ARG)
    "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command C-x C-n can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
    (interactive "p")
    (fi-scroll-check-goal-column)
    (column-preserve-begin)
    (forward-line (- ARG))
    (column-preserve-finish))

;; column-preserving scrolling
(defun interactive-scroll-up (ARG)
    "TODO: Write documentation."
    (interactive "P")
    (let ((fi-lnum-lines ARG)
          (fi-ldirection 1)
          (fi-lrunaway 100)
          (fi-lwindow (selected-window)))
        (if (null fi-lnum-lines) (setq fi-lnum-lines 4))
        (if (< fi-lnum-lines 0) (setq fi-ldirection -1))
        (if (= fi-lnum-lines 0) (progn)
            (save-excursion (scroll-up fi-lnum-lines))
            (while (and (> fi-lrunaway 0)
                        (null (pos-visible-in-window-p (point) fi-lwindow)))
                (next-line fi-ldirection)
                (setq fi-lrunaway (1- fi-lrunaway))))))

(defun interactive-scroll-down (ARG)
    "TODO: Write documentation."
    (interactive "P")
    (if (null ARG) (setq ARG 4))
    (interactive-scroll-up (- ARG)))
