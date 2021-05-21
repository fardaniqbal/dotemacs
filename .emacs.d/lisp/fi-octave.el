;;;
;; fi-octave.el
;;
;; Miscellaneous octave-mode enhancements.  To use, put this file in your
;; load-path and add these lines to your .emacs file:
;; (require 'fi-octave) (add-hook 'octave-mode-hook 'fi-octave-mode-hook)
;;
;; Author(s)    : Fardan Iqbal
;; Updated      : 2010.07.13
;;

(provide 'fi-octave)

;;
;; Custom font lock faces.
;;

(defvar font-lock-fi-punctuation-face 'font-lock-fi-punctuation-face
    "Face name to use for highlighting punctuation.")
(defface font-lock-fi-punctuation-face '((t (:bold t)))
    "Font Lock mode face used for highlighting punctuation."
    :group 'font-lock-faces)

(defvar font-lock-fi-bold-punctuation-face 'font-lock-fi-bold-punctuation-face
    "Face name to use for highlighting bold punctuation.")
(defface font-lock-fi-bold-punctuation-face '((t (:bold t)))
    "Font Lock mode face used for highlighting bold punctuation."
    :group 'font-lock-faces)

(defvar font-lock-fi-number-face 'font-lock-fi-number-face
    "Face name to use for highlighting numbers (e.g. 3, 0.5f, 0xFF1B, etc.).")
(defface font-lock-fi-number-face '((t (:foreground "magenta")))
    "Font Lock mode face used for highlighting numbers (e.g. 3, 0.5f, 0xFF1B,
etc.)."
    :group 'font-lock-faces)

;;
;; "Helper" regexes used by the main syntax matchers.
;;

(defconst fi-octave-rgx-imm-hex "\\(?:0x[0-9a-fA-F]+[uUsSlL]*\\)"
    "Regex matching a C-style \"0x\"-prefixed hexadecimal literal.")
(defconst fi-octave-rgx-imm-radix "\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)"
    "Regex matching a decimal literal, possibly with an explicitly specified radix point.")
(defconst fi-octave-rgx-imm-nothex (concat "\\(?:" fi-octave-rgx-imm-radix
                                           "\\(?:[Ee][+-]?" fi-octave-rgx-imm-radix
                                           "\\)?[fFdDuUsSlL]*\\)")
    "Regex matching a C-style decimal, octal, or floating point literal.")
(defconst fi-octave-rgx-imm-number (concat "\\(?:" fi-octave-rgx-imm-hex "\\|"
                                           fi-octave-rgx-imm-nothex "\\)")
    "Regex matching any valid C-style numeric literal.")

(defconst fi-octave-rgx-idbase "\\(?:[a-zA-Z_][a-zA-Z_0-9]*\\)"
    "Basic fi-octave identifier matcher.")

;;
;; Token Matchers
;;

(defconst fi-octave-match-number-sub (concat "[^a-zA-Z_0-9]\\(" fi-octave-rgx-imm-number "\\)"))

(defun fi-octave-match-number (LIMIT)
    "Matcher for literal numbers (including floating point, hex, and octal)."
    (re-search-forward fi-octave-match-number-sub LIMIT 'limit-on-fail))

;;
;; Octave Mode Hook
;;

(defun fi-octave-init-font-lock ()
  ;(font-lock-add-keywords
  ; nil '(("\\([_a-zA-Z][_a-zA-Z0-9]*\\)[[:space:]]*(" 1 font-lock-function-name-face)) t)
  (font-lock-add-keywords
   nil '((fi-octave-match-number 1 font-lock-fi-number-face)
         ("\\([+-]+\\)"     1 font-lock-fi-punctuation-face)
         ("\\([/*%]\\)"     1 font-lock-fi-punctuation-face)
         ("\\([=;<>]\\)"    1 font-lock-fi-punctuation-face)
         ("\\([?:,!.]\\)"   1 font-lock-fi-punctuation-face)
         ("\\([&|^~]\\)"    1 font-lock-fi-punctuation-face)
         ("\\([{}()]\\)"    1 font-lock-fi-bold-punctuation-face)
         ("\\(\\[\\|\\]\\)" 1 font-lock-fi-bold-punctuation-face)) nil))

(defun fi-octave-mode-hook ()
  (fi-octave-init-font-lock))
