;;
;; fi-cc-mode.el
;;
;; Miscellaneous cc-mode enhancements. Mostly syntax-highlighting improvements.
;; To use, put this file in your load-path add these lines to your .emacs:
;; (require 'fi-cc-mode) (add-hook 'c-mode-common-hook 'fi-cc-mode-hook)
;;
;; Author(s)    : Fardan Iqbal
;; Updated      : 2016.12.29
;;

(provide 'fi-cc-mode)
(require 'font-lock)

(defcustom fi-cc-mode-highlight-namespace-p nil
  "*If non-nil, add extra highlighting to namespaces in
namespace-qualified symbols in C++ mode.  E.g., highlight FOO and
BAR in FOO::BAR::func()."
  :group 'fi-cc-mode
  :type 'boolean)

;; ----------------------------
;; Misc Subroutines

(defun fi-cc-mode-rgx-generate-nest-matcher (OPENSTR CLOSESTR MAXDEPTH)
    "Generates a regex that matches a block of text starting with OPENSTR, going
all the way to CLOSESTR.  Recursively nested blocks of text are taken into
consideration ('nested text' being a block of text bounded by OPENSTR and
CLOSESTR that is part of a larger block of text also bounded by OPENSTR and
CLOSESTR).

For example, if OPENSTR = \"{\" and CLOSESTR = \"}\", the returned regex would
match text between (and including) curly braces, correctly handling any nested
curly braces it might encounter.  However, due to limitations of regular
expressions, the returned regex will only match up to a finite nesting depth.
This depth limit is specified by MAXDEPTH."
    (let ((fi-lskipper nil)
          (fi-lfirstrun nil)
          (fi-lreturn nil))
        ;(setq OPENSTR  (regexp-quote OPENSTR))
        ;(setq CLOSESTR (regexp-quote CLOSESTR))
        (setq fi-lskipper (concat "[^" OPENSTR CLOSESTR "]*"))
        (setq fi-lfirstrun t)
        (setq fi-lreturn "\\(?:\\)")
        (while (> MAXDEPTH 0)
            (if (null fi-lfirstrun)
                    (setq fi-lreturn (concat fi-lskipper "\\(?:" fi-lreturn fi-lskipper "\\)*"))
                (setq fi-lfirstrun nil)
                (setq fi-lreturn fi-lskipper))
            (setq fi-lreturn (concat "\\(?:" OPENSTR fi-lreturn CLOSESTR "\\)"))
            (setq MAXDEPTH (1- MAXDEPTH)))
        fi-lreturn))


;; ----------------------------
;; Font-Lock Tweaks

;;
;; Custom font lock faces.
;;

(defvar font-lock-fi-punctuation-face 'font-lock-fi-punctuation-face
    "Face name to use for highlighting punctuation.")
(defface font-lock-fi-punctuation-face '((t (:bold t)))
    "Font Lock mode face used for highlighting punctuation."
    :group 'font-lock-faces)

(defvar font-lock-fi-number-face 'font-lock-fi-number-face
    "Face name to use for highlighting numbers (e.g. 3, 0.5f, 0xFF1B, etc.).")
(defface font-lock-fi-number-face '((t (:foreground "red")))
    "Font Lock mode face used for highlighting numbers (e.g. 3, 0.5f, 0xFF1B,
etc.)."
    :group 'font-lock-faces)

(defvar font-lock-fi-constant-face 'font-lock-fi-constant-face
    "Face name to use for highlighting constants (fi-cc-mode).")
(defface font-lock-fi-constant-face '((t (:foreground "red")))
    "Font Lock mode face used for highlighting constants (fi-cc-mode)."
    :group 'font-lock-faces)

;; The constant-face hilighting looks bad in cc-mode (especially in java-mode),
;; so disable it.  We'll provide our own constant hilighting.
;(custom-set-faces '(font-lock-constant-face ((t (:foreground "unspecified-fg")))))

;;
;; "Helper" regexes used by the main syntax matchers.
;;

(defconst fi-cc-mode-rgx-curlyb (fi-cc-mode-rgx-generate-nest-matcher "{" "}" 15)
    "Regex matching a block of text bounded by { curly braces }.")
(defconst fi-cc-mode-rgx-angleb (fi-cc-mode-rgx-generate-nest-matcher "<" ">" 5)
    "Regex matching a block of text bounded by < angle brackets >.")
(defconst fi-cc-mode-rgx-squareb (fi-cc-mode-rgx-generate-nest-matcher "\\[" "\\]" 4)
    "Regex matching a block of text bounded by [ square brackets ].")

(defconst fi-cc-mode-rgx-imm-hex "\\(?:0x[0-9a-fA-F]+[uUsSlL]*\\)"
    "Regex matching a C-style \"0x\"-prefixed hexadecimal literal.")
(defconst fi-cc-mode-rgx-imm-radix "\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)"
    "Regex matching a decimal literal, possibly with an explicitly specified radix point.")
(defconst fi-cc-mode-rgx-imm-nothex (concat "\\(?:" fi-cc-mode-rgx-imm-radix
                                            "\\(?:[Ee][+-]?" fi-cc-mode-rgx-imm-radix
                                            "\\)?[fFdDuUsSlL]*\\)")
    "Regex matching a C-style decimal, octal, or floating point literal.")
(defconst fi-cc-mode-rgx-imm-number (concat "\\(?:" fi-cc-mode-rgx-imm-hex "\\|"
                                            fi-cc-mode-rgx-imm-nothex "\\)")
    "Regex matching any valid C-style numeric literal.")

(defconst fi-cc-mode-rgx-idbase "\\(?:[a-zA-Z_][a-zA-Z_0-9]*\\)"
    "Basic fi-cc-mode identifier matcher.")
(defconst fi-cc-mode-rgx-idcpp (concat "\\(?:[a-zA-Z_][a-zA-Z_0-9]*[[:space:]]*"
                                       fi-cc-mode-rgx-angleb "?\\(?:[[:space:]]*"
                                       "\\:\\:[[:space:]]*[a-zA-Z_][a-zA-Z_0-9]*[[:space:]]*"
                                       fi-cc-mode-rgx-angleb "?\\)*\\)")
    "C++ identifier matcher for fi-cc-mode.")
;(defconst fi-cc-mode-rgx-typedef-typename "")

;;
;; Main syntax matchers.
;;

(defvar fi-cc-mode-namespace-limit 1
    "Used internally by fi-cc-mode for highlighting C++ namespace tokens.  Do
not modify.")
(make-variable-buffer-local 'fi-cc-mode-namespace-limit)
(set-default 'fi-cc-mode-namespace-limit 1)

(defconst fi-cc-mode-match-c++-defun-sub
    (concat "\\(operator\\(?:\\(?:[[:space:]]+new\\|delete\\|sizeof\\)\\|"
            "\\(?:[[:space:]]*[=<>/*%?:!&|^~.+-]+\\)\\)\\|"
            "~?[_a-zA-Z][_a-zA-Z0-9]*\\)[[:space:]]*("))

(defconst fi-cc-mode-match-namespace-sub
    (concat "\\([_a-zA-Z][_a-zA-Z0-9]*\\)[[:space:]]*"
            fi-cc-mode-rgx-angleb
            "?[[:space:]]*::[[:space:]]*"))

(defconst fi-cc-mode-match-namespace-full-sub
    (concat fi-cc-mode-match-namespace-sub "\\(?:"
            fi-cc-mode-match-namespace-sub "\\)*"
            fi-cc-mode-match-c++-defun-sub))

(defconst fi-cc-mode-match-number-sub (concat "[^a-zA-Z_0-9]\\(" fi-cc-mode-rgx-imm-number "\\)"))

(defun fi-cc-mode-match-c++-defun (LIMIT)
    "Matcher for C++ function signatures."
    (re-search-forward fi-cc-mode-match-c++-defun-sub LIMIT 'limit-on-fail))

(defun fi-cc-mode-match-namespace (LIMIT)
    "Matcher for C++ namespace tokens (e.g., FOO, BAR, and ASDF in
'FOO::BAR::ASDF::blah (int something)...')."
    (let ((fi-lstartpt   (point))
          (fi-lreturn    (point))
          (fi-lmatchbeg   nil)
          (fi-lmatchbeg-s nil)
          (fi-ldone       nil))
        (setq LIMIT (min LIMIT fi-cc-mode-namespace-limit))
        (setq fi-lreturn (re-search-forward fi-cc-mode-match-namespace-full-sub LIMIT 'limit-on-fail))
        (setq fi-lmatchbeg (match-beginning 1))
        (if (or (null fi-lreturn) (null fi-lmatchbeg)) nil
            (goto-char fi-lstartpt)
            (while (not fi-ldone)
                (setq fi-lreturn (re-search-forward fi-cc-mode-match-namespace-sub LIMIT 'limit-on-fail))
                (setq fi-lmatchbeg-s (match-beginning 1))
                (setq fi-ldone (or (null fi-lreturn) (null fi-lmatchbeg-s)
                                   (>= fi-lmatchbeg-s fi-lmatchbeg))))
            fi-lreturn)))

(defun fi-cc-mode-match-number (LIMIT)
    "Matcher for C-style literal numbers (including floating point, hex, and octal)."
    (re-search-forward fi-cc-mode-match-number-sub LIMIT 'limit-on-fail))


;; ----------------------------
;; Mode Change Hook

(defun fi-cc-mode-hook ()
  ;; Custom constant hilighting (because default is buggy).
  (font-lock-add-keywords
   nil '(("\\<\\(true\\|false\\)\\>" 1 font-lock-fi-constant-face)) nil)
  (if (equal major-mode 'java-mode)
      ;; Special handling required for Java.
      (progn (font-lock-add-keywords
              nil '(;; Hilight a few keywords as types so declarations look consistent.
                    ("\\<\\(static\\|final\\|abstract\\|synchronized\\)\\>" 1 font-lock-type-face)
                    ("\\<\\(public\\|private\\|protected\\)\\>" 1 font-lock-type-face)
                    ("\\<\\(volatile\\|transient\\|native\\|strictfp\\)\\>" 1 font-lock-type-face)
                    ("\\<\\(throws\\|class\\|interface\\|enum\\)\\>" 1 font-lock-type-face)
                    ;; First, hilight the keywords that may be mistaken for function calls.
                    ("\\<\\(assert\\|case\\|catch\\|if\\|for\\|instanceof\\|return\\|super\\|switch\\|this\\|throw\\|while\\|null\\)\\>" 1 font-lock-keyword-face)
                    ;; Now it's safe to hilight function calls.
                    ("\\([_a-zA-Z][_a-zA-Z0-9]*\\)[[:space:]]*(" 1 font-lock-function-name-face)) nil))
    (font-lock-add-keywords
     nil '(("\\<\\(TRUE\\|FALSE\\|NULL\\)\\>" 1 font-lock-fi-constant-face)) nil)
    (font-lock-add-keywords
     nil '(("\\([_a-zA-Z][_a-zA-Z0-9]*\\)[[:space:]]*(" 1 font-lock-function-name-face)) t)
    (when (and (equal major-mode 'c++-mode) fi-cc-mode-highlight-namespace-p)
      ;; Special handling required for C++ (namespaces, operator
      ;; overloading, templates, destructors...).
      (font-lock-add-keywords
       nil '((fi-cc-mode-match-c++-defun
              (1 font-lock-function-name-face nil t)
              (fi-cc-mode-match-namespace
               (progn (setq fi-cc-mode-namespace-limit (point))
                      (forward-line 0))
               (goto-char fi-cc-mode-namespace-limit)
               (1 font-lock-function-name-face t t)))) t)))

  ;; Common compiler extensions.
  (when (or (equal major-mode 'c-mode) (equal major-mode 'c++-mode))
    (font-lock-add-keywords
     nil '(("\\<\\(__cdecl\\|__stdcall\\|__fastcall\\|__thiscall\\)\\>" 1 font-lock-keyword-face)
           ) nil))
  ;; Hilight a few C and C++ keywords as types so declarations look consistent.
  (when (or (equal major-mode 'c-mode) (equal major-mode 'c++-mode))
    (font-lock-add-keywords
     nil '(("\\<\\(inline\\|restrict\\|const\\|volatile\\)\\>" 1 font-lock-type-face)
           ("\\<\\(auto\\|register\\|static\\|extern\\)\\>" 1 font-lock-type-face)
           ("\\<\\(struct\\|union\\|enum\\|typedef\\)\\>" 1 font-lock-type-face)
           ) nil))
  ;; Same, but for C++-specific keywords.
  (when (equal major-mode 'c++-mode)
    (font-lock-add-keywords
     nil '(("\\<\\(explicit\\|mutable\\|virtual\\)\\>" 1 font-lock-type-face)
           ("\\<\\(class\\|namespace\\|template\\|typename\\)\\>" 1 font-lock-type-face)
           ) nil))
  ;; Extra hilighting (numbers, punctuation, etc.).
  (font-lock-add-keywords
   nil '((fi-cc-mode-match-number 1 font-lock-fi-number-face)
         ("\\([+-]+\\)" 1 font-lock-fi-punctuation-face)
         ("\\([/*%]\\)" 1 font-lock-fi-punctuation-face)
         ("\\([=;<>]\\)" 1 font-lock-fi-punctuation-face)
         ("\\([?:,!.]\\)" 1 font-lock-fi-punctuation-face)
         ("\\([&|^~]\\)" 1 font-lock-fi-punctuation-face)
         ("\\([{}()]\\)" 1 font-lock-fi-punctuation-face)
         ("\\(\\[\\|\\]\\)" 1 font-lock-fi-punctuation-face)
         ) nil);t)
  ;; Don't let punctuation face override the <brackets> in #include <file.h>.
  (font-lock-add-keywords
   nil '(("^\\s-*#\\s-*include\\s-*\\(<[^>]*>\\)" 1 font-lock-string-face)
         ) nil))


;; ----------------------------
;; Old testing stuff

;(message "\"%s\"" (replace-regexp-in-string "\\\\" "\\\\\\\\" (concat "typedef[[:space:]]+\\(?:\\(?:"
;                                                                      fi-cc-mode-rgx-idbase "[[:space:]]*\\)*?"
;                                                                      fi-cc-mode-rgx-idcpp  "[[:space:]]*\\)?")))
;(message "\"%s\"" (replace-regexp-in-string "\\\\" "\\\\\\\\"
;                                            (concat "\\(?:[a-zA-Z_][a-zA-Z_0-9]*[[:space:]]*"
;                                                    (fi-cc-mode-rgx-generate-nest-matcher "<" ">" 7)    "?\\(?:[[:space:]]*"
;                                                    "\\:\\:[[:space:]]*[a-zA-Z_][a-zA-Z_0-9]*[[:space:]]*"
;                                                    (fi-cc-mode-rgx-generate-nest-matcher "<" ">" 7) "?\\)*\\)")))
;(message "\"%s\"" (replace-regexp-in-string "\\\\" "\\\\\\\\"
;                                            (fi-cc-mode-rgx-generate-nest-matcher "<" ">" 7)))


;; ----------------------------
;; Misc. font-lock regex/face pairs. Might come in handy in the future.
;;
;; A lot of this stuff was borrowed from here:
;; lynx.sao.ru/~karpov/software/emacs_font_lock.html

;("^\\(#[ \t]*include[ \t]+[\<\"]\\)\\(.*\\)\\([\>\"]\\)" (1 font-lock-preprocessor-face) (2 font-lock-string-face) (3 font-lock-preprocessor-face))
;("^\\(#[ \t]*define[ \t]+[a-zA-Z0-9_]+\\)" (1 font-lock-preprocessor-face))
;("^\\(#[ \t]*ifn?def[ \t]+[a-zA-Z0-9_]+\\)" (1 font-lock-preprocessor-face))
;("[ \t\(\{]\\([a-zA-Z0-9_]+_str\\)[ \t\)\*]" (1 font-lock-type-face))
;("[ \t\(\{]\\(if\\|for\\|switch\\|case\\|while\\|sizeof\\)[ \t\(]" 1 font-lock-keyword-face)
;("[ \t\(\{]\\(class\\|public\\|protected\\|private\\|slots\\)[ \t\)\:]" 1 font-lock-keyword-face)
;("[ \t\(\{]\\(stdin\\|stdout\\|stderr\\|NULL\\)[ \t\)\;\,]" 1 font-lock-keyword-face)
;("\\(\\.\\.\\.\\)" 1 font-lock-function-name-face)
;("\\]\\(\.\\)" 1 font-lock-fi-punctuation-face)

;("[^a-zA-Z_0-9\)]\\(0x[0-9a-fA-F]+\\)[^a-zA-Z_0-9\(]" 1 font-lock-fi-number-face)
;("[^a-zA-Z_0-9\)]\\([0-9\.]+\\(e[+-]?[0-9]*\\)?[fFdDsSlL]?\\)[^a-zA-Z_0-9\(]" 1 font-lock-fi-number-face)
;("\\([_a-zA-Z][_a-zA-Z0-9]*\\)[ \t\r\n]*(" 1 font-lock-function-name-face)

;("\\([\+\-]\\)" 1 font-lock-fi-punctuation-face)
;("\\(\/\\|\*\\)" 1 font-lock-fi-punctuation-face)
;("\\([\?\:\,\!\.]\\)" 1 font-lock-fi-punctuation-face)
;("\\([=\;\{\}\<\>\(\)]\\)" 1 font-lock-fi-punctuation-face)
;("\\(\\[\\|\\]\\|\&\\|\|\\)" 1 font-lock-fi-punctuation-face)
;("\\(\"\\|\'\\)" 1 font-lock-string-face)

;; EOF
