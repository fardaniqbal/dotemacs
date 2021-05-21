;;
;; fi-my-faces.el -- My face settings (fonts, colors, etc.)
;;
;; Author(s)    : Fardan Iqbal
;; Updated      : 2017.01.04
;;
;; Installation:
;; -------------
;; Place this file in a directory that's listed in your load-path.  File name
;; must be "fi-my-faces.el".  Add the following line to your ~/.emacs file:
;;   (require 'fi-my-faces)
;;
;; TODO:
;; -----
;; - Add support for dark-on-light color scheme.
;;

(provide 'fi-my-faces)

;;
;; ----------------------------
;;

(defun fi-x-list-fonts (PATTERN)
    "Like `x-list-fonts', except returns nil in situations where
`x-list-fonts' would throw an error."
    (condition-case nil (x-list-fonts PATTERN) (error nil)))

(defconst fi-my-fixed-font (or (and (null window-system)
                                    "-*-fixed-medium-r-*-*-*-*-80-*-*-*-*-*")
                               (and (fi-x-list-fonts ".*ProFontWindows.*")
                                    "-*-ProFontWindows-medium-r-*-*-11-*-*-*-*-*-*-*")
                               (and (fi-x-list-fonts ".*ProFont.*")
                                    "-*-ProFont-medium-r-*-*-11-*-*-*-*-*-*-*")
                               (and (fi-x-list-fonts ".*Lucida Console.*")
                                    "-*-Lucida Console-medium-r-*-*-12-*-*-*-*-*-*-*")
                               "-*-Monospace-medium-r-*-*-*-90-*-*-*-*-*-*")
    "My preferred fixed-width font.")
(defconst fi-my-proportional-font (or (and (null window-system)
                                           "-*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*")
                                      (and (fi-x-list-fonts ".*helvetica.*")
                                           "-*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*")
                                      (and (fi-x-list-fonts ".*Arial.*")
                                           "-*-Arial-medium-r-*-*-12-*-*-*-*-*-*-*")
                                      "-*-Sans Serif-medium-r-*-*-*-90-*-*-*-*-*-*")
    "My preferred proportional font.")

;;
;; ----------------------------
;; Define button/widget faces beforehand.

(defface custom-button '((t (:box (:line-width 1 :style released-button)
                                  :foreground "black"
                                  :background "lightgrey"
                                  :inherit    variable-pitch)))
    "Normal button face.")
(defface custom-button-pressed '((t (:box (:line-width 1 :style pressed-button)
                                          :inherit custom-button)))
    "Pressed button face.")
(defface custom-button-mouse '((t (:background "gray90" :inherit custom-button)))
    "Mouse rollover button face.")

;;
;; ----------------------------
;;

(defun eval-no-err (BODY)
  "Evaluate BODY until it throws an error or finishes executing.
Errors are silently discarded.  Returns the result of evaluating
BODY, or nil on error."
  (condition-case nil (eval BODY) (error nil)))

(defun get-foreground-color (&optional FRAME)
  "Returns the foreground color of frame FRAME."
    (condition-case nil
        (let ((lreturn 'unspecified-fg)
              (lframe-params (frame-parameters FRAME))
              (lparam nil))
          (while lframe-params
            (setq lparam (car lframe-params))
            (setq lframe-params (cdr lframe-params))
            (when (and lparam (car lparam) (equal (car lparam) 'foreground-color))
              (setq lreturn (cdr lparam))
              (setq lframe-params nil))) lreturn)
      (error 'unspecified-fg)))

;;
;; ----------------------------
;;

;; Define some color constants based on terminal colors.  This helps us
;; maintain a consistent look between terminal-mode emacs and gui emacs.
(defconst fi-vtforeground      "#BCBFB8")
(defconst fi-vtforeground-bold "#FFFFFF")
(defconst fi-vtbackground      "#2E3436")
(defconst fi-vtblack        "#000000")
(defconst fi-vtblack-bold   "#555753")
(defconst fi-vtred          "#CC0000")
(defconst fi-vtred-bold     "#EF2929")
(defconst fi-vtgreen        "#4E9A06")
(defconst fi-vtgreen-bold   "#8AE234")
(defconst fi-vtyellow       "#C4A000")
(defconst fi-vtyellow-bold  "#FCE94F")
(defconst fi-vtblue         "#3465A4")
(defconst fi-vtblue-bold    "#729FCF")
(defconst fi-vtmagenta      "#75507B")
(defconst fi-vtmagenta-bold "#AD7FA8")
(defconst fi-vtcyan         "#08C9CC")
(defconst fi-vtcyan-bold    "#34E2E2")
(defconst fi-vtwhite        "#BCBFB8")
(defconst fi-vtwhite-bold   "#FFFFFF")

(eval-no-err '(set-face-attribute  'font-lock-comment-delimiter-face nil ; emacs >= 22
                                   :foreground 'unspecified
                                   :inherit    'font-lock-comment-face))

;; if emacs is running in a terminal
(if window-system (progn)
  (eval-no-err '(set-face-foreground 'font-lock-variable-name-face  "cyan"))
  (eval-no-err '(set-face-foreground 'font-lock-comment-face        "blue"))
  (eval-no-err '(set-face-foreground 'font-lock-doc-face            "blue"))
  (eval-no-err '(set-face-foreground 'font-lock-builtin-face        "magenta"))
  (eval-no-err '(set-face-foreground 'font-lock-function-name-face  (get-foreground-color)))
  (eval-no-err '(set-face-foreground 'font-lock-keyword-face        "yellow"))
  (eval-no-err '(set-face-foreground 'font-lock-string-face         "red"))
  (eval-no-err '(set-face-background 'show-paren-match-face         "cyan"))
  (eval-no-err '(set-face-background 'show-paren-mismatch-face      "magenta"))
  (eval-no-err '(set-face-foreground 'menu                          "gray75"))
  (eval-no-err '(set-face-background 'menu                          "black"))
  (eval-no-err '(set-face-foreground 'mode-line                     "gray75"))
  (eval-no-err '(set-face-background 'mode-line                     "black"))
  (eval-no-err '(set-face-foreground 'minibuffer-prompt             "cyan")) ; emacs >= 22
  (eval-no-err '(set-face-bold-p     'minibuffer-prompt             t))
  (eval-no-err '(set-face-bold-p     'font-lock-builtin-face        nil))
  (eval-no-err '(set-face-bold-p     'font-lock-keyword-face        nil))
  (eval-no-err '(set-face-bold-p     'show-paren-match-face         t))
  (eval-no-err '(set-face-bold-p     'show-paren-mismatch-face      t))
  (eval-no-err '(set-face-inverse-video-p 'mode-line                t))

  ;; tweak colors for diff mode
  (eval-after-load 'diff-mode
    '(progn
       (set-face-foreground 'diff-added   "cyan")
       (set-face-foreground 'diff-removed "magenta")
       (set-face-bold-p 'diff-changed nil))))

;; if emacs is running under a window system
(if (null window-system) (progn)
  (eval-no-err '(set-default-font fi-my-fixed-font t)) ; older emacs versions only take 2 args
  (eval-no-err '(set-default-font fi-my-fixed-font))
  (eval-no-err '(set-frame-font   fi-my-fixed-font t)) ; replaced set-default-font in emacs 23.1
  (eval-no-err '(set-face-font 'variable-pitch fi-my-proportional-font))
  (eval-no-err '(set-face-font 'menu           fi-my-proportional-font))
  (eval-no-err '(set-face-font 'fixed-pitch    fi-my-fixed-font))
  (eval-no-err '(set-foreground-color                               fi-vtforeground))
  (eval-no-err '(set-background-color                               fi-vtbackground))
  (eval-no-err '(set-cursor-color                                   "green"))
  (eval-no-err '(set-face-foreground 'font-lock-builtin-face        fi-vtmagenta))
  (eval-no-err '(set-face-foreground 'font-lock-constant-face       fi-vtmagenta))
  (eval-no-err '(set-face-foreground 'font-lock-comment-face        fi-vtblue))
  (eval-no-err '(set-face-foreground 'font-lock-doc-face            fi-vtblue))
  (eval-no-err '(set-face-foreground 'font-lock-fi-punctuation-face fi-vtforeground-bold))
  (eval-no-err '(set-face-foreground 'font-lock-fi-constant-face    fi-vtred))
  (eval-no-err '(set-face-foreground 'font-lock-fi-number-face      fi-vtred))
  (eval-no-err '(set-face-foreground 'font-lock-function-name-face  fi-vtforeground-bold))
  (eval-no-err '(set-face-foreground 'font-lock-keyword-face        fi-vtyellow))
  (eval-no-err '(set-face-foreground 'font-lock-string-face         fi-vtred))
  (eval-no-err '(set-face-foreground 'font-lock-type-face           fi-vtgreen))
  (eval-no-err '(set-face-foreground 'font-lock-variable-name-face  fi-vtcyan))
  (eval-no-err '(set-face-foreground 'font-lock-warning-face        fi-vtcyan-bold))

  (eval-no-err '(set-face-foreground 'buffer-menu-buffer            fi-vtforeground-bold))
  (eval-no-err '(set-face-foreground 'show-paren-match-face         fi-vtwhite-bold))
  (eval-no-err '(set-face-background 'show-paren-match-face         "DarkSlateGray4"))
  (eval-no-err '(set-face-foreground 'show-paren-mismatch-face      fi-vtwhite-bold))
  (eval-no-err '(set-face-background 'show-paren-mismatch-face      fi-vtmagenta))
  (eval-no-err '(set-face-background 'mode-line                     "gray85"))
  (eval-no-err '(set-face-foreground 'region fi-vtwhite-bold))
  (eval-no-err '(set-face-background 'region fi-vtblue))

  (eval-no-err '(set-face-attribute 'mode-line-inactive nil
                                    :box     '(:style released-button)
                                    :inherit 'mode-line))

  ;; treat MS Windows a little bit differently
  (if (not (memq window-system '(w32 win32 winnt)))
      (eval-no-err '(set-face-bold-p 'mode-line-buffer-id nil))
    (eval-no-err '(set-face-bold-p 'mode-line-buffer-id t))
    (eval-no-err '(set-face-font 'mode-line fi-my-proportional-font)))

  ;; buttons, widgets, etc.
  (eval-no-err '(set-face-font   'custom-button fi-my-proportional-font))
  (eval-no-err '(set-face-font   'custom-button-mouse fi-my-proportional-font))
  (eval-no-err '(set-face-font   'custom-button-pressed fi-my-proportional-font))
  (eval-no-err '(set-face-font   'custom-button-unraised fi-my-proportional-font))
  (eval-no-err '(set-face-font   'custom-button-pressed-unraised fi-my-proportional-font))

  ;; tweak colors for diff mode
  (eval-after-load 'diff-mode
    '(progn
       (set-face-foreground 'diff-added   fi-vtcyan)
       (set-face-foreground 'diff-removed fi-vtmagenta)))

  (defun fi-my-faces-disable-bold ()
    "Disable all bold faces, with a few exceptions."
    (let ((dont-unbold-these-faces
           '(mode-line-emphasis     ; let this face remain bold
             region                 ; let these faces take on the
             secondary-selection))) ; boldness of underlying text
      (mapc (lambda (face)
              (if (not (member face dont-unbold-these-faces))
                  (eval-no-err '(set-face-bold-p face nil))))
            (face-list))))

  ;; We can't disable bold for all faces on startup because some major modes
  ;; create their own faces after our startup code has already ran.  Instead,
  ;; add the bold-disabling function as a hook that gets called whenever the
  ;; major mode changes.
  (add-hook 'after-change-major-mode-hook 'fi-my-faces-disable-bold))
