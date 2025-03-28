;; nano-theme.el --- Minimal NANO (not a real) theme     -*- lexical-binding: t -*-
;; source: https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4

(defface nano-default '((t)) "")   (defface nano-default-i '((t)) "")
(defface nano-highlight '((t)) "") (defface nano-highlight-i '((t)) "")
(defface nano-subtle '((t)) "")    (defface nano-subtle-i '((t)) "")
(defface nano-faded '((t)) "")     (defface nano-faded-i '((t)) "")
(defface nano-salient '((t)) "")   (defface nano-salient-i '((t)) "")
(defface nano-popout '((t)) "")    (defface nano-popout-i '((t)) "")
(defface nano-strong '((t)) "")    (defface nano-strong-i '((t)) "")
(defface nano-critical '((t)) "")  (defface nano-critical-i '((t)) "")

(defun nano-set-face (name &optional foreground background weight)
  "Set NAME and NAME-i faces with given FOREGROUND, BACKGROUND and WEIGHT"

  (apply #'set-face-attribute `(,name nil
                                      ,@(when foreground `(:foreground ,foreground))
                                      ,@(when background `(:background ,background))
                                      ,@(when weight `(:weight ,weight))))
  (apply #'set-face-attribute `(,(intern (concat (symbol-name name) "-i")) nil
                                :foreground ,(face-background 'nano-default)
                                ,@(when foreground `(:background ,foreground))
                                :weight regular)))

(defun nano-link-face (sources faces &optional attributes)
  "Make FACES to inherit from SOURCES faces and unspecify ATTRIBUTES."

  (let ((attributes (or attributes
                        '( :foreground :background :family :weight
                           :height :slant :overline :underline :box))))
    (dolist (face (seq-filter #'facep faces))
      (dolist (attribute attributes)
        (set-face-attribute face nil attribute 'unspecified))
      (set-face-attribute face nil :inherit sources))))

(defun nano-install-theme ()
  "Install THEME"

  (set-face-attribute 'default nil
                      :foreground (face-foreground 'nano-default)
                      :background (face-background 'nano-default))
  (dolist (item '((nano-default .  (variable-pitch variable-pitch-text
                                                   fixed-pitch fixed-pitch-serif))
                  (nano-highlight . (hl-line highlight))
                  (nano-subtle .    (match region
                                           lazy-highlight widget-field))
                  (nano-faded .     (shadow
                                     font-lock-comment-face
                                     font-lock-doc-face
                                     icomplete-section
                                     completions-annotations))
                  (nano-popout .    (warning
                                     font-lock-string-face))
                  (nano-salient .   (success link
                                             help-argument-name
                                             custom-visibility
                                             font-lock-type-face
                                             font-lock-keyword-face
                                             font-lock-builtin-face
                                             completions-common-part))
                  (nano-strong .    (font-lock-function-name-face
                                     font-lock-variable-name-face
                                     icomplete-first-match
                                     minibuffer-prompt))
                  (nano-critical .  (error
                                     completions-first-difference))
                  (nano-faded-i .   (help-key-binding))
                  (nano-default-i . (custom-button-mouse
                                     isearch))
                  (nano-critical-i . (isearch-fail))
                  ((nano-subtle nano-strong) . (custom-button
                                                icomplete-selected-match))
                  ((nano-faded-i nano-strong) . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))

  ;; Mode & header lines
  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :underline nil
                      :box `( :line-width 1
                              :color ,(face-background 'nano-default))
                      :inherit 'nano-subtle)
  (set-face-attribute 'mode-line nil
                      :background (face-background 'default)
                      :underline (face-foreground 'nano-faded)
                      :height 40 :overline nil :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'default)
                      :underline (face-foreground 'nano-faded)
                      :height 40 :overline nil :box nil))

(defun nano-light (&rest args)
  "NANO light theme (based on material colors)"

  (interactive)
  (nano-set-face 'nano-default "#37474F" "#FFFFFF") ;; Blue Grey / L800
  (nano-set-face 'nano-strong "#000000" nil 'regular) ;; Black
  (nano-set-face 'nano-highlight nil "#FAFAFA") ;; Very Light Grey
  (nano-set-face 'nano-subtle nil "#ECEFF1") ;; Blue Grey / L50
  (nano-set-face 'nano-faded "#90A4AE") ;; Blue Grey / L300
  (nano-set-face 'nano-salient "#673AB7") ;; Deep Purple / L500
  (nano-set-face 'nano-popout "#FFAB91") ;; Deep Orange / L200
  (nano-set-face 'nano-critical "#FF6F00") ;; Amber / L900
  (nano-install-theme))

(defun nano-dark (&rest args)
  "NANO dark theme (based on nord colors)"

  (interactive)
  (nano-set-face 'nano-default "#ECEFF4" "#2E3440") ;; Snow Storm 3
  (nano-set-face 'nano-strong "#ECEFF4" nil 'regular) ;; Polar Night 0
  (nano-set-face 'nano-highlight nil "#3B4252")  ;; Polar Night 1
  (nano-set-face 'nano-subtle nil "#434C5E") ;; Polar Night 2
  (nano-set-face 'nano-faded "#677691") ;;
  (nano-set-face 'nano-salient "#81A1C1")  ;; Frost 2
  (nano-set-face 'nano-popout "#D08770") ;; Aurora 1
  (nano-set-face 'nano-critical "#EBCB8B") ;; Aurora 2
  (nano-install-theme))

(provide 'nano-theme)
