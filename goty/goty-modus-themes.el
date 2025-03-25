;; goty-modus-themes.el   -*- lexical-binding: t; -*-
;; inspired by: https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4

(use-package modus-themes
  :ensure t
  :demand t
  :bind
  (("<f5>" . modus-themes-toggle))
  :config
  (setq
   modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi)
   modus-themes-mixed-fonts t
   ;; modus-themes-variable-pitch-ui t
   ;; modus-themes-italic-constructs t
   ;; modus-themes-bold-constructs t
   ;; modus-themes-completions '((t . (bold)))
   ;; modus-themes-prompts '(bold)
   ;; modus-themes-headings
   ;; '((agenda-structure . (variable-pitch light 2.2))
   ;;   (agenda-date . (variable-pitch regular 1.3))
   ;;   (t . (regular 1.15)))
   )


  (mapc #'disable-theme custom-enabled-themes)
  (modus-themes-load-theme 'modus-vivendi-tinted)
  )

(defun my-modus-themes-custom-faces (&rest _)
  (progn
    (setq modus-themes-common-palette-overrides
          '((border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)))
    (modus-themes-with-colors
      (custom-set-faces
       `(header-line ((,c
                       :background ,bg-dim
                       :underline nil
                       :box (:line-width 1 :color ,bg-main)
                       :inherit lazy-highlight
                       )))
       `(header-line-inactive ((,c
                                :background ,bg-main
                                :underline nil
                                :box (:line-width 1 :color ,bg-main)
                                :inherit lazy-highlight
                                )))
       `(mode-line ((,c
                     :background ,bg-main
                     :underline ,bg-active
                     :height 40
                     :overline nil
                     :box nil
                     )))
       `(mode-line-inactive ((,c
                              :background ,bg-main
                              :underline ,bg-active
                              :height 40
                              :overline nil
                              :box nil
                              )))))))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

(provide 'goty-modus-themes)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
