;; init.el --- gotoss08 personal config       -*- lexical-binding: t; -*-
;; partially inspired by: https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4

(setq init-start-time (current-time))   ;benchmark

;;;
;;; Custom file
;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;;
;;; Elpaca
;;;

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;;
;;; Personal info
;;;

(setq user-full-name "Pavel Pereverzev")
(setq user-mail-address "gotoss08@gmail.com")

;;;
;;; Encoding
;;;

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)  ;; If your terminal is UTF-8
(add-hook 'before-save-hook 'utf-8-unix)

;;;
;;; Private variables
;;;

(add-to-list 'load-path "~/.emacs.d/goty")
(require 'goty-privates)

;;;
;;; Fonts
;;;

;; (set-frame-font "Iosevka 18" nil t)
;;(setq coding-font "Iosevka")
(setq coding-font "Fira Code")
(set-face-attribute 'default nil :family coding-font :height 160)
(set-face-attribute 'fixed-pitch nil :family coding-font)
(set-face-attribute 'variable-pitch nil :family "Aptos Serif")

(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(dolist (face '(default fixed-pitch))
  (set-face-attribute `,face nil :font "Fira Code"))

;;;
;;; Frame
;;;

(setq default-frame-alist
      '((height . 44) (width  . 81) (left-fringe . 0) (right-fringe . 0)
        (internal-border-width . 32) (vertical-scroll-bars . nil)
        (bottom-divider-width . 0) (right-divider-width . 0)
        (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)

;;;
;;; Disable && enable modes
;;;

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
;; (global-hl-line-mode t)
(pixel-scroll-precision-mode t)
(electric-pair-mode t)

;;;
;;; Initial misc
;;;

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq visible-bell t)
(setq frame-inhibit-implied-resize t) ;; prevent resize window on startup
(setq default-frame-alist '((width . 100) (height . 30)))
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
(save-place-mode t) ;; When you visit a file, point goes to the last place where it was when you previously visited the same file
(savehist-mode t)
(setq require-final-newline t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)
(setq default-input-method "russian-computer")

;;; Spaces
(setq backward-delete-char-untabify-method 'hungry) ;; AWESOME
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
;; (set-face-background 'trailing-whitespace "yellow")
(setq-default electric-indent-inhibit t) ;; AWESOME
;; (global-whitespace-mode) ; Enable whitespace mode everywhere
(add-hook 'after-save-hook 'delete-trailing-whitespace)

;;; Backups, autosaving, lock-files...
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files t)
(setq backup-directory-alist '(("." . (expand-file-name "custom.el" user-emacs-directory))))
(setq backup-by-copying t) ;; Backups dont break file creation time

;;;
;;; Theme
;;;

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'nano-theme)
;;(mapc #'disable-theme custom-enabled-themes)
;;(nano-dark)

;;; Theme settings

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

  (setq modus-themes-common-palette-overrides
        '(
          ;; (cursor cyan-intense)
          ;; (comment magenta-faint)
          ;; (bg-paren-match bg-magenta-subtle)
          ;; (fg-paren-match magenta)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          ))

  (mapc #'disable-theme custom-enabled-themes)
  (modus-themes-load-theme 'modus-vivendi-tinted)
  )

(defun my-modus-themes-custom-faces (&rest _)
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
                            )))
     )))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

;; (load-theme 'wombat)

;;; Org mode

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(defun my-org-mode-hook ()
  (visual-line-mode t)
  (variable-pitch-mode t))

(use-package org
  :ensure nil
  :hook
  (org-mode . my-org-mode-hook)
  (org-capture-mode . delete-other-windows)
  ;; :init
  ;; (setq org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %H:%M>")
  ;;       org-display-custom-times t)
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
	    org-log-done t
	    ;; org-default-notes-file "~/org/todo.org"
	    org-directory (concat (expand-file-name "~/") "OneDrive/org")
	    org-agenda-files (list "inbox.org" "agenda.org"))
  (setq org-capture-templates
       `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c i" . org-capture-inbox)))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; Packages config

(use-package icomplete
  :init
  (setq completion-styles '(flex))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-tidy-shadowed-file-names t)
  (setq icomplete-separator " · ")
  (setq icomplete-in-buffer t)
  ;; (icomplete-vertical-mode t)
  (fido-vertical-mode t)
  ;; (fido-mode t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions))

;;; Minibuffer completion

(setq tab-always-indent 'complete
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " . "
      icomplete-with-completion-tables t
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      resize-mini-windows 'grow-only
      icomplete-matches-format nil)
(bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
(bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)

(use-package completion-preview
  :hook (prog-mode . completion-preview-mode)
  :bind
  (:map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate)))

(use-package which-key
  :config
  (which-key-mode t))

(use-package gptel
  :ensure t
  :init
  (setq gptel-model 'gemini-2.0-flash)
  (if (boundp 'goty/gemini-api-key)
      (setq gptel-backend (gptel-make-gemini "Gemini"
			    :key goty/gemini-api-key
			    :stream t))))

(use-package restclient
  :ensure t)

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme 'solarized-wombat-dark))

(use-package move-dup
  :ensure t
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-prev-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

(use-package consult
  :ensure t
  :demand t
  :bind (("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("C-M-y" . consult-yank-pop)
         ("M-y" . consult-yank-from-kill-ring)
         ("C-c f l" . consult-focus-lines)))

;; mc/skip-to-next-like-this
;; mc/skip-to-previous-like-this

;;; Custom keybidings

(global-set-key (kbd "C-c e c") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c e o") (lambda () (interactive) (find-file "~/OneDrive/org/")))

(bind-key "M-/" #'hippie-expand)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-r" #'isearch-backward-regexp)
(bind-key "C-M-s" #'isearch-forward)
(bind-key "C-M-r" #'isearch-backward)

;; source: https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-x C-r" #'recentf-open)
(bind-key "M-n" #'make-frame)
(bind-key "C-z"  nil) ;; No suspend frame
(bind-key "C-<wheel-up>" nil) ;; No text resize via mouse scroll
(bind-key "C-<wheel-down>" nil) ;; No text resize via mouse scroll

;;; Header & mode lines

(setq-default mode-line-format "")
(setq-default header-line-format
  '(:eval
    (let ((prefix (cond (buffer-read-only     '("RO" . isearch))
                        ((buffer-modified-p)  '("**" . error))
                        (t                    '("RW" . help-key-binding))))
          (mode (concat "(" (downcase (cond ((consp mode-name) (car mode-name))
                                            ((stringp mode-name) mode-name)
                                            (t "unknow")))
                        " mode)"))
          (coords (format-mode-line "%l:%c ")))
      (list
       (propertize " " 'face (cdr prefix)  'display '(raise -0.25))
       (propertize (car prefix) 'face (cdr prefix))
       (propertize " " 'face (cdr prefix) 'display '(raise +0.25))
       (propertize (format-mode-line " %b ") 'face 'icomplete-first-match)
       (propertize mode)
       (propertize " " 'display `(space :align-to (- right ,(length coords))))
       (propertize coords 'face 'shadow)))))

;;; Minibuffer setup

(defun minibuffer-setup ()
  (set-window-margins nil 3 0)
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (+ (point-min) 1)
      `(display ((margin left-margin)
                 ,(format "# %s" (substring (minibuffer-prompt) 0 1))))))
  (setq truncate-lines t))
(add-hook 'minibuffer-setup-hook #'minibuffer-setup)

;;; End

(defun display-startup-echo-area-message ()
  (let ((init-time (float-time (time-subtract (current-time) init-start-time)))
        (total-time (string-to-number (emacs-init-time "%f"))))
    (message (concat
              (propertize "Startup time: " 'face 'bold)
              (format "%.2fs " init-time)
              (propertize (format "(+ %.2fs system time)"
                                  (- total-time init-time)) 'face 'shadow)))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
