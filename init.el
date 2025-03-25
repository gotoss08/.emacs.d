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
;;; Load path
;;;

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/goty")

;;;
;;; Personal info
;;;

(setq user-full-name "Pavel Pereverzev")
(setq user-mail-address "gotoss08@gmail.com")

(if (eq system-type 'windows-nt)
    (setq one-drive-path "~/OneDrive/org/")
  (setq one-drive-path "/mnt/c/Users/gotos/OneDrive/org/"))

;;;
;;; Encoding
;;;

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)  ;; If your terminal is UTF-8
(add-hook 'before-save-hook 'utf-8-unix)

;;;
;;; Tramp
;;;

(setq tramp-verbose 1)

;;;
;;; Private variables
;;;

(require 'goty-privates)

;;;
;;; Fonts
;;;

;; Don’t compact font caches during GC.
;; (setq inhibit-compacting-font-caches t)

(defun goty/change-font (font-name)
  (set-face-attribute 'default nil :family font-name :weight 'regular :height 150)
  (set-face-attribute 'fixed-pitch nil :family font-name)
  ;; (set-face-attribute 'bold nil :weight 'regular)
  ;; (set-face-attribute 'bold-italic nil :weight 'regular)
  )

;; main fonts
;; (goty/change-font "Iosevka NF")
;; (goty/change-font "IosevkaTerm NF")
;; (goty/change-font "IosevkaTermSlab NF")
;; (goty/change-font "ZedMono NF")
;; (goty/change-font "VictorMono NF")
;; (goty/change-font "Inconsolata LGC Nerd Font")
;; (goty/change-font "JetBrainsMono NF")
;; (goty/change-font "Terminess Nerd Font Mono")

;; too meh
;; (goty/change-font "CaskaydiaCove NF")
;; (goty/change-font "FiraCode Nerd Font")
;; (goty/change-font "Hack Nerd Font")

;; no cyrillic
;; (goty/change-font "ComicShannsMono Nerd Font")
;; (goty/change-font "Hurmit Nerd Font")

;; (set-frame-font "terminus-32" nil t)
;; (set-frame-font "Terminess Nerd Font-16" nil t)

(set-frame-font "ZedMono NF-15" nil t)
;; (set-face-attribute 'variable-pitch nil :family "Aptos" :height 130)
;; (set-face-attribute 'variable-pitch nil :family "Aptos Serif" :height 140)
(set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 130)

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;;;
;;; Frame
;;;

(setq frame-inhibit-implied-resize t) ;; prevent resize window on startup
(setq default-frame-alist
      '((height . 32) (width  . 100) (left-fringe . 0) (right-fringe . 0)
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
(pixel-scroll-precision-mode t)
(electric-pair-mode t)
;; (global-hl-line-mode t)

;;;
;;; Initial misc
;;;

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq visible-bell t)
(setq default-input-method "russian-computer")
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
(save-place-mode t) ;; When you visit a file, point goes to the last place where it was when you previously visited the same file
(savehist-mode t)
(setq require-final-newline t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

;;;
;;; Spaces
;;;

(setq backward-delete-char-untabify-method 'hungry) ;; AWESOME
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default electric-indent-inhibit t) ;; AWESOME
(add-hook 'after-save-hook 'delete-trailing-whitespace)

;;;
;;; Backups, autosaving, lock-files...
;;;

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files t)
(setq backup-directory-alist '(("." . (expand-file-name "custom.el" user-emacs-directory))))
(setq backup-by-copying t) ;; Backups dont break file creation time

;;;
;;; Theme
;;;

(mapc #'disable-theme custom-enabled-themes)

(defun disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-wombat-dark))

;; (load-theme 'wombat)
;; (require 'goty-modus-themes)
;; (require 'nano-theme)
;; (nano-dark)

(use-package mood-line
  :ensure t
  ;; :config
  ;; (mood-line-mode)
  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure t)
;;  :init (doom-modeline-mode 1))

(setq-default mode-line-format nil)
(require 'nano-modeline)
;; (setq nano-modeline-position)
(nano-modeline-text-mode t)
(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;; (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
(add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
(add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
(add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
(add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
(add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
(add-hook 'term-mode-hook            #'nano-modeline-term-mode)
(add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
(add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
(add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
(add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil
        doom-themes-padded-modeline t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(setq-default header-line-format mode-line-format)

(use-package spacemacs-theme
  :ensure t)

;; (require 'goty-header-modeline)
;; (goty/header-modeline)

;; (require 'goty-minibuffer)
;; (goty/minibuffer)

;;; Org mode

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(use-package org
  ;; :defer t
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (variable-pitch-mode t)
                ;; (visual-line-mode t)
                (org-indent-mode t)))
  (org-capture-mode . delete-other-windows)
  ;; :init
  ;; (setq org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %H:%M>")
  ;;       org-display-custom-times t)
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
	    org-log-done t
	    ;; org-default-notes-file "~/org/todo.org"
	    org-directory one-drive-path
	    org-agenda-files (list "inbox.org" "agenda.org"))
  (setq org-capture-templates
       `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))))
  (setq org-cycle-separator-lines 2)
  (setq org-adapt-indentation t)
  (setq org-indent-indentation-per-level 4)
  (setq org-hide-leading-stars t)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c i" . org-capture-inbox)))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

;;; Packages config

(use-package icomplete
  :init
  (setq completion-styles '(flex))
  :config
  (setq tab-always-indent 'complete
      icomplete-tidy-shadowed-file-names t
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " · "
      icomplete-with-completion-tables t
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      resize-mini-windows 'grow-only
      icomplete-matches-format nil)
  ;; (icomplete-vertical-mode t)
  (fido-vertical-mode t)
  ;; (fido-mode t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions))

;; (bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
;; (bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)

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

;; TODO: add new binds for multiple cursors
;; mc/skip-to-next-like-this
;; mc/skip-to-previous-like-this

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

;;;
;;; Custom keybidings
;;;

(global-set-key (kbd "C-c e c") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c e o") (lambda () (interactive) (find-file one-drive-path)))

(bind-key "M-/" #'hippie-expand)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-r" #'isearch-backward-regexp)
(bind-key "C-M-s" #'isearch-forward)
(bind-key "C-M-r" #'isearch-backward)
(bind-key "<f2>" #'toggle-input-method)

;; source: https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-x C-r" #'recentf-open)
(bind-key "M-n" #'make-frame)
(bind-key "C-z"  nil) ;; No suspend frame
(bind-key "C-<wheel-up>" nil) ;; No text resize via mouse scroll
(bind-key "C-<wheel-down>" nil) ;; No text resize via mouse scroll

;;;
;;; End
;;;

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
