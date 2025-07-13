;;; init.el --- My Emacs config -*- lexical-binding: t; -*-

;;; Custom file

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Package manager (TODO)

(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;;; Variables

(if (eq system-type 'windows-nt)
    (setq my-onedrive-path "~/OneDrive/")
  (setq my-onedrive-path "/mnt/c/Users/gotos/OneDrive/"))

(setq my-org-path (expand-file-name "org" my-onedrive-path))

(setq user-full-name "Pavel Pereverzev")
(setq user-mail-address "gotoss08@gmail.com")

;;; Private

(defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

(defun openai-api-key ()
  (read-file-contents (expand-file-name "api-secrets/openai.txt" my-onedrive-path)))

(defun gemini-api-key ()
  (read-file-contents (expand-file-name "api-secrets/gemini.txt" my-onedrive-path)))

;;; Encoding && Language

(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")

;;; Config

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
;; (set-frame-font "Aporetic Sans Mono 10" nil t)
(set-frame-font "Aporetic Serif Mono 10" nil t)
;; (load-theme 'modus-vivendi)
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(height . 60))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 3)
(pixel-scroll-precision-mode t)
(global-visual-line-mode t)

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-dark))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package saveplace
  :ensure t
  :init
  (setq save-place-file (expand-file-name "places" user-emacs-directory))
  (save-place-mode 1))

;;; Org

(load (expand-file-name "org-config.el" user-emacs-directory))

;;; Third-party Plugins

(use-package multiple-cursors :ensure t)

(use-package gptel
  :ensure t
  :init
  :config
  (setq gptel-include-reasoning nil)
  (setq gptel-api-key (openai-api-key))
  (gptel-make-gh-copilot "Copilot")
  (gptel-make-gemini "Gemini" :key (gemini-api-key) :stream t)
  (add-hook 'gptel-post-response-hook 'gptel-auto-scroll))

(use-package elysium
  :ensure t
  :after (gptel)
  :demand t
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical))

;; (use-package gptel-prompts
;;   :ensure t
;;   :after (gptel)
;;   :demand t
;;   :config
;;   (gptel-prompts-update)
;;   ;; Ensure prompts are updated if prompt files change
;;   (gptel-prompts-add-update-watchers))

(use-package smerge-mode
  :ensure t
  :hook
  (prog-mode . smerge-mode))

;; TODO: use fido-mode more properly
(fido-vertical-mode t)

;;; Key bindings

;; Using general.el for all keybindings

(use-package general
  :ensure t
  :config
  (general-define-key
    ;; General purpose keys
    "<f2>" 'toggle-input-method
    "C-<return>" 'gptel-send
    "C-x C-b" 'ibuffer

    ;; Multiple Cursors
    "C-S-c C-S-c" 'mc/edit-lines
    "C->" 'mc/mark-next-like-this
    "C-<" 'mc/mark-previous-like-this
    "C-c C-<" 'mc/mark-all-like-this

    ;; Org mode bindings (overriding default C-c a/c/l)
    "C-c a" 'org-agenda
    "C-c c" 'org-capture
    "C-c l" 'org-store-link

    ;; Custom file bindings
    "C-c r" (lambda () (interactive) (find-file my-org-someday-path))
    "C-c m" (lambda () (interactive) (find-file my-org-meetings))

    "C-c g g" 'gptel
    "C-c g r" 'gptel-rewrite
    "C-<enter>" 'gptel-send

    "C-c e c" (lambda () (interactive) (find-file user-init-file))
    "C-c e o" (lambda () (interactive) (find-file my-org-path))
    "C-c e p" (lambda () (interactive) (find-file my-org-projects))
    )
  )
