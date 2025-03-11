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
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; Me
(setq user-full-name "Pavel Pereverzev")
(setq user-mail-address "gotoss08@gmail.com")

;;; Encoding settings
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)  ;; If your terminal is UTF-8
(add-hook 'before-save-hook 'utf-8-unix)

;; Initialize package system
;;(require 'package)
;;(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
;;                         ("melpa" . "https://melpa.org/packages/")))
;;(package-initialize)

;; Refresh package contents if needed
;;(unless package-archive-contents
;;  (package-refresh-contents))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)

;;; Font settings
(set-frame-font "Iosevka 18" nil t)
;; (set-frame-font "Cascadia Code 16" nil t)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono-12") ; Example font

;;; Theme settings
;; (load-theme 'modus-vivendi-tinted)
;; (load-theme 'wombat)

(setq inhibit-startup-message t)
(setq frame-inhibit-implied-resize t) ;; prevent resize window on startup
(setq default-frame-alist '((width . 80) (height . 24)))

;;; Keyboard layout settings
(setq default-input-method "russian-computer")

;;; Private variables

(add-to-list 'load-path "~/.emacs.d/goty")
(require 'goty-privates)

;;; Org mode

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(use-package org
  :hook
  (org-mode . visual-line-mode)
  ;; (org-capture-mode . delete-other-windows)
  :init
  ;; (setq	org-time-stamp-custom-formats '("<%d/%m/%y %a>" . "<%d/%m/%y %a %H:%M>")
  ;; 	org-display-custom-times t)
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
  ;; :init
  ;; (setq completion-styles '(flex))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-tidy-shadowed-file-names t)
  (setq icomplete-in-buffer t)
  (icomplete-vertical-mode t))

(advice-add 'completion-at-point
              :after #'minibuffer-hide-completions)

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

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-wombat-dark))

(use-package move-dup
  :ensure t
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)))

;;; Custom keybidings

(global-set-key (kbd "C-c e c") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c e o") (lambda () (interactive) (find-file "~/OneDrive/org/")))
