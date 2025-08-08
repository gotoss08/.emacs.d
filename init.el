;;; init.el --- My Emacs config -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Custom file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Package Manager
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(package-initialize)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Helper functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'windows-nt)
  (setq my-onedrive-path "~/OneDrive/"))

(when (eq system-type 'darwin)
  (setq my-onedrive-path "~/Library/CloudStorage/OneDrive-Личная/"))

(setq my-org-path (expand-file-name "org" my-onedrive-path))

(setq user-full-name "Pavel Pereverzev")
(setq user-mail-address "gotoss08@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Secrets
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun openai-api-key ()
  (read-file-contents (expand-file-name "api-secrets/openai.txt" my-onedrive-path)))

(defun gemini-api-key ()
  (read-file-contents (expand-file-name "api-secrets/gemini.txt" my-onedrive-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Initial buffer & Initial major mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-buffer-choice 'remember-notes)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq default-major-mode 'org-mode)
(setq remember-data-file (expand-file-name "org/remember-notes.org" my-onedrive-path))
(setq remember-notes-initial-major-mode 'org-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Encoding & Language
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(setq default-input-method "russian-computer")
(global-set-key (kbd "<f2>") 'toggle-input-method)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Windows
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'windows-nt)
  (setq w32-apps-modifier 'hyper
      w32-lwindow-modifier 'super
      w32-rwindow-modifier 'hyper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Mac
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil
        mac-use-title-bar nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Visuals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
(blink-cursor-mode -1)

(global-visual-line-mode t)

(display-time-mode t)
(line-number-mode t)
(column-number-mode t)

(setopt show-trailing-whitespace t)
(setopt indicate-empty-lines t)

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 3)
(pixel-scroll-precision-mode t)

(windmove-default-keybindings 'control)

;; Disable dailogs and stuff

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq pop-up-windows nil)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package which-key
  :ensure t
  :config
  (which-key-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Font
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (eq system-type 'darwin)
    (set-frame-font "Monaco 16" nil t)
  (set-frame-font "Aporetic Serif Mono 11" nil t))

;; (set-frame-font "Aporetic Sans Mono 11" nil t)
;; (set-frame-font "Aporetic Serif Mono 11" nil t)
;; (set-frame-font "Sudo 14" nil t)
;; (set-frame-font "Terminess Nerd Font 13" nil t)
;; (set-frame-font "Terminus 12" nil t)
;; (set-frame-font "VictorMono NF 12" nil t)
;; (set-frame-font "ZedMono NF 12" nil t)
;; (set-frame-font "JetBrains Mono NL 12" nil t)
;; (set-frame-font "JuliaMono 12" nil t)
;; (set-frame-font "Aptos Mono 12" nil t)
;; (set-frame-font "Cascadia Code 12" nil t)
;; (set-frame-font "Consolas 12" nil t)
;; (set-frame-font "DejaVu Sans Mono 12" nil t)
;; (set-frame-font "Fira Code 12" nil t)
;; (set-frame-font "FiraCode Nerd Font 12" nil t)
;; (set-frame-font "Hack Nerd Font 12" nil t)
;; (set-frame-font "Hurmit Nerd Font 10" nil t)
;; (set-frame-font "IBM Plex Mono 11" nil t)
;; (set-frame-font "Inconsolata LGC Nerd Font 11" nil t)
;; (set-frame-font "Iosevka 10" nil t)
;; (set-frame-font "Iosevka Term 10" nil t)
;; (set-frame-font "Ubuntu Mono 12" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'modus-vivendi)

(use-package ef-themes
  :ensure t
  :config
  ;; (load-theme 'ef-dark)
  ;; (load-theme 'ef-bio)
  (load-theme 'ef-symbiosis))

;; (use-package modus-themes
;;   :config
;;   (load-theme 'modus-vivendi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Session, backups, places, recentf, hist, bookmarks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable junk
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

(setq save-place-file (expand-file-name "places" user-emacs-directory))
(save-place-mode t)

(setq savehist-additional-variables
      '(kill-ring
        command-history
	set-variable-value-history
	custom-variable-history
	query-replace-history
	read-expression-history
	minibuffer-history
	read-char-history
	face-name-history
	bookmark-history
        ivy-history
	counsel-M-x-history
	file-name-history
        counsel-minibuffer-history))
(setq history-length 250)
(setq kill-ring-max 25)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(put 'ivy-history                'history-length 25)
(put 'counsel-M-x-history        'history-length 25)
(put 'counsel-minibuffer-history 'history-length 25)
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode t)

;; (desktop-save-mode t)

(setq recentf-max-menu-items 25)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(recentf-mode t)

(setq bookmark-default-file (expand-file-name "bookmark" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt indent-tabs-mode nil)
(setopt tab-width 4)
(setopt comment-empty-lines t)

(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

(use-package emacs
  :config
  ;; Treesitter config
  ;; Reminder: run `M-x treesit-install-language-grammar'
  ;; (setq major-mode-remap-alist
  ;;       '((yaml-mode . yaml-ts-mode)
  ;;         (bash-mode . bash-ts-mode)
  ;;         (js2-mode . js-ts-mode)
  ;;         (typescript-mode . typescript-ts-mode)
  ;;         (json-mode . json-ts-mode)
  ;;         (css-mode . css-ts-mode)
  ;;         (python-mode . python-ts-mode)))
  :hook
  ((prog-mode . electric-pair-mode)))

(electric-indent-mode t)

(use-package eshell
  :init
  (defun my-setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . my-setup-eshell)))

(use-package project
  :custom
  (when (>= emacs-major-version 30)
    (project-mode-line t)))

;; (use-package eglot
;;   ;; no :ensure t here because it's built-in
;;
;;   ;; Configure hooks to automatically turn-on eglot for selected modes
;;   :hook
;;   (((python-mode ruby-mode elixir-mode) . eglot-ensure))
;;
;;   :custom
;;   (eglot-send-changes-idle-time 0.1)
;;   (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
;;
;;   :config
;;   (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
;;   ;; Sometimes you need to tell Eglot where to find the language server
;;   ; (add-to-list 'eglot-server-programs
;;   ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Minibuffer Completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;; (setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

(fido-vertical-mode t)
(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  ORG
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "org-config.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Third Party
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Motion aids
(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package move-text
  :ensure t
  :bind
  (("M-p" . 'move-text-up)
   ("M-n" . 'move-text-down))
  :config
  (move-text-default-bindings))

(use-package expand-region
  :ensure t
  :bind (("C-=" . 'er/expand-region)))

(use-package smart-hungry-delete
  :ensure t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	       ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	       ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package gptel
  :ensure t
  :bind
  (("C-RET" . 'gptel-send)
   ("C-c g g" . 'gptel)
   ("C-c g r" . 'gptel-rewrite))
  :config
  (setq gptel-include-reasoning nil)
  (setq gptel-api-key (openai-api-key))
  (gptel-make-gh-copilot "Copilot")
  (gptel-make-gemini "Gemini" :key (gemini-api-key) :stream t)
  (add-hook 'gptel-post-response-hook 'gptel-auto-scroll))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Fancy completion-at-point functions
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Templates
(use-package tempel
  :ensure t
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or
  ;; writing prose.
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package crux
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Key Bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f1>") 'eshell)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
(global-set-key (kbd "C-S-<down>") 'shrink-window)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key (kbd "C-c e c") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c e o") (lambda () (interactive) (find-file my-org-path)))
(global-set-key (kbd "C-c e p") (lambda () (interactive) (find-file my-org-projects)))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defun sum-hours-in-region (start end)
  "Sum all numbers followed by час/часа/часов in selected region.
Copy result to kill ring, move point to region start, and deactivate mark."
  (interactive "r")
  (let ((total 0))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\([0-9]+\\(?:\\.[0-9]+\\)?\\) *час[ао]?[в]?" end t)
        (setq total (+ total (string-to-number (match-string 1))))))
    (let ((total-str (number-to-string total)))
      (kill-new total-str)
      (goto-char start)              ; move point AFTER save-excursion
      (deactivate-mark)
      (message "Total hours: %s (copied to kill ring)" total-str)
      total)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  End
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "emacs init time %s" (emacs-init-time))

(setq gc-cons-threshold 800000)

;;; init.el ends here
