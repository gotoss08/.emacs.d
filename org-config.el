;;; org-config.el --- Org Mode config -*- lexical-binding: t; -*-

;; ORG Directory

;; old: previously was using direct one drive path, but then migrated to ~/org symlink to onedrive
;; (if (eq system-type 'windows-nt)
;;     (setq my-onedrive-path "~/OneDrive/")
;;   (setq my-onedrive-path "/mnt/c/Users/gotos/OneDrive/"))
;; (setq my-org-path (expand-file-name "org" my-onedrive-path))

(use-package org
  :ensure t
  :pin org
  :mode (("\\.org$" . org-mode)))

;; Path to Org
(setq org-directory (expand-file-name "~/org"))
(setq my-org-path org-directory)

;; Common Org Files
(setq my-org-inbox-path    (expand-file-name "inbox.org"       my-org-path))
(setq my-org-work-path     (expand-file-name "work.org"        my-org-path))
(setq my-org-personal-path (expand-file-name "personal.org"    my-org-path))
(setq my-org-someday-path  (expand-file-name "someday.org"     my-org-path))
(setq my-org-regulations   (expand-file-name "regulations.org" my-org-path))
(setq my-org-meetings      (expand-file-name "meetings.org"    my-org-path))
(setq my-org-projects      (expand-file-name "projects/"       my-org-path))
(setq my-org-habits-path   (expand-file-name "habits.org"      my-org-path))
(setq my-org-daily-path    (expand-file-name "daily"           my-org-path))

;; Agenda files
(setq org-agenda-files
      (list my-org-inbox-path
            my-org-work-path
            my-org-personal-path
            my-org-someday-path
            my-org-regulations
            my-org-meetings
	    my-org-projects
	    my-org-habits-path))

;; UI/Behavior
(setq org-startup-indented t)
(setq org-ellipsis " ▼")
(setq org-pretty-entities t)
(setq org-enforce-todo-dependencies t)
(setq org-hierarchical-todo-statistics nil)
(setq system-time-locale "C")
(setq org-deadline-warning-days 3)
(setq org-log-start 'time)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-startup-folded 'content)
(setq org-agenda-skip-scheduled-if-done t)

;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;; Faces (optional)
(setq org-todo-keyword-faces
      '(("TODO"     . "red")
        ("NEXT"     . "blue")
        ("WAIT"     . "orange")
        ("DONE"     . "green")
        ("CANCELED" . "gray")))

(setq org-fontify-done-headline t)
(setq org-fontify-whole-heading-line t)

;; Capture Templates
(setq org-default-notes-file my-org-inbox-path)
(setq org-capture-templates
      `(("i" "Inbox" entry (file ,my-org-inbox-path)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

        ("w" "Рабочая задача" entry (file+headline ,my-org-work-path "Проекты / задачи")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

        ("p" "Личное" entry (file ,my-org-personal-path)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

        ("d" "Когда-нибудь / идея" entry (file ,my-org-someday-path)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

        ("r" "Регламент" entry (file+headline ,my-org-regulations "Идеи / обсуждение")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

        ("m" "Совещание" entry (file+olp+datetree ,my-org-meetings)
         "* %^{Название}\n:PROPERTIES:\n:CREATED: %U\n:ATTENDEES: %^{Участники}\n:END:\n\n** Вопросы\n- %?\n\n** Решения\n- \n\n** Задачи\n- ")))

;; Agenda commands
;; (setq org-agenda-custom-commands
;;       '(("o" "🧠 Обзор дня" agenda ""
;;          ((org-agenda-span 'day)))

;;         ("w" "📅 Задачи недели + Review"
;;          ((agenda "")
;;           (tags-todo "+project")
;;           (tags "+habit")))

;;         ("p" "👤 Только мои задачи (без делегирования)"
;;          ((tags-todo "-person")))

;;         ("d" "🧾 Делегировано сотрудникам"
;;          ((tags-todo "+person")))

;;         ("h" "🏠 Личное + Привычки"
;;          ((tags-todo "+personal")
;;           (tags "+habit")))

;;         ("r" "📃 Только Review / табель / квартира"
;;          ((search "Weekly Review")
;;           (search "табель")
;;           (search "Оплатить квартиру")))))

;; Archive
(setq org-archive-location (expand-file-name "archive.org::datetree/" org-directory))

;; Refile
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 3)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Habits
(require 'org-habit)
(setq org-habit-graph-column 60)

;; Babel
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (dot . t)))

;; Tempo
(require 'org-tempo)

;; Usage:
;; <s[TAB] :: expands to
;;
;;#+BEGIN_SRC ?
;;
;;#+END_SRC

;; Abbrev
(add-hook 'org-mode-hook #'abbrev-mode)

;; Abbrev template
(define-skeleton org-skeleton-header
  "Insert a default Org mode file header."
  nil
  "#+TITLE: " (file-name-base (or buffer-file-name (buffer-name))) "\n"
  "#+AUTHOR: " user-full-name "\n"
  "#+EMAIL: " user-mail-address "\n"
  "#+DATE: " (format-time-string "%Y-%m-%d") "\n"
  "#+LANGUAGE: ru\n"
  "#+STARTUP: show2levels\n"
  "#+OPTIONS: toc:nil\n\n")
(define-abbrev org-mode-abbrev-table "orghead" "" 'org-skeleton-header)

;; Keys
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c r") (lambda () (interactive) (find-file my-org-someday-path)))
(global-set-key (kbd "C-c m") (lambda () (interactive) (find-file my-org-meetings)))
(global-set-key (kbd "C-c e o") (lambda () (interactive) (find-file my-org-path)))

(setq org-agenda-custom-commands
      '(
	("n" "All (Agenda + TODOs)"
	 ((agenda "")
	 (todo "")))
	("w" "Work only (Agenda + TODOs)"
	 ((agenda)
	  (todo))
	 ((org-agenda-tag-filter-preset '("-Семья" "-Личное"))))
	))
	 ;; (org-agenda-category-filter "-Семья"
	 ;; (agenda)
	 ;; (todo)))))

(defun my/org-open-agenda-and-todo ()
  "Open org agenda on the left and TODO list on the right."
  (interactive)
  (delete-other-windows)
  (let ((agenda-buffer (org-get-agenda-file-buffer (car org-agenda-files))))
    (org-agenda-list)
    (split-window-right)
    (other-window 1)
    (org-todo-list)))

(global-set-key (kbd "C-c A") #'my/org-open-agenda-and-todo)

;; Export
(setq org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-htmlize-output-type 'css
      org-html-postamble nil
      org-export-with-toc nil
      org-export-with-title t)

(setq org-html-head
      (format "<link rel=\"stylesheet\" href=\"file://%s\">"
              (expand-file-name "~/css/executive-deluxe.css")))

(defun my/org-export-current-tree-to-html-to-desktop ()
  "Экспортирует текущий заголовок и его поддерево в HTML-файл."
  (interactive)
  (org-export-to-file
      'html
      (concat
       (expand-file-name "~/Desktop/")
       (file-name-sans-extension (file-name-nondirectory buffer-file-name))
       "_"
       (replace-regexp-in-string "[^[:alnum:]_-]" "_" (org-get-heading t t t t))
       ".html") nil t nil nil))

(general-define-key
 :keymaps 'org-mode-map
 :prefix "C-c e"
 "t" 'my/org-export-current-tree-to-html-to-desktop)

(defun my/org-create-daily-note ()
  "Create or open an Org mode file named with the current date for daily notes."
  (interactive)
  (let* ((notes-dir my-org-daily-path)
         (file-name (format-time-string "%Y-%m-%d.org" (current-time)))
         (full-path (expand-file-name file-name notes-dir)))
    (unless (file-exists-p notes-dir)
      (make-directory notes-dir t))
    (find-file full-path)
    (unless (file-exists-p full-path)
      (insert (format "#+TITLE: Daily Notes - %s\n" (format-time-string "%Y-%m-%d" (current-time))))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M" (current-time))))
      (insert (format "#+AUTHOR: %s\n" (user-full-name)))
      (insert "#+STARTUP: showeverything\n")
      (insert "\n")
      )))

(global-set-key (kbd "C-c n d") 'my/org-create-daily-note)

(provide 'org-config)
;;; org-config.el ends here
