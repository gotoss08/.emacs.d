;; -*- lexical-binding: t; -*-
(require 'org)
(require 'org-agenda)

(defvar my/telegram-bot-token "7935608191:AAGgCB0Vkv2Yg4yTfo1-Y4dopiZ7J1L8B1Q")
(defvar my/telegram-chat-id "597857039")
(defvar my/agenda-org-file (expand-file-name "~/org/generated-agenda.org"))
(defvar my/agenda-html-file (expand-file-name "~/org/generated-agenda.html")) ;; куда сохранять
(defvar my/agenda-md-file (expand-file-name "~/org/generated-agenda.md"))

(setq org-directory (expand-file-name "~/org"))
(setq my-org-path org-directory)

(setq my-org-inbox-path    (expand-file-name "inbox.org"       my-org-path))
(setq my-org-work-path     (expand-file-name "work.org"        my-org-path))
(setq my-org-personal-path (expand-file-name "personal.org"    my-org-path))
(setq my-org-someday-path  (expand-file-name "someday.org"     my-org-path))
(setq my-org-regulations   (expand-file-name "regulations.org" my-org-path))
(setq my-org-meetings      (expand-file-name "meetings.org"    my-org-path))
(setq my-org-projects      (expand-file-name "projects/"       my-org-path))

(setq org-agenda-files
      (list my-org-inbox-path
            my-org-work-path
            my-org-personal-path
            my-org-someday-path
            my-org-regulations
            my-org-meetings
	    my-org-projects))

(defun my/send-weekly-agenda-to-telegram ()
  (let* ((bot-token my/telegram-bot-token)
         (chat-id my/telegram-chat-id)
         (agenda-buffer (get-buffer-create "*weekly-agenda*"))
         (org-agenda-span 'week)
         (org-agenda-start-on-weekday 1)
         (org-agenda-custom-commands
          '(("w" "Weekly Agenda"
             ((agenda ""
                      ((org-agenda-span 'week)
                       (org-agenda-start-day "-mon")
                       (org-agenda-remove-tags t)
                       (org-agenda-prefix-format "  %-12:c%?-12t% s")
                       (org-agenda-overriding-header "")))))))
         (text nil))

    ;; Export agenda
    (with-current-buffer agenda-buffer
      (erase-buffer)
      (org-agenda nil "w")
      (setq text (buffer-string)))

    ;; Cleanup
    (setq text
          (with-temp-buffer
            (insert text)
            (goto-char (point-min))
            (when (re-search-forward "^Week-agenda.*\n" nil t)
              (replace-match ""))
            (goto-char (point-min))
            (while (re-search-forward "[\\`*_]" nil t)
              (replace-match " " nil nil))
            (buffer-string)))

    ;; Send via Telegram
    (let* ((url (format "https://api.telegram.org/bot%s/sendMessage" bot-token))
       (encoded-text (url-hexify-string (format "*Today's weekly agenda:*\n```\n%s\n```" text)))
       (curl-args (list "-s" "-X" "POST" url
                        "-H" "Content-Type: application/x-www-form-urlencoded"
                        "-d" (format "chat_id=%s" chat-id)
                        "-d" (format "text=%s" encoded-text)
                        "-d" "parse_mode=Markdown"
                        "-d" "disable_web_page_preview=true")))
  (apply #'call-process "curl" nil "*telegram-send*" nil curl-args)
  (message "Agenda sent to Telegram."))))
    
;; Entrypoint for batch
(my/send-weekly-agenda-to-telegram)

;; (message invocation-directory)
;; "c:/Users/gotos/scoop/apps/emacs/current/bin/emacs.exe" --batch -l "c:/Users/gotos/.emacs.d/scripts/send-agenda-to-telegram.el"
