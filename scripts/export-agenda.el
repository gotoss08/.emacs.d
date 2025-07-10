(require 'org)
(require 'org-agenda)

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

(with-temp-buffer
  (org-agenda-list)
  (write-file (expand-file-name "~/OneDrive/org/agenda-today.txt")))
