;; goty-minibuffer.el   -*- lexical-binding: t; -*-
;; inspired by: https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4

(defun goty/minibuffer ()
  (defun minibuffer-setup ()
    (set-window-margins nil 3 0)
    (let ((inhibit-read-only t))
      (add-text-properties (point-min) (+ (point-min) 1)
                           `(display ((margin left-margin)
                                      ,(format "# %s" (substring (minibuffer-prompt) 0 1))))))
    (setq truncate-lines t))
  (add-hook 'minibuffer-setup-hook #'minibuffer-setup)
  )

(provide 'goty-minibuffer)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
