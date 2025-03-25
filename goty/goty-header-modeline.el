;; goty-header-modeline.el   -*- lexical-binding: t; -*-
;; inspired by: https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4

(defun goty/header-modeline ()
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
  )

(provide 'goty-header-modeline)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
