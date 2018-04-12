;;; personal --- My Emacs preferences

;;; Commentary:

;; Usage:

;;; Code:

;; Start maximized
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

(setq default-frame-alist
      '((top . 10) (left . 2)
        (width . 160) (height . 50)))

;; Increase font size (it is 160 on my Mac machine)
(set-face-attribute 'default nil :height 120)

;; Setup registers for files I commonly edit
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?n '(file . "~/notes.txt"))
(set-register ?s '(file . "~/.bashrc"))

;; First day of the week is Monday instead of Sunday
(setq european-calendar-style 't)
(setq calendar--week--start--day 1)

;; Mac users
;; (setq mac-command-modifier 'super)
;; (setq mac-option-modifier 'meta)

(provide 'emacs-personal)

;;; personal.el ends here
