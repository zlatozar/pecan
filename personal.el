;;; personal --- My Emacs preferences

;;; Commentary:

;; Usage:

;;; Code:

;; Increase font size (it is 160 on my Mac machine)
(set-face-attribute 'default nil :height 160)

;; Setup registers for files I commonly edit
(set-register ?b '(file . "~/Documents/brainstorming.org"))
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?m '(file . "~/mail.md"))
(set-register ?n '(file . "~/notes.txt"))
(set-register ?t '(file . "~/todo.org"))
(set-register ?s '(file . "~/.bashrc"))

;; First day of the week is Monday instead of Sunday
(setq european-calendar-style 't)
(setq calendar--week--start--day 1)

;; Currently I use MAC with Microsoft keyboard
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(provide 'emacs-personal)

;;; personal.el ends here
