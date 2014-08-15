;;; personal --- My Emacs preferences

;;; Commentary:

;; Usage:

;;; Code:

;; Increase font size (it is 160 on my Mac machine)
(set-face-attribute 'default nil :height 160)

;; Setup registers for files I commonly edit
(set-register ?b '(file . "~/Documents/brainstorming.org"))
(set-register ?s '(file . "~/.bashrc"))
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?j '(file . "~/notes.txt"))
(set-register ?m '(file . "~/mail.md"))
(set-register ?t '(file . "~/todo.org"))

;; First day of the week is Monday instead of Sunday
(setq european-calendar-style 't)
(setq calendar--week--start--day 1)

;; Currently I use MAC with Microsoft keyboard
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(provide 'emacs-personal)

;;; personal.el ends here
