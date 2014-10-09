;;; setup-ido --- ido configuration

;;; Commentary:

;; Usage:

;;; Code:

(use-package ido
  :ensure ido
  :config
  (progn
    (ido-mode t)
    (setq ido-ignore-buffers
          '("\\` " ".*Completion" "^ ?\\*")
          ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-case-fold nil
          ido-auto-merge-work-directories-length -1
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-use-faces nil
          ido-max-prospects 10))
  :bind ("~" . my/ido-go-straight-home))

(defun my/ido-go-straight-home ()
  "Type ~ and it go to ~/.  Type it twice and go to ~/projects."
  (interactive)
  (cond
   ((looking-back "~/") (insert "projects/"))
   ((looking-back "/") (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

;;_______________________________________________________________________________
;;                                                                         Ido++

(use-package flx-ido
  :ensure flx-ido
  :config
  (progn
    (flx-ido-mode 1)))

(use-package ido-vertical-mode
  :ensure ido-vertical-mode
  :init (ido-vertical-mode))

;;_______________________________________________________________________________
;;                                                              Additional setup

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; Ignore following directories/files
(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "modules")
(add-to-list 'ido-ignore-files "\\.DS_Store")

(provide 'setup-ido)

;;; setup-ido.el ends here
