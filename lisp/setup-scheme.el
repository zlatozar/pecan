;;; setup-scheme --- Configure Racket and Geiser

;;; Commentary:

;; Usage:

;;; Code:

(use-package geiser
  :ensure t
  :init
  (use-package ac-geiser
    :ensure t
    :init (add-hook 'geiser-mode-hook 'ac-geiser-setup))
  :config (progn
            (setq geiser-active-implementations '(racket))
            (add-hook 'geiser-repl-mode-hook (lambda () (paredit-mode 1)))))

(provide 'setup-scheme)

;;; setup-scheme.el ends here
