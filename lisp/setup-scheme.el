;;; setup-scheme --- Configure Guile and Geiser

;;; Commentary:

;; Usage:

;;; Code:

(use-package geiser
  :ensure t
  :init
  (use-package ac-geiser
    :ensure t
    :init (progn
            (add-hook 'geiser-mode-hook        'ac-geiser-setup)
            (add-hook 'geiser-repl-mode-hook   'ac-geiser-setup)
            (eval-after-load 'auto-complete
              '(add-to-list 'ac-modes 'geiser-repl-mode))))
  :config (progn
            (setq geiser-default-implementation 'guile)
            (setq geiser-active-implementations '(guile))
            (add-hook 'geiser-repl-mode-hook (lambda () (paredit-mode 1)))))

(provide 'setup-scheme)

;;; setup-scheme.el ends here
