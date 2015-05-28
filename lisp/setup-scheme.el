;;; setup-scheme --- Configure Racket and Geiser

;;; Commentary:

;; Usage:

;;; Code:

(use-package geiser
  :ensure t
  :init
  (use-package geiser-repl
    :config
    (add-hook 'geiser-repl-mode-hook 'paredit-mode))
  :config (progn
            (setq geiser-active-implementations '(guile racket)
                  geiser-mode-smart-tab-p t
                  geiser-repl-autodoc-p t
                  geiser-repl-history-filename "~/.emacs.d/data/geiser-history"
                  geiser-repl-query-on-kill-p nil
                  geiser-implementations-alist
                  '(((regexp "\\.scm$") guile)
                    ((regexp "\\.ss$") guile)
                    ((regexp "\\.rkt$") racket)))))

(provide 'setup-scheme)

;;; setup-scheme.el ends here
