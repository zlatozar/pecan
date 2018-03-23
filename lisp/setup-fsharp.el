;;; setup-fsharp --- F# configuration

;;; Commentary:

;;; Code:

(use-package fsharp-mode
             :ensure t
             :mode (("\\.fs\\'" . fsharp-mode))
             :config (progn

                       (defun my/fsharp-hook ()
                         (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
                         (setq-local dash-at-point-docset "fs")

                         (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
                         (setq fsharp-compiler "/usr/bin/fsharpc"))

                       (add-hook 'fsharp-mode-hook 'my/fsharp-hook)))

(provide 'setup-fsharp)

;;; setup-fsharp.el ends here
