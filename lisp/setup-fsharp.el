;; setup-fsharp --- F# configuration

;;; Commentary:

;;; Code:

(use-package fsharp-mode
             :ensure t
             :mode (("\\.fs[ix]?$" . fsharp-mode))
             :config (progn

                       (defun my/fsharp-hook ()
			 (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
                         (setq-local dash-at-point-docset "fs")
                         (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
                         (setq fsharp-compiler "/usr/bin/fsharpc")
			 (setq fsharp-doc-idle-delay .2)

                       (add-hook 'fsharp-mode-hook 'my/fsharp-hook)
		       (add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode)))

		       (bind-key "M-RET" 'fsharp-eval-region fsharp-mode-map)
		       (bind-key "C-SPC" 'fsharp-ac/complete-at-point fsharp-mode-map))
)

(provide 'setup-fsharp)

;;; setup-fsharp.el ends here
