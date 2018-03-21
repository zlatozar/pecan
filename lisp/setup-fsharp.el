;;; setup-fsharp --- F# configuration

;;; Commentary:

;;; Code:

(defun cool/fsharp-hook ()
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (setq-local dash-at-point-docset "fs"))

(setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/bin/fsharpc")

(use-package fsharp-mode
  :mode (("\\.fs\\'" . fsharp-mode))
  :bind (:map inferior-fsharp-mode-map
         ("M-l" . eyebrowse-next-window-config)
         ("M-h" . eyebrowse-prev-window-config)
         ("C-h" . evil-window-left)
         ("C-j" . evil-window-down)
         ("C-k" . evil-window-up)
         ("C-l" . evil-window-right)))

(add-hook 'fsharp-mode-hook 'cool/fsharp-hook)

(provide 'setup-fsharp)

;;; setup-fsharp.el ends here
