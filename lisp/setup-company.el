;;; setup-company --- Completion with company-mode

;;; Commentary:

;; Usage:

;;; Code:

;;; Company mode for auto completion:

(use-package company
  :ensure t
  :diminish " ⍈"
  :bind (("C-<tab>" . company-complete)
         ("C-c n m" . global-company-mode))
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (setq company-idle-delay 0.03
;;          company-frontends (quote (company-pseudo-tooltip-frontend company-echo-metadata-frontend))
          company-minimum-prefix-length 3
          company-transformers '(company-sort-by-occurrence))
    (bind-keys :map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-d" . company-show-doc-buffer))))

(provide 'setup-company)

;;; setup-auto-complete.el ends here
