;;; setup-slime --- Slime configuration for Common Lisp

;;; Commentary:

;; Usage:

;;; Code:

(use-package slime
  :ensure t
  :config
  (progn
    (setq slime-protocol-version                  'ignore
          slime-net-coding-system                 'utf-8-unix
          slime-load-failed-fasl                  'never
          slime-kill-without-query-p              t
          slime-description-autofocus             t
          slime-repl-history-remove-duplicates    t
          slime-repl-history-trim-whitespaces     t
          slime-export-symbol-representation-auto t)

    (slime-setup '(slime-fancy)))
  :bind ("C-z" . slime-selector))

(use-package slime-company
  :ensure t
  :init
  (progn
    (add-hook 'slime-mode-hook 'my/slime-setup)
    (add-hook 'slime-repl-mode-hook 'my/slime-setup)))

;; SBCL (default)
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :init slime-init-command)))

;; Avoid clash with Pecan global key bindings
(unbind-key "C-c x" slime-mode-map)

;;_______________________________________________________________________________
;;                                                               Helper function

(defun my/slime-setup ()
  "Mode setup function for SLIME buffers."
  (slime-setup '(slime-company)))

(provide 'setup-slime)

;;; setup-slime.el ends here
