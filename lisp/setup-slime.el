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
          slime-ed-use-dedicated-frame            nil
          slime-kill-without-query-p              t
          slime-description-autofocus             t
          slime-repl-history-remove-duplicates    t
          slime-repl-history-trim-whitespaces     t
          slime-enable-evaluate-in-emacs          t
          slime-export-symbol-representation-auto t)

    (bind-key "C-z" 'slime-selector slime-mode-map)
    (bind-key "TAB" 'company-indent-or-complete-common slime-mode-map)

    (slime-setup '(slime-fancy))))

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
