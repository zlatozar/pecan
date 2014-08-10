;;; setup-slime --- Slime configuration for Common Lisp and MIT Scheme

;;; Commentary:

;; Usage:

;;; Code:

(use-package slime
  :load-path "site-lisp/slime/"
  :init
  (use-package ac-slime
    :load-path "site-lisp/ac-slime/")
  (use-package hippie-expand-slime
    :load-path "site-lisp/hippie-expand-slime")

  :config
  (progn
    (setq slime-protocol-version 'ignore
          slime-net-coding-system 'utf-8-unix
          slime-complete-symbol*-fancy t
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    (slime-setup '(slime-repl slime-fuzzy slime-scheme))
    (add-hook 'slime-mode-hook 'my/slime-setup)
    (add-hook 'slime-repl-mode-hook 'my/slime-setup))

  :bind ("C-z" . slime-selector))

;; SBCL (default) and MIT Scheme
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :init slime-init-command)
        (mit-scheme ("mit-scheme") :init mit-scheme-init)))

;;________________________________________________________________________________
;;                                                               Helper Functions

(defun my/slime-setup ()
  "Mode setup function for SLIME LISP buffers."
  (set-up-slime-hippie-expand)
  (set-up-slime-ac t)
  (paredit-mode 1))

(provide 'setup-slime)

;;; setup-slime.el ends here
