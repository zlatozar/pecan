;;; setup-shell --- Configure Emacs shell

;;; Commentary:
;;; It is very convenient to have following alias in `~/.bashrc':
;;;
;;; alias ec="emacsclient -c"
;;;
;;; In this way you can open file from `multi-term' very easily.

;; Usage:
;;     Emacs runs .bashrc in *shell*

;;; Code:

(use-package multi-term
  :ensure multi-term
  :config (progn
            (setq multi-term-program "/bin/bash")

            (defun scame-launch-term ()
              "Launch a new terminal."
              (interactive)
              (unless (multi-term-dedicated-exist-p)
                (multi-term-dedicated-open))
              (multi-term-dedicated-select))

            (key-chord-define-global "qe" 'scame-launch-term)))

(provide 'setup-shell)

;;; setup-shell.el ends here
