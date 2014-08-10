;;; setup-python --- Python configuration

;;; Commentary:

;; Usage:

;;; Code:

;; Jedi - python code completion library
(use-package jedi
  :ensure jedi
  :config (progn
            (setq jedi:setup-keys t
                  jedi:complete-on-dot t
                  jedi:tooltip-method '(pos-tip))
            (add-hook 'python-mode-hook (lambda () (paredit-mode -1)))
            (add-hook 'python-mode-hook (lambda () (jedi-mode 1)))
            (add-hook 'python-mode-hook 'auto-complete-mode)
            (add-hook 'python-mode-hook 'jedi:ac-setup)))

;; 'pyflakes' should be in PATH
(use-package flymake-python-pyflakes
  :ensure flymake-python-pyflakes
  :config (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

;;_______________________________________________________________________________
;;                                                                   Indentation

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))

;;_______________________________________________________________________________
;;                                                                     Debugging

(require 'python)

(defun python--add-debug-highlight ()
  "Add a highlighter for use by `python--pdb-breakpoint-string'."
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pytest; pytest.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'.")

(defun python-insert-breakpoint ()
  "Insert a python breakpoint using `pytest'."
  (interactive)
  (back-to-indentation)
  (split-line)
  (insert python--pdb-breakpoint-string))

(define-key python-mode-map (kbd "<f10>") 'python-insert-breakpoint)

(provide 'setup-python)

;;; setup-python.el ends here
