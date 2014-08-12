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
            (add-hook 'python-mode-hook (lambda () (yas-global-mode 1)))
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
;;                                                                    Additional

(use-package virtualenvwrapper
  :ensure virtualenvwrapper
  :commands venv-workon
  :config (progn
	    (setq venv-location "~/.virtualenvs/")
	    (venv-initialize-interactive-shells)
	    (venv-initialize-eshell)
	    (setq-default mode-line-format
			  (cons '(:exec venv-current-name)
				mode-line-format))
	    (add-hook 'python-mode-hook
		      (lambda ()
			(hack-local-variables)
			(setq python-shell-virtualenv-path
			      (f-join venv-location project-venv-name))
			(venv-workon project-venv-name)))))

(use-package jedi-direx
  :ensure jedi-direx
  :config (progn
	    (add-hook 'jedi-mode-hook 'jedi-direx:setup)
	    (add-hook 'jedi-mode-hook
		      (lambda ()
			(local-set-key (kbd "C-c p j")
				       'jedi-direx:pop-to-buffer)))))

(use-package helm-pydoc
  :ensure helm-pydoc
  :config (add-hook 'python-mode-hook
		    (lambda ()
		      (local-set-key (kbd "C-c p d") 'helm-pydoc))))

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
