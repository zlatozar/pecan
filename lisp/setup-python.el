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
                  jedi:use-shortcuts t
                  jedi:tooltip-method '(pos-tip))
            (add-hook 'python-mode-hook
                      (lambda ()
                        (yas-global-mode 1)
                        (paredit-mode -1)
                        (jedi-mode 1)
                        (auto-complete-mode)
                        (add-to-list 'ac-sources 'ac-source-jedi-direct)
                        (jedi:ac-setup)))
            (defvar jedi-config:vcs-root-sentinel ".git")
            (defvar jedi-config:python-module-sentinel "__init__.py")

            (bind-key "M-." 'jedi:goto-definition jedi-mode-map)
            (bind-key "M-," 'jedi:goto-definition-pop-marker jedi-mode-map)
            (bind-key "C-c C-d" 'jedi:show-doc jedi-mode-map)
            (bind-key "C-c C-l" 'jedi:get-in-function-call jedi-mode-map)))

;; 'pyflakes' should be in PATH
(use-package flymake-python-pyflakes
  :ensure flymake-python-pyflakes
  :config (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

;;_______________________________________________________________________________
;;                                                                   Indentation

(require 'python)

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))

;;_______________________________________________________________________________
;;                                                                    Additional

;; "C-c C-p" to start it
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(use-package virtualenvwrapper
  :ensure virtualenvwrapper
  :commands venv-workon
  :config (progn
            (setq venv-location "~/.virtualenvs/")
            (venv-initialize-interactive-shells)
            (setq-default mode-line-format
                          (cons '(:exec venv-current-name) mode-line-format))))

(use-package jedi-direx
  :ensure jedi-direx
  :config (progn
            (add-hook 'jedi-mode-hook 'jedi-direx:setup)
            (bind-key "C-c p j" 'jedi-direx:pop-to-buffer jedi-mode-map)))

(use-package helm-pydoc
  :ensure helm-pydoc
  :config (bind-key "C-c C-d" 'helm-pydoc python-mode-map))

;;_______________________________________________________________________________
;;                                                                     Debugging

(defun python--add-debug-highlight ()
  "Add a highlighter for use by `python--pdb-breakpoint-string'."
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pytest; pytest.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'.")

(defun python-insert-breakpoint ()
  "Insert a python breakpoint using 'pytest' module."
  (interactive)
  (back-to-indentation)
  (split-line)
  (insert python--pdb-breakpoint-string))

(define-key python-mode-map (kbd "<f10>") 'python-insert-breakpoint)

(provide 'setup-python)

;;; setup-python.el ends here
