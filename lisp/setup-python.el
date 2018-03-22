;;; setup-python --- Python configuration

;;; Commentary:
;;;
;;; Install with 'pip': jedi, ipython, pyflakes

;; Usage:

;;; Code:

;; Jedi - python code completion library

(use-package python
  :ensure t
  :config  (progn
             (bind-key "C-c C-z" 'run-python python-mode-map)

             (bind-key "M-." 'jedi:goto-definition python-mode-map)
             (bind-key "M-," 'jedi:goto-definition-pop-marker python-mode-map)
             (bind-key "C-c C-d" 'jedi:show-doc python-mode-map)

             (defun my/company-jedi-setup ()
               (interactive)
               (use-package company-jedi
                 :ensure t
                 :config
                 (add-to-list 'company-backends 'company-jedi)))

             (add-hook 'python-mode-hook #'my/company-jedi-setup)))

;; 'pyflakes' should be in PATH
(use-package flymake-python-pyflakes
  :ensure t
  :config (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

;;_______________________________________________________________________________
;;                                                                   Indentation

(require 'python)

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)
            (setq python-fill-docstring-style 'onetwo)))

;;_______________________________________________________________________________
;;                                                                    Additional

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

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
