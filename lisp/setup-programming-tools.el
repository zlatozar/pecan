;;; setup-programming-tools --- Helper tools during development

;;; Commentary:

;; Usage:

;;; Code:

(use-package conf-mode
  :mode ((".gitignore" . conf-mode)
         (".gitconfig" . conf-mode)))

(use-package diff-mode
  :mode ("COMMIT_EDITMSG" . diff-mode))

(use-package git-messenger
  :ensure t
  :bind ("C-c x g" . git-messenger:popup-message))

(use-package git-commit-mode
  :ensure t)

(use-package git-gutter+
  :ensure t
  :diminish ""
  :config (global-git-gutter+-mode 1))

;; Browse file versions. Exit with 'q'.
;; As alternative use 'C-x v g'
(use-package git-timemachine
  :ensure t)

;; Parentheses view
(use-package highlight-parentheses
  :ensure t
  :config (add-hook 'prog-mode-hook
                    (lambda () (highlight-parentheses-mode t))))

;; Toggle tests for various programming languages
(use-package toggle-test
  :ensure t)

;; FIXME/TODO/BUG/KLUDGE in special face only in comments and strings
(use-package fic-mode
  :ensure t
  :diminish ""
  :config
  (progn
    (use-package diminish
      :init (diminish 'fic-mode ""))
    (add-hook 'prog-mode-hook 'fic-mode)))

(use-package highlight-numbers
  :ensure t
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Make it easy to run `M-x compile` when saving source files:
(use-package recompile-on-save
  :ensure t
  :commands recompile-on-save)

;; Go to last change
(use-package goto-chg
  :ensure t
  :bind (("C-c x ." . goto-last-change)
         ("C-c x ," . goto-last-change-reverse)))

;; Interactive edit on multiple strings ('M-H' - narrow to function)
(use-package iedit
  :ensure t
  :init (bind-key "C-c ;" 'iedit-mode global-map))

;; Close when compilation is successful
(defun bury-compile-buffer-if-successful (buf str)
  "Bury a compilation buffer if succeeded without warnings.
Optionally BUF and STR could be passed."
  (if (and
       (string-match "compilation" (buffer-name buf))
       (string-match "finished" str)
       (not (with-current-buffer buf
              (save-excursion
                (goto-char (point-min))
                (search-forward "warning" nil t)))))
      (run-with-timer 5 nil
                      (lambda (b)
                        (with-selected-window (get-buffer-window b)
                          (kill-buffer-and-window))) buf)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Clean up trailing spaces
(add-hook 'prog-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; Add #' in lambda expressions
(defun my/sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4))
      (insert "'"))))

(define-key lisp-mode-shared-map "#" #'my/sharp)

;;________________________________________________________________________________
;;                                                              Aggressive indent

(use-package aggressive-indent
  :ensure t)

(defun indent-defun ()
  "Indent current defun.

Do nothing if mark is active (to avoid deactivaing it), or if
buffer is not modified (to avoid creating accidental modifications)."
  (interactive)
  (unless (or (region-active-p)
              buffer-read-only
              (null (buffer-modified-p)))
    (let ((l (save-excursion (beginning-of-defun 1) (point)))
          (r (save-excursion (end-of-defun 1) (point))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (indent-region l r)))))

(defun activate-aggressive-indent ()
  "Locally add `indent-defun' to `post-command-hook'."
  (add-hook 'post-command-hook
            'indent-defun nil 'local))

(provide 'setup-programming-tools)

;;; setup-programming-tools.el ends here
