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
  :ensure git-messenger
  :bind ("C-c x g" . git-messenger:popup-message))

(use-package git-commit-mode
  :ensure git-commit-mode)

(use-package git-gutter+
  :ensure git-gutter+
  :config (global-git-gutter+-mode 1))

;; Browse file versions. Exit with 'q'.
(use-package git-timemachine
  :ensure git-timemachine)

;; Parentheses view
(use-package highlight-parentheses
  :ensure highlight-parentheses
  :config (add-hook 'prog-mode-hook
                  (lambda () (highlight-parentheses-mode t))))

;; Toggle tests for various programming languages
(use-package toggle-test
  :ensure toggle-test)

;; FIXME/TODO/BUG/KLUDGE in special face only in comments and strings
(use-package fic-mode
  :ensure fic-mode
  :config
  (progn (add-hook 'prog-mode-hook 'fic-mode)))

;; Make it easy to run `M-x compile` when saving source files:
(use-package recompile-on-save
  :ensure recompile-on-save
  :commands recompile-on-save)

;; Go to last change
(use-package goto-chg
  :ensure goto-chg
  :bind (("C-c x ." . goto-last-change)
         ("C-c x ," . goto-last-change-reverse)))

;; Interactive edit on multiple strings
(use-package iedit
  :ensure iedit
  :init (bind-key "C-c x ;" 'iedit-mode global-map))

(defun iedit-dwim (arg)
  "Start iedit for ARG but use `narrow-to-defun' to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

(bind-key "C-c ;" 'iedit-dwim global-map)

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

;; Clean up
(add-hook 'prog-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(provide 'setup-programming-tools)

;;; setup-programming-tools.el ends here
