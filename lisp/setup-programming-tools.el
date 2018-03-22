;;; setup-programming-tools --- Helper tools during development

;;; Commentary:

;; Usage:

;;; Code:

;;_______________________________________________________________________________
;;                                                                           Git

;; Show changed lines
(use-package git-gutter
  :ensure t
  :diminish ""
  :config (global-git-gutter-mode 1))

;;_______________________________________________________________________________
;;

;; Parentheses view
(use-package highlight-parentheses
  :ensure t
  :config (add-hook 'prog-mode-hook
                    (lambda () (highlight-parentheses-mode t))))

;; FIXME/TODO/BUG in special face only in comments and strings
(use-package fic-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'fic-mode))

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

;; Press 'C-c ! l' to list of all Flycheck errors
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

;; Clean up trailing spaces
(add-hook 'prog-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(provide 'setup-programming-tools)

;;; setup-programming-tools.el ends here
