;;; setup-emacs-lisp -- Emacs programming made easy

;;; Commentary:

;; Usage:

;;; Code:

;; `go-to-definition' with M-. and back again with M-,
(use-package elisp-slime-nav
  :ensure t
  :diminish ""
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(defun my/byte-compile-current-elisp-file ()
  "Quick way to compile current buffer."
  (interactive)
  (byte-compile-file (buffer-file-name) t))

(bind-key "C-c l" 'my/byte-compile-current-elisp-file emacs-lisp-mode-map)

(use-package litable
  :ensure t
  :config (bind-key "C-c p t" 'litable-mode emacs-lisp-mode-map))

;; Interactive macro expansion
(use-package macrostep
  :ensure t
  :config (bind-key "C-c C-e" 'macrostep-expand emacs-lisp-mode-map))

(use-package eldoc
  :config
  (progn
    (use-package diminish
      :init (diminish 'eldoc-mode "eld"))
    (setq eldoc-idle-delay 0.2)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "blue"
                        :weight 'bold)))

;;_______________________________________________________________________________
;;                                                              Additional tools

(defun my/set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun my/ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))

;; Hit C-h f (for function) or C-h v (for variable)
(defun my/emacs-lisp-setup ()
  "Enable features useful when working with Emacs Lisp."
  (paredit-mode 1)
  (my/set-up-hippie-expand-for-elisp)
  (my/ielm-auto-complete)
  (ac-emacs-lisp-mode-setup)
  (activate-aggressive-indent)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-setup)
(add-hook 'ielm-mode-hook       'my/emacs-lisp-setup)

(defun my/visit-ielm ()
  "Open Emacs Lisp REPL."
  (interactive)
  (if (not (get-buffer "*ielm*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ielm))
    (switch-to-buffer-other-window "*ielm*")))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'my/visit-ielm)

(provide 'setup-emacs-lisp)

;;; setup-emacs-lisp.el ends here
