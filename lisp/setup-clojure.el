;;; setup-clojure --- Clojure with CIDER configuration

;;; Commentary:
;;; Not enabled yet.

;; Usage:

;;; Code:

(use-package cider
  :ensure cider
  :config (progn
            (use-package clojure-mode
              :ensure clojure-mode)
            (use-package clojure-cheatsheet
              :ensure clojure-cheatsheet)
            (use-package clojure-test-mode
              :ensure clojure-test-mode)
            (use-package clojurescript-mode
              :ensure clojurescript-mode)
            (use-package ac-nrepl
              :ensure ac-nrepl)

            (add-hook 'clojure-mode-hook 'cider-mode)
            (add-hook 'clojure-mode-hook 'clojure-test-mode)
;            (add-hook 'clojure-mode-hook '~load-paredit-mode)

            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;            (add-hook 'cider-repl-mode-hook '~load-paredit-mode)

            ;; ;; Moving inside subword
            (add-hook 'cider-repl-mode-hook 'subword-mode)

            ;; Hide *nrepl-connection* and *nrepl-server*
            (setq nrepl-hide-special-buffers t)

            (setq cider-repl-pop-to-buffer-on-connect t)

            (setq cider-popup-stacktraces nil)

            ;; Enable error buffer popping also in the REPL
            (setq cider-repl-popup-stacktraces t)

            (setq nrepl-buffer-name-separator "-")
            (setq nrepl-buffer-name-show-port t)

            (setq cider-repl-history-size 4096)

            ;; Auto-complete nREPL
            (require 'ac-nrepl)
            (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
            (add-hook 'cider-mode-hook 'ac-nrepl-setup)

            (eval-after-load 'auto-complete
              '(progn
                 (add-to-list 'ac-modes 'cider-repl-mode)
                 (defun set-auto-complete-as-completion-at-point-function ()
                   (setq completion-at-point-functions '(auto-complete)))
                 (add-hook 'auto-complete-mode-hook
                           'set-auto-complete-as-completion-at-point-function)
                 (add-hook 'cider-repl-mode-hook
                           'set-auto-complete-as-completion-at-point-function)
                 (add-hook 'cider-mode-hook
                           'set-auto-complete-as-completion-at-point-function)))))

(defun cider-repl-command (cmd)
  "Execute CMD on the cider repl."
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert cmd)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-reset ()
  "Assumes reloaded + tools.namespace is used to reload everything."
  (interactive)
  (save-some-buffers)
  (cider-repl-command "(user/reset)"))

(defun cider-reset-test-run-tests ()
  "Clojure test runner."
  (interactive)
  (cider-repl-reset)
  (cider-test-run-tests))

(define-key cider-mode-map (kbd "C-c r") 'cider-repl-reset)
(define-key cider-mode-map (kbd "C-c .") 'cider-reset-test-run-tests)

(provide 'setup-clojure)

;;; setup-clojure.el ends here
