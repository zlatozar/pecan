;;; setup-clojure --- Clojure with CIDER configuration

;;; Commentary:

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

(provide 'setup-clojure)

;;; setup-clojure.el ends here
