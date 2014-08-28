;;; setup-auto-complete --- Complicated setup for auto-complete

;;; Commentary:

;; Usage:

;;; Code:

(require-package 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)

;; Get pop-ups with docs even if a word is uniquely completed
(setq ac-dwim nil)

;; to work with `flymake'
(ac-flyspell-workaround)

;;; Use Emacs' built-in TAB completion hooks to trigger auto-complete

;; Use 't when `auto-complete' is disabled
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(defun set-auto-complete-as-completion-at-point-function ()
  "Hook AC into `completion-at-point'."
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

; Case sensitivity is important when finding matches
(setq ac-ignore-case nil)

(set-default 'ac-sources
             '(ac-source-filename
               ac-source-functions
               ac-source-variables
               ac-source-symbols
               ac-source-features
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-dictionary
               ac-source-imenu
               ac-source-words-in-all-buffer))

(setq ac-auto-show-menu t
      ac-use-menu-map t
      ac-quick-help-delay 1
      ac-quick-help-height 60
      ac-comphist-file  "~/.emacs.d/data/ac-comphist.dat")

;; AC everywhere - hack
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))
(real-global-auto-complete-mode t)

(provide 'setup-auto-complete)

;;; setup-auto-complete.el ends here
