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

;; Hook AC into `completion-at-point'
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

; Case sensitivity is important when finding matches
(setq ac-ignore-case nil)

(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(setq ac-auto-show-menu t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)

;; AC everywhere - hack
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))
(real-global-auto-complete-mode t)

;; Exclude very large buffers from `dabbrev'
(defun my/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'my/dabbrev-friend-buffer)

(provide 'setup-auto-complete)

;;; setup-auto-complete.el ends here
