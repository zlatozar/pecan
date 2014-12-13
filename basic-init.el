;;; basic-init --- Command line Emacs

;;; Commentary:
;;; This file contains configuration for 'command line' Emacs

;; Usage:

;;; Code:

;;________________________________________________________________________________
;;                                                                        Minimal

;; Highlight everywhere
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq default-buffer-file-coding-system 'utf-8)
(setq inhibit-startup-message t)
(setq make-backup-files nil)

;; spaces instead of tabs
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; no white spaces
(add-hook 'after-save-hook 'whitespace-cleanup)

;; Do not ask for safe variables
(setq enable-local-variables nil)

(show-paren-mode t)

(ido-mode 1)
(ido-everywhere 1)

(setq ido-ignore-buffers
      '("\\` " ".*Completion" "^ ?\\*")
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-faces nil
      ido-max-prospects 10)

;; modeline
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "cyan")
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "brightblack")

;; mini buffer
(set-face-foreground 'minibuffer-prompt "yellow")

;; set the name of the host and current path/file in title bar
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(provide 'basic-init)

;;; basic-init.el ends here
