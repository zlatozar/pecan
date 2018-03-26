;;; init --- Emacs initialization file

;;; Commentary:

;; Usage:
;;
;; GNU Emacs Configuration by Zlatozar Zhelyazkov
;;
;;     http://zlatozar.blogspot.com
;;     http://github.com/zlatozar/pecan
;;
;; Conventions for key bindings:
;;
;;     - C-c t:    Text commands.
;;     - C-c s:    Swoop commands.
;;     - C-c d:    Desktop commands.
;;     - C-c e:    Engine searching commands.
;;     - C-c m:    Major modes.
;;     - C-c n:    Minor modes.
;;     - C-c p:    Programming commands.
;;     - C-c o:    Org mode commands.
;;     - C-c x:    General commands.
;;
;; To see all bindings in this configurations type:
;;    'M-x describe-personal-keybindings'
;;
;; Tip: If you type 'C-c p' and you are note sure how to continue type 'C-h'.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq load-prefer-newer t)

;;_______________________________________________________________________________
;;                                                               Manage packages

(require 'init-utils)

;; Load all packages installed with Makefile
(require 'load-site-lisp)

;; Machinery for installing required packages
(require 'init-melpa)

;; Set up $PATH
(require 'init-exec-path)

;; Provides some package manager agnostic utilities
(use-package packed
  :ensure t)

;;_______________________________________________________________________________
;;                                                                         Eager

;; Alias 'emacs' to 'emacsclient' in my shell, so start server
(use-package server
  :config
  (progn
    (if (window-system)
        (if (server-running-p server-name)
            nil
          (progn
            (setq server-name "server-gui")
            (server-start)))
      (if (server-running-p server-name)
          nil
        (server-start)))))

(use-package auto-compile
  :ensure t
  :config
  (progn
    (auto-compile-on-load-mode 1)
    (auto-compile-on-save-mode 1)
    (setq auto-compile-display-buffer nil)
    (setq auto-compile-mode-line-counter t)))

;;_______________________________________________________________________________
;;               General, Global Settings, Minor Modes and Display Configuration

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(setq inhibit-startup-message t
      message-log-max 1000
      frame-resize-pixelwise t
      enable-local-variables nil
      make-backup-files nil
      auto-save-default t
      auto-save-interval 50
      auto-save-timeout 5
      delete-auto-save-files t
      case-fold-search t
      tooltip-delay 1
      initial-major-mode 'text-mode
      imenu-sort-function 'imenu--sort-by-name
      kill-read-only-ok t
      show-trailing-whitespace t
      size-indication-mode t
      read-quoted-char-radix 16
      line-move-visual nil
      initial-scratch-message ";; scratch buffer created -- happy hacking\n"
      delete-by-moving-to-trash t
      visible-bell nil
      save-interprogram-paste-before-kill t
      history-length 250
      tab-always-indent 'complete
      save-abbrevs 'silently
      require-final-newline t
      abbrev-file-name "~/.emacs.d/data/abbrev_defs"
      select-active-region t
      shift-select-mode nil
      select-enable-clipboard t
      auto-hscroll-mode t
      delete-active-region 'kill
      browse-url-browser-function 'eww-browse-url
      bookmark-default-file "~/.emacs.d/data/bookmarks")

;; Scrolling
(setq scroll-preserve-screen-position 'always
      scroll-conservatively           most-positive-fixnum
      scroll-step                     0)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setq-default truncate-lines t)
(setq-default abbrev-mode t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 90)

(transient-mark-mode t)
(delete-selection-mode t)
(column-number-mode t)
(show-paren-mode t)
(global-hi-lock-mode 1)
(which-function-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(tooltip-mode 1)
(electric-pair-mode 0)

;; shift-{arrows} to move between buffers
(windmove-default-keybindings)

(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Better rectangle manipulations
(cua-selection-mode 1)

;; Never kill scratch buffer
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(use-package fill-column-indicator
  :ensure t
  :config (setq fci-rule-column 80)
  :bind ("C-c x b" . fci-mode))

;;________________________________________________________________________________
;;                                                                  How to expand

;; Hippie expand configuration
(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

(setq hippie-expand-dabbrev-as-symbol nil)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;;________________________________________________________________________________
;;                                                                   Key Bindings

;; Remember keyboard shortcuts
(use-package guide-key
  :ensure t
  :init (guide-key-mode 1)
  :config (setq guide-key/guide-key-sequence '("C-c t" "C-c s"
                                               "C-c e" "C-c p "
                                               "C-c x")))

;;; Setup basic, global key bindings.

(bind-key "<RET>" 'newline-and-indent)
(bind-key "<C-return>" 'newline)
(bind-key "<M-return>" 'indent-new-comment-line)
(bind-key "M-/" 'hippie-expand)
(global-unset-key (kbd "C-z"))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
   point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;; These are commands mostly use for text editing.

(bind-key "C-c m t" 'text-mode)
(bind-key "C-c t a" 'align-regexp)
(bind-key "C-c t c" 'flyspell-auto-correct-word)
(bind-key "C-c t f" 'toggle-text-mode-auto-fill)
(bind-key "C-c t s" 'sort-lines)
(bind-key "C-c x s" (lambda ()
                      (interactive)
                      (switch-to-buffer "*scratch*")))
(bind-key "C-c x v" 'visit-tags-table)
(bind-key "C-c x w" 'whitespace-cleanup)

(use-package key-chord
  :ensure t
  :bind ("C-c n k" . key-chord-mode)
  :init (key-chord-mode 1)
  :config
  (progn
    ;; Pretty much everything in Enlish word beginning with 'q' is
    ;; follewed the vowel 'u'. These chords take advantage of that.
    (key-chord-define-global "q%" 'ispell-buffer)
    (key-chord-define-global "q0" 'delete-window)
    (key-chord-define-global "qe" 'server-edit)
    (key-chord-define-global "qf" 'ido-find-file)
    (key-chord-define-global "qn" 'read-only-mode)))

;;; Desktop management.

(bind-key "C-c d c" 'desktop-clear)
(bind-key "C-c d d" 'desktop-change-dir)
(bind-key "C-c d s" 'desktop-save)

;;________________________________________________________________________________
;;                                                           Third Party Packages

(require 'load-needed)

;; Use Ido everywhere
(require 'setup-ido)

;; A utility to help manage minor modes
(use-package manage-minor-mode
  :ensure t
  :commands manage-minor-mode
  :bind ("C-c x n" . manage-minor-mode))

;; Show key-bindings for the current major mode
(use-package discover-my-major
  :ensure t
  :commands discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;; Use Undo Tree instead of the Emacs default
(use-package undo-tree
  :ensure t
  :diminish ""
  :init (setq undo-tree-visualizer-diff t)
  :config (global-undo-tree-mode t))

;;; More buffer-related configuration.

;; Swap buffer positions
(use-package buffer-move
  :ensure t
  :bind (("<M-down>"  . buf-move-down)
         ("<M-up>"    . buf-move-up)
         ("<M-left>"  . buf-move-left)
         ("<M-right>" . buf-move-right)))

;;; These are some editing commands are used everywhere.

(defun my/move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my/move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Not applicable for Mac
(bind-key "<C-up>" 'my/move-line-up)
(bind-key "<C-down>" 'my/move-line-down)

;; Easily visit recently opened files
(use-package recentf-mode
  :init
  (progn
    (setq recentf-save-file "~/.emacs.d/data/recentf")
    (recentf-mode 1)
    ;; show files from previous session
    (add-hook 'emacs-startup-hook 'recentf-open-files))
  :bind ("<f8>" . recentf-open-files))

;;; Packages to navigate and edit text in semantic terms.

;; Increases the selected region by semantic units
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :config
  (progn
    (use-package change-inner
      :ensure t
      :bind (("M-i" . change-inner)
             ("M-o" . change-outer)))))

;;; These packages also help navigate through text but are more
;;; focused on jumping to specific characters or fixed positions.

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package ace-link
  :ensure t
  :commands ace-link-setup-default
  :config (ace-link-setup-default))

(use-package jump-char
  :ensure t
  :commands jump-char-forward
  :init
  (progn
    (key-chord-define-global "qj" 'jump-char-forward)))

;;; These are packages I use for plain text in general.

(use-package typo
  :ensure t
  :commands typo-mode
  :bind ("C-c n t" . typo-mode))

;; Easy way to double the number for example
(use-package operate-on-number
  :ensure t
  :commands operate-on-number-at-point
  :init (key-chord-define-global "NN" 'operate-on-number-at-point))

;; Minor mode to aid in finding common writing problems
(use-package writegood-mode
  :ensure t
  :commands writegood-mode
  :bind ("C-c n g" . writegood-mode))

(use-package flyspell
  :bind ("C-c n s" . flyspell-mode)
  :config (flyspell-mode 1)
  :init (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package abbrev
  :diminish ""
  :config
  (progn
    (defun endless/ispell-word-then-abbrev (p)
        "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
        (interactive "P")
        (let (bef aft)
          (save-excursion
            (while (progn
                     (backward-word)
                     (and (setq bef (thing-at-point 'word))
                          (not (ispell-word nil 'quiet)))))
            (setq aft (thing-at-point 'word)))
          (when (and aft bef (not (equal aft bef)))
            (setq aft (downcase aft))
            (setq bef (downcase bef))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                                    bef aft (if p "loc" "glob"))))))
  :bind ("C-c x i" . my/ispell-word-then-abbrev))

;; Distraction-free writing mode
(use-package writeroom-mode
  :ensure t
  :bind ("C-c n w" . writeroom-mode))

;; Imitate `narrow-to-region' with more eye-candy
(use-package fancy-narrow
  :ensure t
  :commands fancy-narrow-mode
  :config (fancy-narrow-mode 1))

;; Highlights the previously visible buffer part after each scroll
(use-package on-screen
  :ensure t
  :commands on-screen-mode
  :bind ("C-c n o" . on-screen-mode)
  :init (progn
          (add-hook 'eww-mode-hook 'on-screen-mode)
          (add-hook 'Info-mode-hook 'on-screen-mode)))

;; Displays current match and total matches in modeline
(use-package anzu
  :ensure t
  :diminish ""
  :config (global-anzu-mode 1))

;; Smart 'M-x'
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; Save cursor position between sessions
(use-package saveplace
  :ensure t
  :config (setq-default save-place t
                        save-place-file "~/.emacs.d/data/places"))

(use-package duplicate-thing
  :ensure t
  :config (key-chord-define-global "qd" 'duplicate-thing))

;; Edit filenames at-point in `dired' mode
(use-package dired-efap
  :ensure t
  :config (bind-key "<f2>" 'dired-efap dired-mode-map))

;; File browser
(use-package neotree
  :ensure t
  :bind (("<f5>" . neotree-toggle))
  :config (progn
            (bind-key "r" 'neotree-change-root neotree-mode-map)
            (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
            (bind-key "<right>" 'neotree-select-down-node neotree-mode-map)
            (setq neo-window-width 32
                  neo-create-file-auto-open t
                  neo-banner-message nil
                  neo-show-updir-line nil
                  neo-mode-line-type 'neotree
                  neo-smart-open t
                  neo-dont-be-alone t
                  neo-persist-show nil
                  neo-show-hidden-files t
                  neo-auto-indent-point t)))

;; Line numbers
(use-package linum
  :bind ("C-c n l" . linum-mode)
  ;; :config (setq linum-format "%4d \u2502 ")
  :config (setq linum-format "%4d"))

;; Shell using multi-term
(require 'setup-shell)

;; Auto complete
(require 'setup-company)

;;________________________________________________________________________________
;;                                                                    Programming

;; Flycheck
(use-package flycheck
  :ensure t
  :bind ("C-c n f" . flycheck-mode)
  :init
  (setq flycheck-idle-change-delay 0.3
        flycheck-highlighting-mode 'lines
        flycheck-indication-mode 'left-fringe
        flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

;; Parentheses during programming
(require 'setup-paredit)

;;; Tools

(require 'setup-programming-tools)

;;; Emacs Lisp

(require 'setup-emacs-lisp)

;;; Python:

(require 'setup-python)

;;; Common Lisp

(require 'setup-slime)
(require 'setup-common-lisp)

;;; Scheme

(require 'setup-scheme)

;;; Prolog

(require 'setup-prolog)

;;; F#

(require 'setup-fsharp)

;;; Forth

(require 'setup-forth)

;;________________________________________________________________________________
;;                                                            Programming Helpers

;;; Markdown

(use-package markdown-mode
  :ensure t
  :bind ("C-c m k" . markdown-mode)
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config (progn
            (add-hook 'markdown-mode-hook 'flyspell-mode)
            (add-hook 'markdown-mode-hook 'auto-fill-mode)))

;;; Helm

(use-package helm
  :ensure t
  :config
  (progn
    (key-chord-define-global "qm" 'helm-mini) ;; to switch buffers
    (key-chord-define-global "qo" 'helm-occur)
    (key-chord-define-global "qf" 'helm-find-files)
    (use-package helm-ag
      :ensure t
      :config (key-chord-define-global "qy" 'helm-ag))
    (use-package helm-ls-git
      :ensure t
      :config
      (progn
        (setq helm-ls-git-status-command 'magit-status)
        (key-chord-define-global "qp" 'helm-browse-project)))))

;; Search words through a whole buffer or across buffers
(use-package helm-swoop
  :ensure t
  :config (setq swoop-font-size-change: nil)
  :bind (("C-c s s" . helm-swoop)
         ("C-c s b" . helm-swoop-back-to-last-point)
         ("C-c s m" . helm-multi-swoop)
         ("C-c s a" . helm-multi-swoop-all)
         ("C-c s i" . helm-swoop-from-isearch)))

;;________________________________________________________________________________
;;                                                          Interactive Functions

(require 'load-defuns)

;;________________________________________________________________________________
;;                                                                       Epilogue

;; Load personal preferences
(setq personal-file "~/.emacs.d/personal.el")
(if (file-exists-p personal-file)
    (load personal-file 'noerror))

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

(provide 'init)

;;; init.el ends here
