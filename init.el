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

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq load-prefer-newer t)

;;_______________________________________________________________________________
;;                                                               Manage packages

(require 'init-utils)

;; Load all packages installed with Makefile
(require 'load-site-lisp)

;; Machinery for installing required packages (needs `dash')
(require 'init-melpa)

;; Set up $PATH
(require 'init-exec-path)

;; Load packages that needed for bootstrap
(require-load 'use-package)
(require-load 'bind-key)

;; Benchmark of calls to Emacs require and load functions
(use-package benchmark-init
  :ensure t)

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
      major-mode 'text-mode
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
      save-abbrevs t
      require-final-newline t
      abbrev-file-name "~/.emacs.d/data/abbrev_defs"
      select-active-region t
      shift-select-mode nil
      x-select-enable-clipboard t
      auto-hscroll-mode t
      linum-format " %03d "
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
(setq-default abbrev-mode 1)
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
(menu-bar-mode -1)
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(tooltip-mode 1)
(electric-pair-mode 0)

;; shift-{arrows} to move between buffers
(windmove-default-keybindings)

(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Start maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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
    (key-chord-define-global "EE" 'server-edit)
    (key-chord-define-global "%%" 'ispell-buffer)
    ;; Pretty much everything in Enlish word beginning with 'q' is
    ;; follewed the vowel 'u'. These chords take advantage of that.
    (key-chord-define-global "qq" 'read-only-mode)
    (key-chord-define-global "qs" 'save-buffer)
    (key-chord-define-global "q0" 'delete-window)
    (key-chord-define-global "qv" 'vc-next-action)
    (key-chord-define-global "qh" 'mark-whole-buffer)
    (key-chord-define-global "qf" 'ido-find-file)))

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
  :idle (global-undo-tree-mode t)
  :diminish ""
  :init (setq undo-tree-visualizer-diff t))

;;; More buffer-related configuration.

;; Intelligently group together open buffers
(use-package kpm-list
  :ensure t)

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

;; 'M-w' is a prefix
(use-package easy-kill
  :ensure t
  :bind ("M-w" . easy-kill))

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
    (defun my/ispell-word-then-abbrev (p)
      "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
      (interactive "P")
      (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
        (call-interactively 'ispell-word)
        (setq aft (downcase (or (thing-at-point 'word) "")))
        (unless (string= aft bef)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob"))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)))))
  :bind ("C-c x i" . my/ispell-word-then-abbrev))

;; Distraction-free writing mode
(use-package writeroom-mode
  :ensure t
  :bind ("C-c n w" . writeroom-mode))

;; Centers the text of the window when there's only one window in the frame
(use-package centered-window-mode
  :ensure t
  :bind ("C-c m c" . centered-window-mode))

;; Imitate `narrow-to-region' with more eye-candy
(use-package fancy-narrow
  :ensure t
  :commands fancy-narrow-mode
  :config (fancy-narrow-mode 1))

;; Sticky window
(use-package dedicated
  :ensure t
  :bind ("C-c x d" . dedicated-mode))

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
  :config (setq smex-save-file "~/.emacs.d/data/smex-items")
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; Save point position between sessions
(use-package saveplace
  :ensure t
  :config (setq-default save-place t
                        save-place-file "~/.emacs.d/data/places"))

(use-package duplicate-thing
  :ensure t
  :config (key-chord-define-global "qd" 'duplicate-thing))

;; Edit filenames at-point in dired
(use-package dired-efap
  :ensure t
  :config (bind-key "<f2>" 'dired-efap dired-mode-map))

;; File browser
(use-package neotree
  :load-path "site-lisp/emacs-neotree/"
  :config (bind-key "<f5>" 'neotree-toggle))

;; Line numbers
(use-package linum
  :bind ("C-c n l" . linum-mode))

;; Relative line numbers
(use-package relative-line-numbers
  :ensure t
  :bind ("C-c n r" . relative-line-numbers-mode))

(use-package tex-mode
  :init
  (progn
    (add-hook 'tex-mode-hook (lambda () (typo-mode -1)))
    (add-hook 'tex-mode-hook (lambda () (flycheck-mode -1)))))

;; Shell using multi-term
(require 'setup-shell)

;; Auto complete
(require 'setup-auto-complete)

;; Org mode
(require 'setup-org-mode)

;;________________________________________________________________________________
;;                                                                    Programming

;; Flycheck
(use-package flycheck
  :load-path "site-lisp/flycheck/"
  :idle (global-flycheck-mode)
  :diminish "fc"
  :init (use-package flycheck-pos-tip
          :load-path "site-lisp/flycheck-pos-tip/")
  :config (setq-default flycheck-disabled-checkers
                        '(emacs-lisp-checkdoc))
  :bind ("C-c n f" . flycheck-mode))

;; Paredit during programming
(require 'setup-paredit)

;;; Tools

(require 'setup-programming-tools)
(require 'setup-yasnipped)

;;; Emacs Lisp

(require 'setup-emacs-lisp)

;;; Python:

(require 'setup-python)

;;; Common Lisp

;; 'C-h S' to describe symbol
(require 'info-look)
(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(require 'setup-slime)
(require 'setup-common-lisp)

;;; Clojure:


;;; Prolog

(require 'setup-prolog)

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

;; Allows users to select a piece of text and perform actions
;; based on predefined patterns
(use-package wand
  :ensure t
  :bind ("C-c RET" . wand:execute)
  :config
  (progn
    (setq wand:*rules*
          (list
           (wand:create-rule :match ">>> "
                             :capture :after
                             :action python-shell-send-string)
           (wand:create-rule :match "https?://"
                             :capture :whole
                             :action eww-browse-url)
           (wand:create-rule :match "file:"
                             :capture :after
                             :action find-file-other-window)))))

;; Dasily define search engines, bind them to keybindings
(use-package engine-mode
  :ensure t
  :commands (engine-mode defengine)
  :init (engine-mode t)
  :config
  (progn
    (setq engine/keymap-prefix (kbd "C-c e"))
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      "d")
    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      "g")
    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      "w")))

;;; Helm

(use-package helm-config
  :load-path "site-lisp/helm"
  :config
  (progn
    (key-chord-define-global "qm" 'helm-mini)
    (key-chord-define-global "qo" 'helm-occur)
    (key-chord-define-global "qf" 'helm-find-files)
    (use-package helm-ag
      :load-path "site-lisp/emacs-helm-ag"
      :config (key-chord-define-global "qp" 'helm-ag))
    (use-package helm-ls-git
      :load-path "site-lisp/helm-ls-git"
      :config
      (progn
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
