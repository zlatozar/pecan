;;; init-melpa --- Install packages from MELPA

;;; Commentary:

;; Usage:

;;; Code:

(require 'package)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(setq package-enable-at-startup nil
      package-check-signature nil)

(unless package-archive-contents
  (message "Refreshing package archives...")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

;; Load packages that needed for bootstrap

(require 'use-package)
(setq  use-package-verbose t)

(require 'bind-key)

(use-package dash
  :ensure t
  :commands dash-enable-font-lock)

(defun upgrade-packages ()
  "Upgrade all packages, no questions asked."
  (interactive)
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)))

(bind-key "C-c x u" 'upgrade-packages)

(provide 'init-melpa)

;;; init-melpa.el ends here
