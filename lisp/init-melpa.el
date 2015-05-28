;;; init-melpa --- Install packages from MELPA

;;; Commentary:

;; Usage:

;;; Code:

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("mepla-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("mepla" . "http://mepla.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load packages that needed for bootstrap
(require 'use-package)
(require 'bind-key)

(use-package dash
  :ensure t
  :commands dash-enable-font-lock)

(provide 'init-melpa)

;;; init-melpa.el ends here
