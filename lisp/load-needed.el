;;; init-needed -- Provide functions that others rely on

;;; Commentary:

;; Usage:

;;; Code:

(use-package epl
  :ensure t)

(use-package pkg-info
  :ensure t)

(use-package s
  :ensure t)

(use-package f
  :ensure t)

(use-package popup
  :ensure t)

(use-package async
  :ensure t)

(use-package ht
  :ensure t)

(use-package noflet
  :ensure t)

(use-package makey
  :ensure t)

(use-package request
  :ensure t)

(use-package syntactic-sugar
  :ensure t)

(use-package ample-regexps
  :ensure t)

(provide 'load-needed)

;;; load-needed.el ends here
