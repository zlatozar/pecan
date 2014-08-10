;;; init-needed -- Provide functions that others rely on

;;; Commentary:

;; Usage:

;;; Code:

(use-package epl
  :ensure epl)

(use-package pkg-info
  :ensure pkg-info)

(use-package s
  :ensure s)

(use-package f
  :ensure f)

(use-package popup
  :ensure popup)

(use-package async
  :ensure async)

(use-package ht
  :ensure ht)

(use-package noflet
  :ensure noflet)

(use-package makey
  :ensure makey)

(use-package request
  :ensure request)

(use-package syntactic-sugar
  :ensure syntactic-sugar)

(use-package ample-regexps
  :ensure ample-regexps)

(provide 'load-needed)

;;; load-needed.el ends here
