;;; setup-mit-scheme --- Configure MIT Scheme with SLIME

;;; Commentary:

;; Usage:

;;; Code:

(add-hook 'scheme-mode-hook (lambda () (slime-mode 1)))

;;; TODO work in progress

;;_______________________________________________________________________________
;;                                                                    Additional

(use-package scheme-complete
  :ensure scheme-complete
  :init (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
  :bind ("C-c c" . scheme-smart-complete))

(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)))

;;_______________________________________________________________________________
;;                                                                         Tools

;; Highlight macros
;; Example: (register-scheme-keywords '("defgen" "fluid-let"))
(defun register-scheme-keywords (keywords)
  (mapc #'(lambda (kword)
            (font-lock-add-keywords 'scheme-mode
                                    `((,(concat "\\(" kword "\\)") 1 font-lock-keyword-face))))
        keywords))

(provide 'setup-mit-scheme)

;;; setup-mit-scheme.el ends here
