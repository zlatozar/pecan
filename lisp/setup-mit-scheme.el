;;; setup-mit-scheme --- Configure MIT Scheme

;;; Commentary:

;; Be sure that ~/.scheme.init has the following content:
;;
;;  (load-option 'format)
;;  (load "~/.emacs.d/site-lisp/mit-scheme-swank/swank.scm" (->environment '(runtime swank)))

;; Usage:

;;; Code:

(when (require 'slime nil t)

  (defun mit-scheme-start-swank (file encoding)
    (format "%S\n\n" `(start-swank ,file)))

  (defun mit-scheme-find-buffer-package ()
    (save-excursion
      (let ((case-fold-search t))
        (goto-char (point-min))
        (and (re-search-forward "^;+ package: \\(([^)]+)\\)" nil t)
             (match-string-no-properties 1)))))

  (defun mit-scheme-slime-mode-init ()
    (slime-mode t)
    (make-local-variable 'slime-find-buffer-package-function)
    (setq slime-find-buffer-package-function 'mit-scheme-find-buffer-package))

  (slime-setup)
  (if (not (memq 'mit-scheme slime-lisp-implementations))
      (setq slime-lisp-implementations
            (cons '(mit-scheme ("mit-scheme")
                               :init mit-scheme-start-swank)
                  slime-lisp-implementations)))
  (setq slime-default-lisp 'mit-scheme)
  (add-hook 'scheme-mode-hook 'mit-scheme-slime-mode-init))


(provide 'setup-mit-scheme)

;;; setup-mit-scheme.el ends here
