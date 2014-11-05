;;; setup-shell --- Configure Emacs shell

;;; Commentary:
;;; It is very convenient to have following alias in `~/.bashrc':
;;;
;;; alias ec="emacsclient -c"
;;;
;;; In this way you can open file from `multi-term' very easily.

;; Usage:
;;     Emacs runs .bashrc in *shell*

;;; Code:

(use-package multi-term
  :ensure multi-term
  :config (progn
            (setq multi-term-program "/bin/bash")

            (defun scame-launch-term ()
              "Launch a new terminal."
              (interactive)
              (unless (multi-term-dedicated-exist-p)
                (multi-term-dedicated-open))
              (multi-term-dedicated-select))

            (key-chord-define-global "qe" 'scame-launch-term)))

(use-package tramp
  :config
  (progn
    (setq tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
          tramp-backup-directory-alist backup-directory-alist
          tramp-auto-save-directory "~/.emacs.d/data/tramp"

          ;; use the settings in ~/.ssh/config instead of Tramp's
          tramp-use-ssh-controlmaster-options nil
          ;; update vc ignore to include tramp files
          vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp)
          backup-enable-predicate
          (lambda (name)
            (and (normal-backup-enable-predicate name)
                 (not (let ((method (file-remote-p name 'method)))
                        (when (stringp method)
                          (member method '("su" "sudo"))))))))

    (use-package tramp-sh
      :config
      (progn
        (add-to-list 'tramp-remote-path "/usr/local/sbin")
        (add-to-list 'tramp-remote-path "~/bin")))))

(provide 'setup-shell)

;;; setup-shell.el ends here
