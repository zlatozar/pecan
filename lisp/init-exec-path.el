;;; init-exec-path --- Take environment variables

;;; Commentary:

;; Usage:
;;
;; Customize `exec-path-from-shell-variables' to modify the list of
;; variables imported.

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    ;; Needed environment variables
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "SBCL_HOME")
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(provide 'init-exec-path)

;;; init-exec-path.el ends here
