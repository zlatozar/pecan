;;; init-exec-path --- Take environment variables

;;; Commentary:

;; Usage:
;;
;; Customize `exec-path-from-shell-variables' to modify the list of
;; variables imported.

;;; Code:

;; `use-package' is not loaded yet
(require-package 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Needed environment variables
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "SBCL_HOME")
(exec-path-from-shell-copy-env "PYTHONPATH")

(provide 'init-exec-path)

;;; init-exec-path.el ends here
