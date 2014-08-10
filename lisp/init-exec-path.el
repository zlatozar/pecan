;;; init-exec-path --- Take shell variables

;;; Commentary:

;; Usage:
;;
;; Customize `exec-path-from-shell-variables' to modify the list of
;; variables imported.

;;; Code:

;; `use-package' is not loaded yet
(require-package 'exec-path-from-shell)

(after-load 'exec-path-from-shell
  (dolist (var '("PYTHONPATH"  "CLASSPATH" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)

;;; init-exec-path.el ends here
