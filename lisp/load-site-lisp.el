;;; init-site-lisp --- Add everything from 'site-lisp' directory in load path

;;; Commentary:

;; Usage:

;;; Code:

(eval-when-compile (require 'cl))

(defun my/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (loop for dir in (directory-files parent-dir)
                   unless (string-match "^\\." dir)
                   collecting (expand-file-name dir))
             load-path)))))

(my/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

(provide 'load-site-lisp)

;;; load-site-lisp.el ends here
