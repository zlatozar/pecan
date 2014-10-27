;;; init-melpa --- Install packages from MELPA

;;; Commentary:

;; Usage:

;;; Code:

(require 'dash)

;; Syntax highlighting of dash functions
(eval-after-load "dash" '(dash-enable-font-lock))

(require 'package)

;; Add melpa to package repos
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  "Iterate over PACKAGES and install them."
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun require-load (package)
  "Simple function that install given PACKAGE and load it."
  (progn
    (require-package package)
    (require package)))

(provide 'init-melpa)

;;; init-melpa.el ends here
