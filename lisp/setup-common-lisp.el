;;; setup-common-lisp --- Configuration for Common Lisp

;;; Commentary:

;; Usage:

;;; Code:

;; Common Lisp HyperSpec location (this may vary)
(require 'hyperspec)

;; This may vary
(setq common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/")) ; <-- insert your path
      common-lisp-hyperspec-symbol-table
      (expand-file-name "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/Data/Map_Sym.txt"))   ; <-- insert your path

(defadvice common-lisp-hyperspec
  (around hyperspec-lookup-w3m () activate)
  (let* ((window-configuration (current-window-configuration))
         (browse-url-browser-function
          `(lambda (url new-window)
             (w3m-browse-url url nil)
             (let ((hs-map (copy-keymap w3m-mode-map)))
               (define-key hs-map (kbd "q")
                 (lambda ()
                   (interactive)
                   (kill-buffer nil)
                   (set-window-configuration
                    ,window-configuration)))
               (use-local-map hs-map)))))
    ad-do-it))

(provide 'setup-common-lisp)

;;; setup-common-lisp.el ends here
