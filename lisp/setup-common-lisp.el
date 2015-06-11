;;; setup-common-lisp --- Configuration for Common Lisp

;;; Commentary:
;;; SLIME should be loaded first

;; Usage:

;;; Code:

(defadvice slime-hyperspec-lookup (around browse-with-eww activate)
  "Show hyperspec in EWW browser."
  (cl-flet ((browse-url (url) (eww-browse-url url)))
    ad-do-it))

(defadvice slime-repl-insert-prompt
    (after beginning-of-line-at-end-of-prompt () activate)
  "Define REPL behavior."
  (let ((inhibit-read-only t))
    (goto-char slime-repl-input-start-mark)
    (add-text-properties (line-beginning-position) (line-end-position)
                         '(read-only fence
                                     inhibit-line-move-field-capture t
                                     field output
                                     rear-nonsticky t
                                     front-sticky (field
                                                   inhibit-line-move-field-capture)
                                     fontified t))))

(provide 'setup-common-lisp)

;;; setup-common-lisp.el ends here
