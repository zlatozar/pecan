;;; setup-shell --- Configure Emacs shell

;;; Commentary:

;; Usage:
;;     Emacs runs .bashrc in *shell*

;;; Code:

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(key-chord-define-global "qe" 'visit-term-buffer)

(provide 'setup-shell)

;;; setup-shell.el ends here
