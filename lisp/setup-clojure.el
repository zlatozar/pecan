;;; setup-clojure --- Clojure with CIDER configuration

;;; Commentary:
;;; Not enabled yet.

;; Usage:

;;; Code:

(use-package cider
  :ensure t
  :config (progn
            (use-package clojure-mode
              :ensure t)
            (use-package clojure-cheatsheet
              :ensure t)
            (use-package clojure-test-mode
              :ensure t)
            (use-package ac-cider
              :ensure t)))

;;_______________________________________________________________________________
;;                                                                    Additional

(defun cider-repl-command (cmd)
  "Execute CMD on the cider repl."
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert cmd)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-reset ()
  "Assumes reloaded + tools.namespace is used to reload everything."
  (interactive)
  (save-some-buffers)
  (cider-repl-command "(user/reset)"))

(defun cider-reset-test-run-tests ()
  "Clojure test runner."
  (interactive)
  (cider-repl-reset)
  (cider-test-run-tests))

(define-key cider-mode-map (kbd "C-c r") 'cider-repl-reset)
(define-key cider-mode-map (kbd "C-c .") 'cider-reset-test-run-tests)

;;_______________________________________________________________________________
;;                                                        Cycle between () {} []

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun live-cycle-clj-coll ()
  "Convert the coll at (point) from (x) -> {x} -> [x] -> (x) recur."
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))
     ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "[" (substring (live-delete-and-extract-sexp) 1 -1) "]"))
     ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")"))
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))
    (goto-char original-point)))

(define-key clojure-mode-map (kbd "C-`") 'live-cycle-clj-coll)

(provide 'setup-clojure)

;;; setup-clojure.el ends here
