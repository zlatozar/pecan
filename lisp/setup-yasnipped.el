;;; setup-yasnipped --- Defines 

;;; Commentary:

;; Use only own snippets, do not use bundled ones.

;; Usage:

;;; Code:

(use-package yasnippet
  :ensure yasnippet
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  :config (progn
            (setq yas-verbosity 1
                  yas-wrap-around-region t)
            (define-key yas-minor-mode-map [(tab)] nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil))
  :bind ("C-c p y" . yas-ido-expand))

(defun yas-ido-expand ()
  "Let you select (and expand) a yasnippet key."
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

(provide 'setup-yasnipped)

;;; setup-yasnipped.el ends here
