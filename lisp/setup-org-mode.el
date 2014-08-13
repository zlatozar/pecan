;;; setup-org-mode --- Org-mode configuration

;;; Commentary:

;; Usage:

;;; Code:

(define-key global-map (kbd "C-c o l") 'org-store-link)
(define-key global-map (kbd "C-c o a") 'org-agenda)

(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

(provide 'setup-org-mode)

;;; setup-org-mode.el ends here

