;;; setup-forth --- GForth configuration

;;; Commentary:

;;; Code:

(defun my/forth-mode ()
  (setq forth-indent-level 4
        forth-minor-indent-level 2
        forth-hilight-level 3))

(use-package forth-mode
  :ensure t
  :if (executable-find "gforth")
  :mode "\\.f\\'"
  :defines (forth-indent-level
            forth-minor-indent-level
            forth-hilight-level)
  :init
  ;;(autoload 'forth-mode "/opt/install/gforth/gforth.el")
  (add-hook 'forth-mode-hook 'my/forth-mode))

(provide 'setup-forth)

;;; setup-forth.el ends here
