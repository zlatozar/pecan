;;; setup-prolog --- Configure SWI Prolog

;;; Commentary:
;;; Hint: http://www.swi-prolog.org/gtrace.html

;; Usage:

;;; Code:

(require 'prolog)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swipl)

(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
                              auto-mode-alist))

(provide 'setup-prolog)

;;; setup-prolog.el ends here
