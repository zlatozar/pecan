;;; setup-prolog --- Configure SWI Prolog

;;; Commentary:
;;; Hint: http://www.swi-prolog.org/gtrace.html

;; Usage: M-x run-prolog
;;        C-c C-b allows to evaluate a buffer with prolog code.

;;; Code:

(require 'prolog)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swipl)

(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
                              auto-mode-alist))

(provide 'setup-prolog)

;;; setup-prolog.el ends here
