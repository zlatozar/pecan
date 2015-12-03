;;; setup-prolog --- Configure SWI Prolog 7.2.3 or bigger

;;; Commentary:
;;; Hint: http://www.swi-prolog.org/gtrace.html
;;;       ?- gtrace, <rule>

;; Libraries: pack_install(func).     % Naming is hard
;;            pack_install(mavis).    % Optional typing. Type is predicate!
;;            pack_install(spawn).    % Concurrency

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
