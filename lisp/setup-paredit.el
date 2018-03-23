;;; setup-paredit --- Keybindings for paredit

;;; Commentary: Used only in `lisp-mode'

;; Usage:
;;      Keys:       Example:
;;
;;      "C-)"   -> ( (a b|) c (d) (e) ) => ( (a b |c) (d) (e) ) move ')' forward and join 'c'
;;      "C-}"   -> the opposite from previous one - move ')' backward and pull 'b'
;;
;;      "M-("   -> ( (a b) |c (d) (e) ) => ( (a b) (|c) (d) (e) ) wraps 'c' with '()'
;;      "M-\"   -> the same as previous one but wraps 'c' with double quote
;;
;;      "M-S"   -> ( (a |b) c (d) (e) ) => ( (a) | (b) c (d) (e) ) divide (a b) to (a) (b)
;;      "M-J"   -> the opposite from previous one joins (d) and (e) to (d e)
;;
;;      "M-s"   -> ( (a b) "c" (d) (e) ) => ( a b c (d) (e) ) remove '()' or double quote
;;      "M-r"   -> ( (|(0 1) a b) c (d) (e) ) => ( (0 1) c (d) (e) ) remove all outer '( )' and its content
;;
;;      "M-d"   -> ( (|a b) c (d) (e) ) => twice => ( () c (d) (e) ) remove one by one what is inside '( )'
;;      "M-DEL" -> the same as previous one but removes backward

;;; Code:

(use-package paredit
  :ensure t
  :diminish "(p)"
  :init (add-hook 'prog-mode-hook (lambda () (paredit-mode 1)))
  :bind (("C-M-l" . paredit-recentre-on-sexp)

         ("C-)"   . paredit-forward-slurp-sexp)
         ("C-}"   . paredit-forward-barf-sexp)
         ("M-\""  . paredit-meta-doublequote)
         ("M-S"   . paredit-split-sexp)
         ("M-J"   . paredit-join-sexp)
         ("M-s"   . paredit-splice-sexp)
         ("M-r"   . paredit-raise-sexp)
         ("M-d"   . paredit-forward-kill-word)
         ("M-DEL" . paredit-backward-kill-word)

         ("C-c p C" . paredit-convolute-sexp)
         ("C-c p a" . paredit-add-to-next-list)
         ("C-c p A" . paredit-add-to-previous-list)))

;; Making paredit work with `delete-selection-mode'
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

(global-set-key (kbd "C-M-u") #'paredit-backward-up)
(global-set-key (kbd "C-M-n") #'paredit-forward-up)

;; This one's surpisingly useful for writing prose.
(global-set-key "\M-S" #'paredit-splice-sexp-killing-backward)
(global-set-key "\M-R" #'paredit-raise-sexp)
(global-set-key "\M-(" #'paredit-wrap-round)
(global-set-key "\M-[" #'paredit-wrap-square)
(global-set-key "\M-{" #'paredit-wrap-curly)

(provide 'setup-paredit)

;;; setup-paredit.el ends here
