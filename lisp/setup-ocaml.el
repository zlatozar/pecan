;;; setup-ocaml --- OCaml configuration

;;; Commentary:

;; Installation of Ubuntu/Debian systems
;;
;; # add-apt-repository ppa:avsm/ppa
;; # apt-get update
;; # apt-get install ocaml ocaml-native-compilers camlp4-extra opam build-essential m4

;; Follow OPAM instructions during installation
;;
;; $ opam init

;; $ opam install core
;; $ opam install async
;; $ opam install utop
;; $ opam install merlin
;; $ opam install tuareg
;; $ opam install ocp-indent
;; (optional) $ opam install oasis ounit bisect
;;
;; $ eval `opam config env`

;; Usage: Place in your OCaml project following files:

;; .dir-locals.el

;; ((tuareg-mode .
;;     ((utop-command . "utop -emacs -init <full path to .ocamlinit>")
;;      (compile-command . "make -C <full path to the project>"))))

;; .merlin (an example)

;; S src
;; B _build
;; B ~/.opam/system/lib/core
;; B ~/.opam/system/lib/async

;; PKG core
;; PKG async
;; FLG -w +a-4-40..42-44-45-48

;;; Code:

(use-package reason-mode
  :load-path "site-lisp"
  :config (add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)
                              (paredit-mode))))

(use-package opam
  :ensure t
  :preface
  (defun add-opam-load-path ()
    (interactive)
    (setq opam-share (substring (shell-command-to-string "opam config var prefix 2> /dev/null") 0 -1))
    (add-to-list 'load-path (concat opam-share "/share/emacs/site-lisp"))
    (setq refmt-command (concat opam-share "/bin/refmt")))
  :init
  (progn
    (opam-init)
    (add-opam-load-path)))

(use-package utop
  :ensure t
  :commands utop-minor-mode
  :init (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  :config
  (progn
    (add-hook 'utop-mode-hook 'paredit-mode)
    (add-hook 'utop-mode-hook  #'(lambda () (ocaml-bind-paredit utop-mode-map)))))

(use-package ocp-indent
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

(use-package tuareg
  :ensure t
  :preface
  (defun ocaml-bind-paredit (keymap)
    (bind-keys
     :map keymap
     ("C-M-f" . smie-forward-sexp-command)
     ("C-M-b" . smie-backward-sexp-command)
     ("[" . paredit-open-square)
     ("]" . paredit-close-square)
     ("{" . paredit-open-curly)
     ("}" . paredit-close-curly)
     ("}" . paredit-close-curly)
     ("<backspace>" . paredit-backward-delete)))
  :commands esk-tuareg-eval
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :config
  (progn
    (add-hook 'tuareg-mode-hook 'paredit-mode)
    (add-hook 'tuareg-mode-hook  #'(lambda () (ocaml-bind-paredit tuareg-mode-map)))
    (bind-keys
     :map tuareg-mode-map
     ("RET" . reindent-then-newline-and-indent)
     ("C-c C-c" . tuareg-eval-buffer)
     ("C-c h" . caml-help)
     ("C-c C-s" . utop))))

(use-package merlin
  :ensure t
  :init
  (progn
    ;; To use `merlin-locate' to go to the source of things installed with
    ;; 'opam', you first of all need to keep the source around when
    ;; installing, and let opam create .cmt files:
    ;;
    ;; Set this in ~/.bash_profile:
    ;;
    ;; export OPAMKEEPBUILDDIR=true
    ;; export OCAMLPARAM="_,bin-annot=1"
    ;; export OPAMBUILDDOC=true

    (setenv "OPAMKEEPBUILDDIR" "true")
    (setenv "OCAMLPARAM" "_,bin-annot=1")
    (setenv "OPAMBUILDDOC" "true")
    (add-hook 'tuareg-mode-hook 'merlin-mode))
  :config
  (progn
    (use-package merlin-company)
    (setq merlin-completion-types nil
          merlin-completion-arg-type nil
          merlin-completion-with-doc t
          merlin-completion-dwim nil
          merlin-error-after-save nil
          merlin-command 'opam
          merlin-default-flags '("-principal"))
    (define-key merlin-mode-map (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
    (define-key merlin-mode-map (kbd "C-c <down>") 'merlin-type-enclosing-go-down)))

(use-package flycheck-ocaml
  :ensure t
  :init (flycheck-ocaml-setup)
  :config (setq merlin-error-after-save nil))

(provide 'setup-ocaml)

;;; setup-ocaml.el ends here
