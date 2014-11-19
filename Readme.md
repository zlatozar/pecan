### Pecan Emacs

This Emacs (ver. 24.4.1) configuration is suitable for programmers using one of the
following programming languages:

* _Python_
* _Common Lisp_
* _SWI Prolog_
* _Emacs Lisp_

### How to start using it?

- Take the source

```
$ cd ~
$ git clone http://github.com/zlatozar/pecan .emacs.d
```

- Download bootstrap packages

```
$ cd ~/.emacs.d/
$ make
```

- Run Emacs and all additional packages will be downloaded from **MELPA**

- Optional you can install **ANSI Common Lisp** specification as _Info_ page

```
$ cd ~/.emacs.d/
$ make cl-info
```

### Key Bindings

To see all key bindings defined in this configuration:

```
M-x describe-personal-keybindings
```

### Key Bindings Highlights

Directive  |  Interpretation
---------  |  --------------
C-h C-m    |  discover-my-major
C-c x n    |  manage-minor-mode
C-=        |  er/expand-region
C-c SPC    |  ace-jump-mode
qj         |  jump-char-forward
C-c x i    |  my/ispell-word-then-abbrev
qd         |  duplicate-thing
F2         |  dired-efap dired-mode-map
C-c n l    |  show line numbers
C-c n r    |  show relative line numbers
C-c n d    |  dedicated mode
C-c RET    |  wand:execute
qo         |  helm-occur
qp         |  helm-ag up to .git
F5         |  neotree-toggle
C-c n w    |  writeroom-mode
C-c x z    |  zap-up-to-char
C-c p f    |  cleanup-buffer
C-c C-z    |  REPLs
C-c ;      |  iedit ('M-H' narrow to function)
C-c x .    |  goto-last-change
C-c C-i    |  ido-imenu
M-.        |  go to definition
M-,        |  back from definition
C-c C-d    |  show documentation
C-c p y    |  (python) yas-ido-expand
C-M-left   |  ( a b (|c d) ) -> ( a (b |c d) )
C-M-right  |  ( a b (|c d) ) -> ( a b c (|d) )

Have fun!
