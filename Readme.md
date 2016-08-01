### Pecan Emacs

This Emacs (ver. 24.4.1) configuration is suitable for programmers using one of the
following programming languages:

* _Common Lisp_
* _Emacs Lisp_
* _Racket Scheme_
* _OCaml_
* _Python_
* _SWI Prolog_

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
$ sudo make cl-info
```

### Key Bindings

To see all key bindings defined in this configuration:

```
M-x describe-personal-keybindings
```

### Key Bindings Highlights

Key&nbsp;Binding   | Interpretation               |  Description
---------   | --------------               |  --------------
`C-c x u`   |                              |  Upgrade all installed packages
`C-h C-m`   | `discover-my-major`          |  Shows available key bindings for current major mode
`C-c x n`   | `manage-minor-mode`          |  Manage minor mode
`C-=`       | `er/expand-region`           |  Increases the selected region by semantic units
`C-c SPC`   | `ace-jump-mode`              |  Enabling fast/direct cursor movement in current view
`qj`        | `jump-char-forward`          |  Navigate by char
`C-c x i`   | `my/ispell-word-then-abbrev` |  Calls `ispell-word` then create `abbrev`
`qd`        | `duplicate-thing`            |  Duplicate line
`F2`        |                              |  Rename file in a `dired-mode`
`C-c n l`   |                              |  Show line numbers
`C-c n r`   |                              |  Show **relative** line numbers
`C-c n d`   | `dedicated mode`             |  Pin buffer - `dedicated-mode`
`C-c RET`   | `wand:execute`               |  Execute command on selected text
`qo`        | `helm-occur`                 |  Find in a buffer
`qp`        | `helm-browse-project`        |  Find file in a .git project
`F5`        | `neotree-toggle`             |  File browser
`C-c n w`   | `writeroom-mode`             |  Write peacefully
`M-Z`       | `zap-up-to-char`             |  Deletes everything to given character **excluding**
`C-c p f`   | `cleanup-buffer`             |  Align file, clean up white spaces and remove trailing spaces
`C-c C-z`   |                              |  Switch to REPL
`C-c ;`     | `iedit`                      |  Edit one occurrence of word in a buffer (then `'M-H'` to narrow to function)
`C-c x .`   | `goto-last-change`           |  Go to the last change
`C-c C-i`   | `ido-imenu`                  |  Find method definition in the current buffer
`M-.`       |                              |  (_programming_) Go to definition
`M-,`       |                              |  (_programming_) Back from definition
`C-c ! l`   |                              |  (_programming_) List all Flycheck errors
`C-c C-d`   |                              |  (_programming_) Show documentation
`C-c p s`   |  `magit-status`              |  Git status
`C-c p g`   |  `magit-grep`                |  Grep git project
`C-c p y`   | `yas-ido-expand`             |  (_python_) Code templates
`C-return`  | `cua-selection-mode`         |  Edit rectangles
`M-w`       | `easy-kill`                  |  Various way to select and kill (`?` for help)
`C-M-left`  |                              |  `( a b (|c d) ) -> ( a (b |c d) )` move left parent to the **left**
`C-M-right` |                              |  `( a b (|c d) ) -> ( a b c (|d) )` move left parent to the **right**

Have fun!
