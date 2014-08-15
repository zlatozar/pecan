###Emacs Configuration###

_(work in progress)_

####Programming###

If you use one of the following languages:

* _Python_
* _Common Lisp_
* _SWI Prolog_
* _Emacs Lisp_

this confiuguration is suitable for you.

###How to start using it###

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

###Key Bindings###

To see all key bindings defined in this configuration:

```
M-x describe-personal-keybindings
```

Have fun!
