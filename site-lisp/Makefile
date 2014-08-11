#_______________________________________________________________________________
#
#               Makefile for Emacs package installation
#_______________________________________________________________________________
#

#_______________________________________________________________________________
#                                                                     VARIABLES

EMACS = emacs -q --no-site-file

EL=$(shell for f in $(PWD); do ls $$f/*.el; done)
ELC=$(EL:.el=.elc)

.SUFFIXES:	.el .elc

NO_COLOR    = \x1b[0m
OK_COLOR    = \x1b[32;01m
ERROR_COLOR = \x1b[31;01m
WARN_COLOR  = \x1b[33;01m

OK_STRING    = $(OK_COLOR)[OK]$(NO_COLOR)
ERROR_STRING = $(ERROR_COLOR)[ERRORS]$(NO_COLOR)
WARN_STRING  = $(WARN_COLOR)[WARNINGS]$(NO_COLOR)

EMACS_DEFAULT = ~/.emacs.d

#_______________________________________________________________________________
#                                                              HELPER FUNCTIONS

CHDIR_SHELL := $(SHELL)
define chdir

	$(eval _D=$(firstword $(1) $(@D)))
	$(info $(MAKE): cd $(_D)) $(eval SHELL = cd $(_D); $(CHDIR_SHELL))

endef

#_______________________________________________________________________________
#                                                                         RULES

default: all

# Do not forget to update .PHONY
all: dash flycheck flycheck-pos-tip slime helm helm-ag \
	 cider ac-cider prolog.elc

.el.elc:
	@ echo "\n** Bite compiling ===> " $@
	$(EMACS) -batch -f batch-byte-compile $<
	@ echo " **Finished."

#_______________________________________________________________________________
#                                                                     Bootstrap

dash:
	@ echo "$(OK_COLOR)===> Checkout or update dash.el...$(NO_COLOR)"
	@ if [ ! -d dash.el ]; then git clone https://github.com/magnars/dash.el.git; \
	  else cd dash.el; git pull --rebase; cd ..; fi;

#_______________________________________________________________________________
#                                                                      Flycheck

flycheck:
	@ echo "$(OK_COLOR)===> Checkout or update flycheck...$(NO_COLOR)"
	@ if [ ! -d flycheck ]; then git clone https://github.com/flycheck/flycheck.git; \
	  else cd flycheck; git pull --rebase; cd ..; fi;

flycheck-pos-tip:
	@ echo "$(OK_COLOR)===> Checkout or update flycheck-pos-tip...$(NO_COLOR)"
	@ if [ ! -d flycheck-pos-tip ]; then git clone https://github.com/flycheck/flycheck-pos-tip.git; \
	  else cd flycheck-pos-tip; git pull --rebase; cd ..; fi;

#_______________________________________________________________________________
#                                                                         Slime

slime: _ac-slime _hippie-expand-slime
	@ echo "$(OK_COLOR)===> Checkout or update slime...$(NO_COLOR)"
	@ if [ ! -d slime ]; then git clone http://github.com/slime/slime.git; \
	  else cd slime; git pull --rebase; cd ..; fi;
	@ cd slime; make; cd ..;

_ac-slime:
	@ echo "$(OK_COLOR)===> Checkout or update ac-slime...$(NO_COLOR)"
	@ if [ ! -d ac-slime ]; then git clone https://github.com/purcell/ac-slime.git; \
	  else cd ac-slime; git pull --rebase; cd ..; fi;

_hippie-expand-slime:
	@ echo "$(OK_COLOR)===> Checkout or update hippie-expand-slime...$(NO_COLOR)"
	@ if [ ! -d hippie-expand-slime ]; then git clone https://github.com/purcell/hippie-expand-slime.git; \
	  else cd hippie-expand-slime; git pull --rebase; cd ..; fi;

#_______________________________________________________________________________
#                                                                         Helm

helm:
	@ echo "$(OK_COLOR)===> Checkout or update helm...$(NO_COLOR)"
	@ if [ ! -d helm ]; then git clone https://github.com/emacs-helm/helm.git; \
	  else cd helm; git pull --rebase; cd ..; fi;

helm-ag:
	@ echo "$(OK_COLOR)===> Checkout or update helm-ag...$(NO_COLOR)"
	@ if [ ! -d emacs-helm-ag ]; then git clone https://github.com/syohex/emacs-helm-ag.git; \
	  else cd emacs-helm-ag; git pull --rebase; cd ..; fi;

#_______________________________________________________________________________
#                                                                       Clojure

cider:
	@ echo "$(OK_COLOR)===> Checkout or update to latest tag cider...$(NO_COLOR)"
	@ if [ ! -d cider ]; then git clone https://github.com/clojure-emacs/cider.git; \
	  else cd cider; git fetch --tags; cd ..; fi;
	@ cd cider; sha=`git rev-list --tags --max-count=1`; latestTag=`git describe --tags $$sha`; echo "Latest Tag: " $$latestTag; \
	  git checkout $$latestTag; cd ..;

ac-cider:
	@ echo "$(OK_COLOR)===> Checkout or update ac-cider...$(NO_COLOR)"
	@ if [ ! -d ac-cider ]; then git clone https://github.com/clojure-emacs/ac-cider.git; \
	  else cd ac-cider; git pull --rebase; cd ..; fi;

#_______________________________________________________________________________
#                                                                        Prolog

prolog.el:
	wget -N http://bruda.ca/_media/emacs/prolog.el

#_______________________________________________________________________________
#                                                     ADDITIONAL/OPTIONAL RULES

_v8:
	$(call chdir, $(EMACS_DEFAULT))
	@ echo "$(OK_COLOR)===> Checkout or update Google V8 engine...(it is slow and SCons is required)$(NO_COLOR)"
	@ if [ ! -d v8 ]; then svn checkout http://v8.googlecode.com/svn/trunk/ v8; else cd v8; svn up; cd ..; fi;
	@ cd v8; scons sample=shell; cd ..;
	@ echo "$(ERROR_COLOR)Please migrate build to use GYP and set 'GCC_TREAT_WARNINGS_AS_ERRORS': 'NO' in build/standalone.gypi\n$(NO_COLOR)"

jslint-v8: _v8
	@ echo "$(OK_COLOR)===> Checkout or update jslint-v8...$(NO_COLOR)"
	@ if [ ! -d jslint-v8 ]; then git clone http://github.com/valeryz/jslint-v8.git; else cd jslint-v8; git pull --rebase; cd ..; fi;
	@ echo "$(ERROR_COLOR)Set path in 'flymake-jslint.el'\n$(NO_COLOR)"

#_______________________________________________________________________________
#                                                                  SYSTEM RULES

.PHONY: clean dash flycheck flycheck-pos-tip slime ac-slime \
		hippie-expand-slime helm helm-ag cider ac-cider

clean:
	rm -f $(ELC)

distclean:
	rm -f $(EL) $(ELC) *~ */*~ .\#* */.\#*
usage:
	@ echo
	@ echo
	@ echo "usage: make [foo.el | foo.elc | slime | usage | clean | distclean]"
	@ echo "       make foo.el     will fetch foo.el from the Internet"
	@ echo "       make foo.elc    will byte-compile foo.el."
	@ echo "                       (fetching foo.el first if necessary)."
	@ echo "       make slime      checkout or update slime"
	@ echo
	@ echo "       make clean      will remove all .elc files in this dir."
	@ echo "       make distclean  will remove .el and .elc files in this dir."
	@ echo "       make usage      displays this message."
	@ echo
