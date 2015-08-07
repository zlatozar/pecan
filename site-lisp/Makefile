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

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

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
all: prolog.elc

.el.elc:
	@ echo "\n** Bite compiling ===> " $@
	$(EMACS) -batch -f batch-byte-compile $<
	@ echo " **Finished."

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

.PHONY: clean

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
