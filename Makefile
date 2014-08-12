#_______________________________________________________________________________
#
#               Makefile to bootstrap
#_______________________________________________________________________________
#

NO_COLOR    = \x1b[0m
OK_COLOR    = \x1b[32;01m
ERROR_COLOR = \x1b[31;01m
WARN_COLOR  = \x1b[33;01m

OK_STRING    = $(OK_COLOR)[OK]$(NO_COLOR)
ERROR_STRING = $(ERROR_COLOR)[ERRORS]$(NO_COLOR)
WARN_STRING  = $(WARN_COLOR)[WARNINGS]$(NO_COLOR)

#_______________________________________________________________________________
#                                                                         RULES

bootstrap:
	@ echo "$(OK_COLOR)===> Download packages needed to bootstrap...$(NO_COLOR)"
	@ $(MAKE) -C site-lisp/
	@ echo "$(OK_COLOR)===> Configure MIT Scheme initialization...$(NO_COLOR)"
	@ cp ~/.emacs.d/data/.scheme.init ~/
	@ echo "$(OK_COLOR)DONE$(NO_COLOR)"
