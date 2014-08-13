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
#                                                                      REQUIRED

default: bootstrap

bootstrap:
	@ echo "$(OK_COLOR)===> Download packages needed to bootstrap...$(NO_COLOR)"
	@ $(MAKE) -C site-lisp/
	@ echo "$(OK_COLOR)DONE$(NO_COLOR)"

#_______________________________________________________________________________
#                                                                      OPTIONAL

cl-info:
	@ echo "$(OK_COLOR)===> Install Common Lisp info pages...$(NO_COLOR)"
	@ cp -r reference/common-lisp/ /usr/share/info/
	@ echo "$(OK_COLOR)Now you can use 'C-h S' to refer Common Lisp specification$(NO_COLOR)"
