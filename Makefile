#_______________________________________________________________________________
#
#               Makefile to bootstrap
#_______________________________________________________________________________
#

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

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
	@ cp reference/common-lisp/ansicl* /usr/share/info/
	@ echo "$(OK_COLOR)Now you can use 'C-h S' to refer Common Lisp specification$(NO_COLOR)"
