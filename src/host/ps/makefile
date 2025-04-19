HOST = ps
HOST_INTERPRETER ?= $${PS_HOST_INTERPRETER:-gs -q -dBATCH -sDEVICE=pdfwrite -dNOPAUSE -sOutputFile=ignore-me.pdf }
TEST_TAGS ?= ps core
TEST_FEATURES ?= -m

include ../../makefile-common.mk

