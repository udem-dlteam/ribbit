HOST = c
HOST_COMPILER = $${C_HOST_COMPILER:-$(CC) -o}
TEST_TAGS ?=c core variadics r4rs flat-closure
TEST_FEATURES ?= -f+ compression/lzss/2b, -e optimal, -f+ c/gc/mark-sweep, -f+ c/gc/mark-sweep-dsw, -f+ flat-closure
include ../../makefile-common.mk
