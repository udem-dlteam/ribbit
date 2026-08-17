HOST = c
HOST_COMPILER ?= $${C_HOST_COMPILER:-$(CC) -o}
TEST_TAGS ?= c core variadics r4rs flat-closure bench
TEST_FEATURES ?= -f+ compression/lzss/2b, -e optimal, -f+ c/gc/mark-sweep, --f+ flat-closure ,f+ c/gc/treadmill
include ../../makefile-common.mk
