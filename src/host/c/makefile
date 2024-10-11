HOST = c
HOST_COMPILER = $${C_HOST_COMPILER:-$(CC) -o}
TEST_TAGS=c core variadics r4rs
TEST_FEATURES ?= -f+ compression/lzss/2b, -e optimal, -f+ compression/lzss/2b -e optimal, -f+ c/gc/mark-sweep, -f+ c/gc/mark-sweep-dsw
include ../../makefile-common.mk
