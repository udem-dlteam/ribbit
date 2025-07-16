HOST = asm
HOST_COMPILER ?= $${ASM_HOST_COMPILER:-./host/asm/asm2exe.sh -o}
TEST_TAGS ?= asm variadics core r4rs
TEST_FEATURES ?= -f+ compression/lzss/2b, -e optimal, -f+ compression/lzss/2b -e optimal
include ../../makefile-common.mk
