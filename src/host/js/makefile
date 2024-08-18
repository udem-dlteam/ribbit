
HOST = js
HOST_INTERPRETER = $${JS_HOST_INTERPRETER:-node}

TEST_TAGS=js core variadics r4rs
TEST_FEATURES ?= -e original, -e optimal, -e optimal -f+ compression/lzss/tag, -m, -m -e optimal -f+ compression/lzss/tag
include ../../makefile-common.mk
