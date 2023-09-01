HOST = js
HOST_INTERPRETER = $${JS_HOST_INTERPRETER:-node}
RSC_MUST_TEST_FEATURES ?= -e original; -e optimal -f+ compression/tag
RSC_MUST_BENCH_OPTIONS ?= -f+ js/node; -f+ js/node -e skip
include ../../makefile-common.mk
