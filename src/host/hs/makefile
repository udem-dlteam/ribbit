HOST = hs
HOST_INTERPRETER = $${HS_HOST_INTERPRETER:-runghc}
HOST_COMPILER = $${HS_HOST_COMPILER:-ghc -O2 -v0 -with-rtsopts=-N1 -threaded -rtsopts -o}
TEST_TAGS=hs core r4rs
TEST_FEATURES ?= -m
include ../../makefile-common.mk
