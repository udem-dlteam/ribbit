# common makefile for host languages

# The specific makefile must contain these lines:
#
# HOST = <host_language_extension>
# HOST_INTERPRETER = <host_interpreter_command>  # only if it is interpreted
# HOST_COMPILER = <host_compiler_command>        # only if it is compiled
# include ../../makefile-common.mk

# HOST_INTERPRETER takes a single argument, the source file to interpret

# HOST_COMPILER takes two arguments, the output file (executable binary) and
# the source file to compile

GSI ?= gsi
GSC ?= gsc
RSC_COMPILER ?= ${GSI} -:r4rs ../../rsc.scm
REPL_PATH ?= lib/r4rs/repl.scm

OUT_TEST ?= .tests

TEST_FEATURES ?= ,
TEST_FILTER ?= *
TEST_DIR ?= tests

BENCH_OPTIONS ?= .
RSC_MUST_BENCH_OPTIONS ?= ,
BENCH_DIR ?= benchmarks
BENCH_FILTER ?= *
TEMP_DIR ?= .tests

BOOT_FILE ?= rsc.scm
BOOT_FILE2 ?= ${BOOT_FILE}
BOOT_HOST ?= ${HOST}
BOOT0 ?=
BOOT1 ?= ${BOOT0}

all:

build-all: build-repl-min build-repl-max build-repl-max-tc build-rsc

build-repl-min: ../../repl-min.scm
	dir="$(RIBBIT_BUILD_DIR)"; $(RSC_COMPILER) -t $(HOST) -l min $(RIBBIT_BUILD_OPTS) -o $${dir:-build}/repl-min.$(HOST) $<

build-repl-max: ../../repl-max.scm
	dir="$(RIBBIT_BUILD_DIR)"; $(RSC_COMPILER) -t $(HOST) -l max $(RIBBIT_BUILD_OPTS) -o $${dir:-build}/repl-max.$(HOST) $<

build-repl-max-tc: ../../repl-max.scm
	dir="$(RIBBIT_BUILD_DIR)"; $(RSC_COMPILER) -t $(HOST) -l max-tc $(RIBBIT_BUILD_OPTS) -o $${dir:-build}/repl-max-tc.$(HOST) $<

build-rsc: ../../rsc.scm  
	dir="$(RIBBIT_BUILD_DIR)"; $(RSC_COMPILER) -t $(HOST) -l max $(RIBBIT_BUILD_OPTS) -o $${dir:-build}/rsc.$(HOST) $<

check-bootstrap:
	echo "====================== TESTING FIRST BOOSTRAP ===="
	cd ../.. && time ${RSC_COMPILER} -ps -t ${HOST} -l r4rs -l prim-wrap ${BOOT0} -l boot-host -e original -f+ v-port -o rsc-bootstrap1.${HOST} ${BOOT_FILE}
	@cd ../..; \
	if [ "${HOST_INTERPRETER}" != "" ]; then \
		time ${HOST_INTERPRETER} rsc-bootstrap1.${HOST} -ps -t ${HOST} -l r4rs -l prim-wrap ${BOOT0} -l boot-host -e original -f+ v-port -o rsc-bootstrap2.${HOST} ${BOOT_FILE}; \
  else \
		${HOST_COMPILER} rsc-bootstrap1.exe -g rsc-bootstrap1.${HOST}; \
		time ./rsc-bootstrap1.exe -ps -t ${HOST} -l r4rs -l prim-wrap ${BOOT0} -l boot-host -f+ v-port -e original -o rsc-bootstrap2.${HOST} ${BOOT_FILE}; \
	fi 


check-repl:
	@host="$(HOST)"; \
	INTERPRETER="$(HOST_INTERPRETER)"; \
	COMPILER="$(HOST_COMPILER)"; \
	RSC_COMPILER="${RSC_COMPILER}"; \
	TEST_FEATURES='${TEST_FEATURES}'; \
	TEST_DIR="${TEST_DIR}"; \
	REPL_PATH="${REPL_PATH}"; \
	cd ../../; \
	TEST_FILTER='${TEST_FILTER}'; \
	error=0; \
	repl="$$REPL_PATH"; \
	setup=`sed -n -e '/;;;setup:/p' $$repl | sed -e 's/^;;;setup://'`; \
	cleanup=`sed -n -e '/;;;cleanup:/p' $$repl | sed -e 's/^;;;cleanup://'`; \
	options=`sed -n -e '/;;;options:/p' $$repl | sed -e 's/^;;;options://'`; \
	argv=`sed -n -e '/;;;argv:/p' $$repl | sed -e 's/^;;;argv://'`; \
	echo "---------------------- $$repl [options:$$options] [argv:$$argv]"; \
	if [ "$$setup" != "" ]; then \
	  sh -c "$$setup"; \
	  if [ $$? != 0 ]; then \
	    echo "Error in the setup"; \
	  	exit 1; \
	  fi; \
	fi; \
	for test_feature in "" `echo "$$TEST_FEATURES" | tr ',' '\n' | sed -e 's/ /,/g'`; do \
	  test_feature=`echo "$$test_feature" | sed -e 's/,/ /g'`; \
	  if [ "$$test_feature" != "," ] && [ "$$test_feature" != "" ]; then \
	     echo "    >>> [test features: `echo "$$test_feature" | sed -e 's/,/ /g'`]"; \
	  fi; \
	  echo $$RSC_COMPILER -t $$host $$options -f+ quiet `echo "$$test_feature" | sed -e 's/,/ /g'` -o repl.$$host $$repl; \
	  $$RSC_COMPILER -t $$host $$options -f+ quiet `echo "$$test_feature" | sed -e 's/,/ /g'` -o repl.$$host $$repl; \
		if [ "$$?" != "0" ]; then \
	    echo "Could not build repl"; \
			exit 1; \
	  else \
	    for prog in `ls $$TEST_DIR/01-r4rs/$$TEST_FILTER.scm host/$$HOST/tests/$$TEST_FILTER.scm`; do \
	      echo "     testing in repl: $$prog"; \
	      if [ "$$INTERPRETER" != "" ]; then \
	        echo "(load \"$$prog\")" | $$INTERPRETER repl.$$host > repl.$$host.out; \
					if [ "$$?" != "0" ]; then \
					  error=1; \
					fi; \
	      else \
	        $$COMPILER repl.$$host.exe repl.$$host; \
					if [ "$$?" != "0" ]; then \
					  error=1; \
					fi; \
	        echo "(load \"$$prog\")" | ./repl.$$host.exe > repl.$$host.out; \
					if [ "$$?" != "0" ]; then \
					  error=1; \
					fi; \
	      fi; \
	      lines=$$(wc -l < repl.$$host.out); \
	      lines_to_keep=$$((lines - 2)); \
	      head -n $$lines_to_keep repl.$$host.out > repl.$$host.expected; \
	      sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - repl.$$host.expected; \
	      if [ "$$?" != "0" ]; then \
	        error=1; \
	      fi; \
	      if [ "$$cleanup" != "" ]; then \
	        sh -c "$$cleanup"; \
					if [ "$$?" != "0" ]; then \
	          echo "Error in the cleanup"; \
	        fi; \
	      fi; \
	    done; \
	  rm -f test.$$host*; \
	  fi; \
	done; \
	if [ "$$error" = "1" ]; then \
	  exit 1; \
	fi

check:
	@cd ../..; \
	succeded=1; \
	for prog in `ls ${TEST_DIR}/**/${TEST_FILTER}.scm host/${HOST}/tests/${TEST_FILTER}.scm`; do \
		HOST="${HOST}" \
		HOST_COMPILER="$(HOST_COMPILER)" \
		HOST_INTERPRETER="$(HOST_INTERPRETER)" \
		RSC_COMPILER="${RSC_COMPILER}" \
		TEST_TAGS="${TEST_TAGS}" \
		TEST_FEATURES="${TEST_FEATURES}" \
		./run_test.sh $$prog; \
		if [ $$? != 0 ]; then \
			succeded=0; \
		fi; \
	done; \
	if [ $$succeded != 1 ]; then \
		exit 1; \
	fi

clean:
	@host="$(HOST)"; \
	rm -f test.$$host*
