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

RSC_COMPILER ?= gsi -:r4rs rsc.scm
REPL_PATH ?= lib/r4rs/repl.scm

TEST_FEATURES ?= .
RSC_MUST_TEST_FEATURES ?= ,
TEST_FILTER ?= *
TEST_DIR ?= tests

BENCH_OPTIONS ?= .
RSC_MUST_BENCH_OPTIONS ?= ,
BENCH_DIR ?= benchmarks
BENCH_FILTER ?= *
TEMP_DIR ?= .tests

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

check-repl: 
	@host="$(HOST)"; \
	INTERPRETER="$(HOST_INTERPRETER)"; \
	COMPILER="$(HOST_COMPILER)"; \
	RSC_COMPILER="${RSC_COMPILER}"; \
	RSC_TEST_FEATURES='${RSC_MUST_TEST_FEATURES}'; \
	TEST_FEATURES='${TEST_FEATURES}'; \
	TEST_DIR="${TEST_DIR}"; \
	REPL_PATH="${REPL_PATH}"; \
	pushd ../../; \
	if [ "$$TEST_FEATURES" != "." ]; then \
	  RSC_TEST_FEATURES="$$RSC_TEST_FEATURES;$$TEST_FEATURES"; \
	fi; \
	TEST_FILTER='${TEST_FILTER}'; \
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
	for test_feature in `echo "$$RSC_TEST_FEATURES" | sed -e 's/ /,/g' | sed -e 's/,*\;,*/\n/g'`; do \
	  if [ "$$test_feature" != "," ] && [ "$$test_feature" != "" ]; then \
	     echo "    >>> [test features: `echo "$$test_feature" | sed -e 's/,/ /g'`]"; \
	  fi; \
	  $$RSC_COMPILER -t $$host $$options -f+ quiet `echo "$$test_feature" | sed -e 's/,/ /g'` -o repl.$$host $$repl; \
	  for prog in `ls $$TEST_DIR/01-r4rs/$$TEST_FILTER.scm host/$$HOST/tests/$$TEST_FILTER.scm`; do \
	    echo "     testing in repl: $$prog"; \
	    if [ "$$INTERPRETER" != "" ]; then \
	      echo "(load \"$$prog\")" | $$INTERPRETER repl.$$host | tail -r | tail -n +3 | tail -r  > repl.$$host.out; \
	    else \
	      $$COMPILER repl.$$host.exe repl.$$host; \
		  echo "(load \"$$prog\")" | ./repl.$$host.exe | tail -r | tail -n +3 | tail -r > repl.$$host.out; \
	    fi; \
        sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - repl.$$host.out; \
        if [ "$$cleanup" != "" ]; then \
          sh -c "$$cleanup"; \
          if [ $$? != 0 ]; then \
		    echo "Error in the cleanup"; \
    	  fi; \
        fi; \
      done; \
	  rm -f test.$$host*; \
    done; \
	popd

check:
	@host="$(HOST)"; \
	INTERPRETER="$(HOST_INTERPRETER)"; \
	COMPILER="$(HOST_COMPILER)"; \
	RSC_COMPILER="${RSC_COMPILER}"; \
	RSC_TEST_FEATURES='${RSC_MUST_TEST_FEATURES}'; \
	TEST_FEATURES='${TEST_FEATURES}'; \
	TEST_DIR="${TEST_DIR}"; \
	TEMP_DIR="${TEMP_DIR}"; \
	pushd ../../; \
	if [ "$$TEST_FEATURES" != "." ]; then \
	  RSC_TEST_FEATURES="$$RSC_TEST_FEATURES;$$TEST_FEATURES"; \
	fi; \
	TEST_FILTER='${TEST_FILTER}'; \
	test_passed=0; \
	num_test=0; \
	mkdir -p $$TEMP_DIR; \
	for prog in `ls $$TEST_DIR/**/$$TEST_FILTER.scm host/$$HOST/tests/$$TEST_FILTER.scm`; do \
	  setup=`sed -n -e '/;;;setup:/p' $$prog | sed -e 's/^;;;setup://'`; \
	  cleanup=`sed -n -e '/;;;cleanup:/p' $$prog | sed -e 's/^;;;cleanup://'`; \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  argv=`sed -n -e '/;;;argv:/p' $$prog | sed -e 's/^;;;argv://'`; \
	  test_name=`basename $$prog`; \
	  echo "---------------------- $$prog [options:$$options] [argv:$$argv]"; \
	  if [ "$$setup" != "" ]; then \
      sh -c "$$setup"; \
	    if [ $$? != 0 ]; then \
	      echo "Error in the setup"; \
		  fi; \
	  fi; \
	  for test_feature in `echo "$$RSC_TEST_FEATURES" | sed -e 's/ /,/g' | sed -e 's/,*\;,*/\n/g'`; do \
	    if [ "$$test_feature" != "," ] && [ "$$test_feature" != "" ]; then \
	  	echo "    >>> [test features: `echo "$$test_feature" | sed -e 's/,/ /g'`]"; \
	    fi; \
	    test_path=$$TEMP_DIR/$$test_name.$$host; \
	    feature_list=`echo "$$test_feature" | sed -e 's/,/ /g'`; \
	    rm -f test_path; \
	    $$RSC_COMPILER -t $$host $$options $$feature_list -o $$test_path $$prog; \
	    if [ "$$INTERPRETER" != "" ]; then \
	      sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | $$INTERPRETER $$test_path "$$argv" > $$test_path.out; \
	    else \
	      $$COMPILER $$test_path.exe $$test_path; \
	      sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./$$test_path.exe "$$argv" > $$test_path.out; \
	    fi; \
	    sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - $$test_path.out; \
	    if [ $$? = 0 ]; then \
	      test_passed=$$(($$test_passed + 1)); \
	    fi; \
	    num_test=$$(($$num_test + 1)); \
			rm -f $$test_path*; \
	  done; \
	  if [ "$$cleanup" != "" ]; then \
      sh -c "$$cleanup"; \
	    if [ $$? != 0 ]; then \
	      echo "Error in the cleanup"; \
		  fi; \
	  fi; \
	done; \
	rmdir $$TEMP_DIR; \
	echo "!!! $$test_passed/$$num_test passed"; \
	popd

clean:
	@host="$(HOST)"; \
	rm -f test.$$host*
