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

RSC_DEFAULT = ../../rsc
RSC_COMPILER ?= ${RSC_DEFAULT}
TEST_FEATURES ?= .
RSC_MUST_TEST_FEATURES ?= ,
TEST_FILTER ?= *
TEST_DIR ?= tests

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

check:
	@host="$(HOST)"; \
	INTERPRETER="$(HOST_INTERPRETER)"; \
	COMPILER="$(HOST_COMPILER)"; \
	RSC_COMPILER="${RSC_COMPILER}"; \
	RSC_DEFAULT="${RSC_DEFAULT}"; \
	RSC_TEST_FEATURES='${RSC_MUST_TEST_FEATURES}'; \
	TEST_FEATURES='${TEST_FEATURES}'; \
	TEST_DIR="${TEST_DIR}"; \
	IS_FANCY="$$([ "$$RSC_DEFAULT" != "$$RSC_COMPILER" ] && echo "yes")"; \
	if [ "$$TEST_FEATURES" != "." ]; then \
	  RSC_TEST_FEATURES="$$RSC_TEST_FEATURES;$$TEST_FEATURES"; \
	fi; \
	if [ "$$IS_FANCY" != "yes" ]; then \
	  RSC_TEST_FEATURES=","; \
	fi; \
	TEST_FILTER='${TEST_FILTER}'; \
	for prog in `ls ../../$$TEST_DIR/*.scm tests/*.scm | grep -E "$$TEST_FILTER"`; do \
	  setup=`sed -n -e '/;;;setup:/p' $$prog | sed -e 's/^;;;setup://'`; \
	  cleanup=`sed -n -e '/;;;cleanup:/p' $$prog | sed -e 's/^;;;cleanup://'`; \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  argv=`sed -n -e '/;;;argv:/p' $$prog | sed -e 's/^;;;argv://'`; \
	  fancy_compiler=`sed -n -e '/;;;fancy-compiler/p' $$prog`; \
	  echo "---------------------- $$prog [options:$$options] [argv:$$argv]"; \
	  if [ "$$setup" != "" ]; then \
        sh -c "$$setup"; \
	    if [ $$? != 0 ]; then \
			echo "Error in the setup"; \
		fi; \
	  fi; \
	  if [ "$$IS_FANCY" != "yes" ] && [ "$$fancy_compiler" = ";;;fancy-compiler" ]; then \
	    echo ">>> Skipped because it doesn't use the fancy compiler"; \
	  else \
	    for test_feature in `echo "$$RSC_TEST_FEATURES" | sed -e 's/ /,/g' | sed -e 's/,*\;,*/\n/g'`; do \
		  if [ "$$test_feature" != "," ] && [ "$$test_feature" != "" ]; then \
			echo "    >>> [test features: `echo "$$test_feature" | sed -e 's/,/ /g'`]"; \
	      fi; \
	      rm -f test.$$host*; \
	      $$RSC_COMPILER -t $$host $$options `echo "$$test_feature" | sed -e 's/,/ /g'` -o test.$$host $$prog; \
	      if [ "$$INTERPRETER" != "" ]; then \
	        sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | $$INTERPRETER test.$$host $$argv > test.$$host.out; \
	      else \
	        $$COMPILER test.$$host.exe test.$$host; \
	        sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./test.$$host.exe $$argv > test.$$host.out; \
	      fi; \
	      sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.$$host.out; \
	      rm -f test.$$host*; \
		done; \
	  fi; \
	  if [ "$$cleanup" != "" ]; then \
        sh -c "$$cleanup"; \
	    if [ $$? != 0 ]; then \
		  echo "Error in the cleanup"; \
		fi; \
	  fi; \
	done

clean:
	@host="$(HOST)"; \
	rm -f test.$$host*
