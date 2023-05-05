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
	for prog in `ls ../../tests/*.scm tests/*.scm`; do \
	  setup=`sed -n -e '/;;;setup:/p' $$prog | sed -e 's/^;;;setup://'`; \
	  cleanup=`sed -n -e '/;;;cleanup:/p' $$prog | sed -e 's/^;;;cleanup://'`; \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  fancy_compiler=`sed -n -e '/;;;fancy-compiler/p' $$prog`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  if [ "$$setup" != "" ]; then \
        sh -c "$$setup"; \
	    if [ $$? != 0 ]; then \
			echo "Error in the setup"; \
		fi; \
	  fi; \
	  if [ "$$RSC_DEFAULT" = "$$RSC_COMPILER" ] && [ "$$fancy_compiler" = ";;;fancy-compiler" ]; then \
	    echo ">>> Skipped because it doesn't use the fancy compiler"; \
	  else \
	    rm -f test.$$host*; \
	    $$RSC_COMPILER -t $$host $$options -o test.$$host $$prog; \
	    if [ "$$INTERPRETER" != "" ]; then \
	      sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | $$INTERPRETER test.$$host > test.$$host.out; \
	    else \
	      $$COMPILER test.$$host.exe test.$$host; \
	      sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./test.$$host.exe > test.$$host.out; \
	    fi; \
	    sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.$$host.out; \
	    rm -f test.$$host*; \
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
