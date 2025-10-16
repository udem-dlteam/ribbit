#
# Ribbit's makefile
#
# This makefile provide help to run tests on various hosts, with various Scheme
# compilers or interpreters. Most of the configurations can be made by the two
# options below:

# OPTION -- The Scheme compiler to use. Predefined configurations.
# Options:
#  - gambit (default)
#  - none   (default when RSC is set)
#  - guile
#  - chicken
ifeq (${RSC},)
SCHEME_COMPILER ?= gambit
endif

# OPTION -- The command to invoke when calling the Ribbit Scheme Compile.
# This option is set automatically by the SCHEME_COMPILER value.
# Examples:
#  - ./rsc-gsc.exe (compiled version of rsc.scm, see recipe below)
#  - gsi rsc.scm (interpreted version by the gambit interpreter)
#  - guile -s rsc.scm (interpreted version by the guile interpreter)
# RSC ?= ./rsc-gsc.exe

# OPTION - Location of the Gambit Scheme Compiler.
GSC ?= gsc

# OPTION - Command line flags to pass to GSC.
GSC_OPTIONS ?=

# OPTION - Location of the guild compiler.
GUILD ?= guild

# OPTION - Location of the guile interpreter.
GUILE ?= guile



# == Default config for Gambit ==
ifeq (${SCHEME_COMPILER},gambit)

# Check availability of -:r4rs on Gambit.
ifeq ($(shell ${GSC} -:r4rs -e '' > /dev/null 2>&1; echo $$?), 0)
GSC_OPTIONS ?= -:r4rs
R4RS_WARNING ?= 0
else
R4RS_WARNING ?= 1
endif

RSC_DEPS ?= rsc-gsc.exe
RSC ?= ./rsc-gsc.exe
endif

# == Default config for Guile ==
ifeq (${SCHEME_COMPILER},guile)
RSC_DEPS ?= rsc-guile.exe
RSC ?= ./rsc-guile.exe
endif


HOSTS = c js py scm hs lua ml sh asm clj zig adb

all:

.PHONY: prebuilt

rsc.exe: ${RSC_DEPS}
	cp ${RSC_DEPS} rsc.exe

#
# GAMBIT CONFIG
#

rsc-gsc.exe: rsc.scm
	@echo "==> Checking Gambit install..."
	@if [ "${R4RS_WARNING}" = "1" ]; then \
		echo "WARNING: You are using an old version of Gambit, and some ribbit features might not work properly, especially regarding R4RS symbols."; \
		echo "It is suggested to use Gambit v4.9.4+. Type 'gsc -v' to see your version."; \
		echo "To install from sources, see https://github.com/gambit/gambit."; \
	fi
	@echo "==> Building 'rsc-gsc.exe' with ${GSC}..."
	@echo "This might take a while..."
	${GSC} ${GSC_OPTIONS} -exe -o rsc-gsc.exe rsc.scm

#
# Guile config
#

rsc-guile.go: rsc.scm
	@echo "==> Building 'rsc-guile.go' with ${GUILD}..."
	${GUILD} compile rsc.scm -o rsc-guile.go

rsc-guile.exe: rsc-guile.go
	@echo "==> Creating callable shell script in ./rsc-guile.exe"
	@# Write file to rsc-guile.exe
	@echo "${GUILE} -e '(load-compiled \"$(shell pwd)/rsc-guile.go\")' -- \$$@" > rsc-guile.exe
	chmod +x rsc-guile.exe

#
# Minify script, used by some VMs
#

scripts/minify: scripts/minify.scm
	${GSC} ${GSC_OPTIONS} -exe -o scripts/minify scripts/minify.scm

#
# Run tests
#

check: ${RSC_DEPS}
	@if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	    cd host/$$host; RSC_COMPILER="${RSC}" $(MAKE) check; cd ../..; \
	    if [ "$$?" != "0" ]; then \
	      exit 1; \
	    fi; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC}" $(MAKE) check; \
	  if [ "$$?" != "0" ]; then \
	    exit 1; \
	  fi; \
	fi

prebuilt:
	RIBBIT_BUILD_DIR=`pwd`/prebuilt RIBBIT_BUILD_OPTS="-m" $(MAKE) build-all

build-all:
	@for host in $(HOSTS); do \
	  echo "====================== BUILDING HOST $$host"; \
	  cd host/$$host; $(MAKE) build-all; cd ../..; \
	done

tests/*: all
	TEST_FILTER="$@" $(MAKE) check



check-repl: ${RSC_DEPS}
	@UP_CASE="(##vector-set! ##main-readtable 1 'upcase)"; \
	if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	  cd host/$$host; RSC_COMPILER="${RSC}" $(MAKE) check-repl; cd ../..; \
	  if [ "$$?" != "0" ]; then \
	    exit 1; \
	  fi; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC}" $(MAKE) check-repl; cd ../..; \
	  if [ "$$?" != "0" ]; then \
	    exit 1; \
	  fi; \
	fi

check-pipeline: check
check-all: check-pipeline check-fancy check-features check-bootstrap

check-features: ${RSC_DEPS}
	@host="$${HOST:-js}"; \
	error=0; \
	for host_path in ./tests/hosts/*.test; do \
		options=`sed -n -e '/;;;options:/p' $$host_path | sed -e 's/^;;;options://'`; \
	    nb_visible_original=`grep -c visible $$host_path`; \
		nb_hidden_original=`grep -c hidden $$host_path`; \
		echo "====================== TESTING FEATURE $$host_path (v:$$nb_visible_original, h:$$nb_hidden_original, options: '$$options')"; \
		cmd="${RSC} -t $$host $$options lib/empty.scm --rvm $$host_path -o .test-feature.REMOVE"; \
		$$cmd; \
		nb_visible_after=`grep -c visible .test-feature.REMOVE;` \
		nb_hidden_after=`grep -c hidden .test-feature.REMOVE`; \
		if [ "$$nb_visible_after" != "$$nb_visible_original" ] || [ "$$nb_hidden_after" != "0" ] ; then \
		  error=1; \
		  echo "Mismatch on visible/hidden."; \
			echo "  Visible expected : $$nb_visible_original "; \
			echo "  Visible got      : $$nb_visible_after "; \
			echo "  Hidden expected  : 0 (found $$nb_hidden_original in original file)"; \
			echo "  Hidden got       : $$nb_hidden_after "; \
			echo ""; \
			echo "--- Original file ($$host_path): "; \
			echo ""; \
			cat $$host_path; \
			echo ""; \
			echo "--- Generated file ($$cmd) :"; \
			echo ""; \
	    cat .test-feature.REMOVE; \
		fi; \
	done; \
	if [ $$error -eq 1 ]; then \
	  exit 1; \
	fi;

check-bootstrap: rsc.exe
	@UP_CASE="(##vector-set! ##main-readtable 1 'upcase)"; \
	if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	    cd host/$$host; RSC_COMPILER="${RSC}" $(MAKE) check-bootstrap; cd ../..; \
	    if [ "$$?" != "0" ]; then \
	      exit 1; \
	    fi; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC}" $(MAKE) check-bootstrap; cd ../..; \
	  if [ "$$?" != "0" ]; then \
	    exit 1; \
	  fi; \
	fi

clean:
	@for host in $(HOSTS); do \
	  echo "====================== CLEANING HOST $$host"; \
	  cd host/$$host; $(MAKE) clean; cd ../..; \
	done
	rm -f rsc-bootstrap*

repl-asm.exe: rsc.exe
	./rsc.exe -t asm -l r4rs lib/r4rs/repl.scm -e optimal -f+ compression/lzss/2b -f+ prim-no-arity -v -x repl-asm.exe

repl-wat: rsc.exe
	@./rsc.exe -t wat -l r4rs lib/r4rs/repl.scm -v -o repl.wat && ./host/wat/run repl.wat

repl-wat-quiet: rsc.exe
	@./rsc.exe -t wat -l r4rs lib/r4rs/repl.scm -f+ quiet -v -o repl.wat && ./host/wat/run repl.wat

../docs/repl_r4rs.js: rsc.exe host/js/* lib/r4rs/* scripts/minify
	./rsc.exe -t js -f+ js/web -l r4rs -f+ compression/lzss/tag -m -e optimal lib/r4rs/repl.scm -f+ prim-no-arity -o ../docs/repl_r4rs.js

../docs/repl_r4rs_tc.js: rsc.exe host/js/* lib/r4rs/* scripts/minify
	./rsc.exe -t js -f+ js/web -f+ debug/repl -l r4rs/tc -f+ compression/lzss/tag -m -e optimal lib/r4rs/repl.scm -f+ prim-no-arity -o ../docs/repl_r4rs_tc.js

repl_r4rs: ../docs/repl_r4rs.js ../docs/repl_r4rs_tc.js
