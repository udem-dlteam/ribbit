HOSTS = c js py scm hs lua ml sh asm clj zig

#RSC_COMPILER ?= gsi -:r4rs rsc.scm
GSI ?= gsi
GSC ?= gsc
RSC_COMPILER ?= ./rsc.exe

all:

.PHONY: prebuilt

scripts/minify: scripts/minify.scm
	${GSC} -exe -o scripts/minify scripts/minify.scm


check: rsc.exe
	@if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	    cd host/$$host; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check; cd ../..; \
	    if [ "$$?" != "0" ]; then \
	      exit 1; \
	    fi; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check; \
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
	

rsc.exe: rsc.scm
	@echo "====================== BUILDING RSC.EXE"
	@echo "This process may take a while..."
	${GSC} -:r4rs -exe -o rsc.exe rsc.scm


check-repl: rsc.exe
	@UP_CASE="(##vector-set! ##main-readtable 1 'upcase)"; \
	if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	  cd host/$$host; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check-repl; cd ../..; \
	  if [ "$$?" != "0" ]; then \
	    exit 1; \
	  fi; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check-repl; cd ../..; \
	  if [ "$$?" != "0" ]; then \
	    exit 1; \
	  fi; \
	fi

check-pipeline: check
check-all: check-pipeline check-fancy check-features check-bootstrap

check-features: rsc.exe
	@host="$${HOST:-js}"; \
	error=0; \
	for host_path in ./tests/hosts/*.test; do \
		options=`sed -n -e '/;;;options:/p' $$host_path | sed -e 's/^;;;options://'`; \
	    nb_visible_original=`grep -c visible $$host_path`; \
		nb_hidden_original=`grep -c hidden $$host_path`; \
		echo "====================== TESTING FEATURE $$host_path (v:$$nb_visible_original, h:$$nb_hidden_original, options: '$$options')"; \
		cmd="${RSC_COMPILER} -t $$host $$options lib/empty.scm --rvm $$host_path -o .test-feature.REMOVE"; \
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
	    cd host/$$host; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check-bootstrap; cd ../..; \
	    if [ "$$?" != "0" ]; then \
	      exit 1; \
	    fi; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check-bootstrap; cd ../..; \
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

check-es: rsc.exe
	echo "====================== TESTING HOST c"
	HOST=c RSC_COMPILER="./rsc.exe --rvm ./host/c/es.c -f+ c/gc/es" POST_TEST_VALIDATION="***REMAINING_RIBS = 0\n***ALIVE_BUT_COLLECTED = 0" TEST_FEATURES="" make check
	echo "====================== TESTING HOST c (no-global-rank-counter)"
	HOST=c RSC_COMPILER="./rsc.exe --rvm ./host/c/es.c -f+ c/gc/es -f+ no-global-rank-counter" POST_TEST_VALIDATION="***REMAINING_RIBS = 0\n***ALIVE_BUT_COLLECTED = 0" TEST_FEATURES="" make check
	echo "====================== TESTING HOST c (always-adupt)"
	HOST=c RSC_COMPILER="./rsc.exe --rvm ./host/c/es.c -f+ c/gc/es -f+ always-adupt" POST_TEST_VALIDATION="***REMAINING_RIBS = 0\n***ALIVE_BUT_COLLECTED = 0" TEST_FEATURES="" make check
	echo "====================== TESTING HOST c (queue-no-remove)"
	HOST=c RSC_COMPILER="./rsc.exe --rvm ./host/c/es.c -f+ c/gc/es -f+ queue-no-remove" POST_TEST_VALIDATION="***REMAINING_RIBS = 0\n***ALIVE_BUT_COLLECTED = 0" TEST_FEATURES="" make check

venv-gen:
	python3 -m venv venv-gen
	. venv-gen/bin/activate && pip install matplotlib networkx pydot && deactivate

fuzz-es: venv-gen
	. venv-gen/bin/activate && parallel ./fuzz.sh 0 {} ::: 10 10 10 10 25 50 100 250

repl-asm.exe: rsc.exe
	./rsc.exe -t asm -l r4rs lib/r4rs/repl.scm -e optimal -f+ compression/lzss/2b -f+ prim-no-arity -v -x repl-asm.exe

../docs/repl_r4rs.js: rsc.exe host/js/* lib/r4rs/* scripts/minify
	./rsc.exe -t js -f+ js/web -l r4rs -f+ compression/lzss/tag -m -e optimal lib/r4rs/repl.scm -f+ prim-no-arity -o ../docs/repl_r4rs.js

../docs/repl_r4rs_tc.js: rsc.exe host/js/* lib/r4rs/* scripts/minify
	./rsc.exe -t js -f+ js/web -f+ debug/repl -l r4rs/tc -f+ compression/lzss/tag -m -e optimal lib/r4rs/repl.scm -f+ prim-no-arity -o ../docs/repl_r4rs_tc.js

repl_r4rs: ../docs/repl_r4rs.js ../docs/repl_r4rs_tc.js
