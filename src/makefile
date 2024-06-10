HOSTS = c js py scm hs lua ml sh asm clj zig

#RSC_COMPILER ?= gsi -:r4rs rsc.scm
GSI ?= gsi
GSC ?= gsc
RSC_COMPILER ?= ./rsc.exe

all:

.PHONY: prebuilt

check: rsc.exe
	@if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	    cd host/$$host; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check; cd ../..; \
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
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check-repl; cd ../..; \
	fi

check-pipeline: check
check-all: check-pipeline check-fancy check-features check-bootstrap

check-features:
	@host="$${HOST:-js}"; \
	for host_path in ./tests/hosts/*; do \
		options=`sed -n -e '/;;;options:/p' $$host_path | sed -e 's/^;;;options://'`; \
	    nb_visible_original=`grep -c visible $$host_path`; \
		nb_hidden_original=`grep -c hidden $$host_path`; \
		echo "====================== TESTING FEATURE $$host_path (v:$$nb_visible_original, h:$$nb_hidden_original, options: '$$options')"; \
		cmd="$${GSI} rsc.scm -t $$host $$options lib/empty.scm --rvm $$host_path -o .test-feature.REMOVE"; \
		$$cmd; \
		nb_visible_after=`grep -c visible .test-feature.REMOVE;` \
		nb_hidden_after=`grep -c hidden .test-feature.REMOVE`; \
		if ! [[ "$$nb_visible_after" -eq "$$nb_visible_original" && "$$nb_hidden_after" -eq "0" ]] ; then \
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
	done

check-bootstrap: rsc.exe
	@UP_CASE="(##vector-set! ##main-readtable 1 'upcase)"; \
	if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
		cd host/$$host; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check-bootstrap; cd ../..; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="${RSC_COMPILER}" $(MAKE) check-bootstrap; cd ../..; \
	fi



clean:
	@for host in $(HOSTS); do \
	  echo "====================== CLEANING HOST $$host"; \
	  cd host/$$host; $(MAKE) clean; cd ../..; \
	done
	rm -f rsc-bootstrap*
