SHELL=bash

HOSTS = c js py scm hs lua ml sh asm

all:

.PHONY: prebuilt

prebuilt:
	RIBBIT_BUILD_DIR=`pwd`/prebuilt RIBBIT_BUILD_OPTS="-m" $(MAKE) build-all

build-all:
	@for host in $(HOSTS); do \
	  echo "====================== BUILDING HOST $$host"; \
	  cd host/$$host; $(MAKE) build-all; cd ../..; \
	done

check:
	@if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	    cd host/$$host; $(MAKE) check; cd ../..; \
	  done; \
	else \
	  cd host/$$HOST; $(MAKE) check; cd ../..; \
	fi


check-fancy:
	@if [ "$$HOST" = "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	    cd host/$$host; RSC_COMPILER="gsi ../../rsc.scm" $(MAKE) check; cd ../..; \
	  done; \
	else \
	  cd host/$$HOST; RSC_COMPILER="gsi ../../rsc.scm" $(MAKE) check; cd ../..; \
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
		cmd="gsi rsc.scm -t $$host $$options lib/empty.scm --rvm $$host_path -o .test-feature.REMOVE"; \
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

check-bootstrap:
	@host="$${HOST:-js}"; \
	case $$host in \
	   js) host_interpreter="$${JS_HOST_INTERPRETER:-node}";; \
           py) host_interpreter="$${PY_HOST_INTERPRETER:-python3}";; \
          scm) host_interpreter="$${SCM_HOST_INTERPRETER:-gsi}";; \
           hs) host_interpreter="$${HS_HOST_INTERPRETER:-runhaskell}";; \
          lua) host_interpreter="$${LUA_HOST_INTERPRETER:-lua}";; \
        scala) host_interpreter="$${SCALA_HOST_INTERPRETER:-scala}";; \
           ml) host_interpreter="$${ML_HOST_INTERPRETER:-ocaml}";; \
           sh) host_interpreter="$${SH_HOST_INTERPRETER:-ksh}";; \
	   rs) host_interpreter="rustc";; \
	esac; \
	echo ./rsc -t $$host -l max -o rsc-bootstrap1.$$host rsc.scm; \
	time ./rsc -t $$host -l max -o rsc-bootstrap1.$$host rsc.scm; \
	echo ./rsc -t $$host -l max -c \"$$host_interpreter rsc-bootstrap1.$$host\" -o rsc-bootstrap2.$$host rsc.scm; \
	time ./rsc -t $$host -l max -c "$$host_interpreter rsc-bootstrap1.$$host" -o rsc-bootstrap2.$$host rsc.scm; \
	echo ./rsc -t $$host -l max -c \"$$host_interpreter rsc-bootstrap2.$$host\" -o rsc-bootstrap3.$$host rsc.scm; \
	time ./rsc -t $$host -l max -c "$$host_interpreter rsc-bootstrap2.$$host" -o rsc-bootstrap3.$$host rsc.scm; \
	if diff rsc-bootstrap2.$$host rsc-bootstrap3.$$host > rsc-bootstrap.diff; then \
	  echo "====================== SUCCESSFUL BOOTSTRAP USING HOST $$host EXECUTED WITH $$host_interpreter"; \
	  rm -f rsc-bootstrap*; \
	else \
	  echo "====================== ***FAILED*** BOOTSTRAP USING HOST $$host EXECUTED WITH $$host_interpreter"; \
	  exit 1; \
	fi

clean:
	@for host in $(HOSTS); do \
	  echo "====================== CLEANING HOST $$host"; \
	  cd host/$$host; $(MAKE) clean; cd ../..; \
	done
	rm -f rsc-bootstrap*
