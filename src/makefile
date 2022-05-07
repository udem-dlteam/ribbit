HOSTS = c js py scm hs lua scala ml sh

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
	@if [ "$$HOST" == "" ]; then \
	  for host in $(HOSTS); do \
	    echo "====================== TESTING HOST $$host"; \
	    cd host/$$host; $(MAKE) check; cd ../..; \
	  done; \
	else \
	  cd host/$$HOST; $(MAKE) check; cd ../..; \
	fi

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
