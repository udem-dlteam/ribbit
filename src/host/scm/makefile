all:

check:
	@echo "*** TESTING scm TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.scm*; \
	  gsi ../../rsc.scm -t scm $$options -o test.scm $$prog; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | gsi test.scm > test.scm.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.scm.out; \
	done

clean:
	rm -f test.scm*
