all:

check:
	@echo "*** TESTING rs TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.rs*; \
	  gsi ../../rsc.scm -t rs $$options -o test.rs $$prog; \
	  rustc test.rs; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./test > test.rs.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.rs.out; \
	done

clean:
	rm -f test.rs* test
