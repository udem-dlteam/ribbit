all:

check:
	@echo "*** TESTING idr TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.idr*; \
	  gsi ../../rsc.scm -t idr $$options -o test.idr $$prog; \
	  idris -o test.idr.exe test.idr; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./test.idr.exe > test.idr.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.idr.out; \
	done

clean:
	rm -f test.idr* test.ibc
