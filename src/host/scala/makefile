all:

check:
	@echo "*** TESTING scala TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.scala*; \
	  gsi ../../rsc.scm -t scala $$options -o test.scala $$prog; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | scala test.scala > test.scala.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.scala.out; \
	done

clean:
	rm -f test.scala*
