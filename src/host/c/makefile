all:

check:
	@echo "*** TESTING c TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.c*; \
	  ../../rsc -t c $$options -o test.c $$prog; \
	  gcc -o test.c.exe test.c; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./test.c.exe > test.c.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.c.out; \
	done

clean:
	rm -f test.c*
