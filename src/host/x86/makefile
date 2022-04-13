all:

check:
	@echo "*** TESTING x86 TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.s*; \
	  ../../rsc -t x86 $$options -o test.s -c "gsi ../../rsc-x86.bundle.scm" $$prog; \
	  gcc -o test.s.exe test.s; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./test.s.exe > test.s.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.s.out; \
	done

clean:
	rm -f test.s*
