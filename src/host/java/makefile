all:

check:
	@echo "*** TESTING java TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.java*; \
	  gsi ../../rsc.scm -t java $$options -o test.java $$prog; \
	  javac test.java; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | java test > test.java.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.java.out; \
	done

clean:
	rm -f test.java* test*.class
