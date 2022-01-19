all:

check:
	@echo "*** TESTING rb TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.rb*; \
	  gsi ../../rsc.scm -t rb $$options -o test.rb $$prog; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ruby test.rb > test.rb.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.rb.out; \
	done

clean:
	rm -f test.rb*
