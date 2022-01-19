all:

check:
	@echo "*** TESTING hs TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.hs*; \
	  gsi ../../rsc.scm -t hs $$options -o test.hs $$prog; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | runhaskell test.hs > test.hs.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.hs.out; \
	done

clean:
	rm -f test.hs*
