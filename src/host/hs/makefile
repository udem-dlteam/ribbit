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

# Instead of using runhaskell, we compile the program once and then feed it RVM
# code as input. This is much faster as loading the program with runhaskell
# can take up to 0.5s.
check-fast:
	@echo "*** TESTING hs TARGET ***"; \
	ghc rvm.hs -O2 -with-rtsopts=-N1 -threaded -rtsopts -o runrvm; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.hs*; \
	  gsi ../../rsc.scm -m $$options $$prog > test.rvm; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./runrvm test.rvm > test.hs.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.hs.out; \
	done

check-fast-min: rvm.min.hs
	@echo "*** TESTING hs TARGET ***"; \
	ghc rvm.min.hs -O2 -with-rtsopts=-N1 -threaded -rtsopts -o runrvm; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.hs*; \
	  gsi ../../rsc.scm -m $$options $$prog > test.rvm; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./runrvm test.rvm > test.hs.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.hs.out; \
	done

rvm.min.hs: minify
	cat rvm.hs | ./minify > rvm.min.hs

clean:
	rm -f rvm.min.hs
	rm -f runrvm
	rm -f rvm.hi*
	rm -f rvm.o*
	rm -f test.hs*
	rm -f test.hi*
	rm -f test.o*
	rm -f test.rvm*
