all:

check:
	@echo "*** TESTING idr TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -rf test.idr*; \
	  gsi ../../rsc.scm -t idr $$options -o test.idr $$prog; \
	  idris2 -o test.idr.exe --output-dir . test.idr; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | ./test.idr.exe > test.idr.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.idr.out; \
	done

clean:
	rm -f test.idr.exe 
	rm -rf test.idr.exe_app
