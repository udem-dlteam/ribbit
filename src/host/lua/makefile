LUA=lua
# LUA=lua5.1
# LUA=lua5.3

all:

check:
	echo "*** TESTING lua TARGET ***"; \
	for prog in `ls ../../tests/*.scm tests/*.scm` ; do \
	  options=`sed -n -e '/;;;options:/p' $$prog | sed -e 's/^;;;options://'`; \
	  echo "---------------------- $$prog [options:$$options]"; \
	  rm -f test.lua*; \
	  gsi ../../rsc.scm -t lua $$options -o test.lua $$prog; \
	  sed -n -e '/;;;input:/p' $$prog | sed -e 's/^;;;input://' | $(LUA) test.lua > test.lua.out; \
	  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $$prog | diff - test.lua.out; \
	done

clean:
	rm -f test.lua*
