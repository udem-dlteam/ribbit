# These settings will bootstrap the x86 AOT compiler using Gambit:
TARGET=x86
EXT=.s
LIB=max
RSC=rsc-x86

# To bootstrap the original AOT compiler with the C host uncomment these lines:
#TARGET=c
#EXT=.c
#LIB=max
#RSC=rsc

all: test1 test2

$(RSC).bundle.scm: shims.scm asm.scm x86.scm rts.scm $(RSC).scm
	sed -e 's/^#!/;;#!/g' $^ > $@

# rule to build $(RSC).bootstrap1.exe using $(RSC).scm run with Gambit
$(RSC).bootstrap1.exe: $(RSC).bundle.scm
	time ./rsc -t $(TARGET) -l $(LIB) -m -c "gsi -:d4 $(RSC).bundle.scm" -o $@$(EXT) $<
	gcc -o $@ $@$(EXT)

# rule to build $(RSC).bootstrap2.exe using $(RSC).bootstrap1.exe
$(RSC).bootstrap2.exe: $(RSC).bootstrap1.exe $(RSC).bundle.scm
	time ./rsc -t $(TARGET) -l $(LIB) -m -c ./$< -o $@$(EXT) $(RSC).bundle.scm
	gcc -o $@ $@$(EXT)

test1: $(RSC).bundle.scm
	time ./rsc -t $(TARGET) -l empty -m -c "gsi -:d4 $<" -o test1.exe$(EXT) test1.scm
	gcc -o test1.exe test1.exe$(EXT)
	echo "ok" | ./test1.exe
	@echo "exit code = $$?"
#	rm test1.exe$(EXT) test1.exe

test2: $(RSC).bundle.scm
	time ./rsc -t $(TARGET) -l $(LIB) -m -c "gsi -:d4 $<" -o test2.exe$(EXT) test2.scm
	gcc -o test2.exe test2.exe$(EXT)
	echo "(this '(is a list) containing 5 \"elements\")" | ./test2.exe
#	rm test2.exe$(EXT) test2.exe

test-bootstrap1: $(RSC).bootstrap1.exe
	time ./rsc -t $(TARGET) -l empty -m -c ./$< -o test1-bootstrap1.exe$(EXT) test1.scm
	gcc -o test1-bootstrap1.exe test1-bootstrap1.exe$(EXT)
	echo "ok" | ./test1-bootstrap1.exe
#	rm test1-bootstrap1.exe$(EXT) test1-bootstrap1.exe
	time ./rsc -t $(TARGET) -l $(LIB) -m -c ./$< -o test2-bootstrap1.exe$(EXT) test2.scm
	gcc -o test2-bootstrap1.exe test2-bootstrap1.exe$(EXT)
	echo "(this '(is a list) containing 5 \"elements\")" | ./test2-bootstrap1.exe
#	rm test2-bootstrap1$.exe(EXT) test2-bootstrap1.exe

test-bootstrap2: $(RSC).bootstrap2.exe
	time ./rsc -t $(TARGET) -l empty -m -c ./$< -o test1-bootstrap2.exe$(EXT) test1.scm
	gcc -o test1-bootstrap2.exe test1-bootstrap2.exe$(EXT)
	echo "ok" | ./test1-bootstrap2.exe
#	rm test1-bootstrap2.exe$(EXT) test1-bootstrap2.exe
	time ./rsc -t $(TARGET) -l $(LIB) -m -c ./$< -o test2-bootstrap2.exe$(EXT) test2.scm
	gcc -o test2-bootstrap2.exe test2-bootstrap2.exe$(EXT)
	echo "(this '(is a list) containing 5 \"elements\")" | ./test2-bootstrap2.exe
#	rm test2-bootstrap2$.exe(EXT) test2-bootstrap2.exe

repl-min: $(RSC).bundle.scm
	time ./rsc -t $(TARGET) -l min -m -c "gsi -:d4 $<" -o repl-min.exe$(EXT) repl-min.scm
	gcc -o repl-min.exe repl-min.exe$(EXT)
	echo "(* 6 7)" | ./repl-min.exe
#	rm repl-min.exe$(EXT) repl-min.exe

repl-max: $(RSC).bundle.scm
	time ./rsc -t $(TARGET) -l max -m -c "gsi -:d4 $<" -o repl-max.exe$(EXT) repl-max.scm
	gcc -o repl-max.exe repl-max.exe$(EXT)
	echo "(* 6 7)" | ./repl-max.exe
#	rm repl-max.exe$(EXT) repl-max.exe

clean:
	rm -f *.bundle.scm *.bootstrap1.* *.bootstrap2.* test*.exe
