# Ribbit-x86

A fork of the Ribbit Scheme implementation that targets x86 machine code.

<hr>

### Usage

The Ribbit AOT compiler is written in Scheme and can be executed with Gambit, Guile or Chicken. It has been tested with Gambit v4.7.5 and above. For the best experience install Gambit from https://github.com/gambit/gambit .

The original Ribbit AOT compiler's source code is in a single file: `src/rsc.scm` . This Scheme file can be executed as a program with the Gambit, Guile or Chicken interpreters.

The file `src/rsc-x86.scm` contains the main part of the AOT compiler that targets x86 machine code.

Both the original and x86 AOT compilers can be executed using the `src/rsc` shell script, which has the additional `-c` option to select a specific build of the Ribbit AOT compiler which is useful for bootstrapping Ribbit.

Ribbit currently supports the target languages C, JavaScript, Python, Scheme and x86 machine code which are selectable with the compiler's `-t` option with `c`, `js`, `py`, `scm`, and `x86` respectively.  The compacted RVM code can be obtained with the target `rvm` which is the default.

The `-m` option causes a minification of the generated program. This requires a recent version of Gambit.

The `-l` option allows selecting the Scheme runtime library (located in the `lib` subdirectory). The `min` library has the fewest procedures and a REPL that supports the core Scheme forms only. The `max` library has most of the R4RS predefined procedures, except for file I/O. The `max-tc` library is like `max` but with run time type checking. The default is the `max-tc` library.

Here are a few examples:

    Compile a hello world program to JavaScript and then execute it:

      $ echo "(display 'hello) (newline)" > hello.scm
      $ ./rsc -t js -l max hello.scm -o hello.js
      $ node hello.js
      hello

    Compile the REPL to Python and then execute an expression:

      $ ./rsc -t py -l max repl-max.scm -o repl-max.py
      $ echo "(+ 1 (* 2 3))" | python3 repl-max.py
      > 7
      > 

    Compile the REPL to x86 and then execute an expression:

      $ make rsc-x86.bundle.scm
      $ ./rsc -t x86 -l max repl-max.scm -o repl-max.s -c "gsi rsc-x86.bundle.scm"
      $ gcc repl-max.s -o repl-max.exe
      $ echo "(+ 1 (* 2 3))" | ./repl-max.exe
      > 7
      > 
