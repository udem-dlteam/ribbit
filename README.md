# Ribbit

A small and portable Scheme implementation that supports closures, tail calls, first-class continuations, a REPL and AOT and incremental compilers. All that for a run time footprint around 4 KB!

You can [try the REPL with a minimal library here](https://udem-dlteam.github.io/ribbit/repl-min.html) or [try a more featureful version here](https://udem-dlteam.github.io/ribbit/repl-max.html).

Please note that currently the incremental compiler used by the REPL only supports a subset of the Scheme special forms. In particular procedure definitions should use the syntax `(define name (lambda ...))` .

<hr>

### Usage

The Ribbit compiler is written in Scheme and can be executed with Gambit, Guile or Chicken. It has been tested with Gambit v4.7.5 and above. For the best experience install Gambit from https://github.com/gambit/gambit .

Currently Ribbit supports the target languages C, JavaScript, Python and Scheme which are selectable with the compiler's `-t` option with `c`, `js`, `py`, and `scm` respectively.  The compacted RVM code can be obtained with the target `none` which is the default.

The `-m` option causes a minification of the generated program. This requires a recent version of Gambit.

The `-l` option allows selecting the Scheme runtime library (located in the `lib` subdirectory). The `min` library has the fewest procedures and a REPL that supports the core Scheme forms only. The `max` library has most of the R4RS predefined procedures, except for file I/O. The `max-tc` library is like `max` but with run time type checking. The default is the `max-tc` library.

Here are a few examples (all assume that a `cd src` has been done first):

    Use Gambit to compile the minimal REPL to Python
    and execute with python3:

      $ gsi rsc.scm -t py -l min repl-min.scm
      $ echo "(define f (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))(f 25)" | python3 repl-min.scm.py
      > 0
      > 75025
      > 

    Do the same but with JavaScript:

      $ gsi rsc.scm -t js -l min repl-min.scm
      $ echo "(define f (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))(f 25)" | node repl-min.scm.js
      > 0
      > 75025
      > 

    Use Guile to compile the REPL with type checking to C
    and then compile RVM with gcc:

      $ guile -s rsc.scm -t c -l max-tc repl-max.scm
      $ gcc repl-max.scm.c
      $ echo "(+ 1 (* 2 3))(car 0)" | ./a.out
      > 7
      > *** type error
      > 

    Use Chicken to compile the minimal REPL to minified Scheme
    and execute with Gambit:

      $ csi -q rsc.scm -t scm -l min -m repl-min.scm
      $ echo "(define twice (lambda (x) (* x 2)))(twice 21)" | gsi repl-min.scm.scm
      > 0
      > 42
      > 

    Use Ribbit as a pipeline compiler to compile the trivial program
    `(putchar 65) (putchar 10)` to the corresponding compacted RVM code
    (only 23 bytes of code):

      $ echo "(putchar 65) (putchar 10)" | gsi rsc.scm
      );'u?>vR6!(:lkm!':lkv6y

    Use Ribbit as a pipeline compiler to compile the trivial program
    `(putchar 65) (putchar 10)` to compacted RVM code and combine
    it with the Python implementation of the RVM and execute it with python3:

      $ echo "(putchar 65) (putchar 10)" | gsi rsc.scm - -l empty -t py | python3
      A
