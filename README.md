# Ribbit

A portable, compact and extensible Scheme implementation that is **fully R4RS complient**. 
This includes closures, I/O, tail calls, first-class continuations and a Read Eval Print Loop (REPL). 

 - **Compact**. Ribbit compresses the code and *only* includes the features you need. [Read about our R4RS complient REPL in 7KB](https://arxiv.org/abs/2310.13589)
 - **Portable**. Ribbit can run on *almost* anything : *JavaScript*, *Assembly (x86)*, *C*, *Python*, *POSIX Shell* and more. [See all Targets](#supported-targets).
 - **Extensible**. Ribbit let's you add new primitives easily and tailor the RVM to your needs. [Read about our markup system.](http://www.iro.umontreal.ca/~feeley/papers/OLearyFeeleyMOREVMS23.pdf)

For more information about Ribbit, you can look at our papers in the [paper section](#research-and-papers). 

## Try it now !

You can [try the R4RS complient repl with types checks here](https://udem-dlteam.github.io/ribbit/repl-min.html) or the version [without types here](https://udem-dlteam.github.io/ribbit/repl-max.html). 

### Usage

The Ribbit AOT compiler is written in Scheme and can be executed with Gambit
v4.7.5. For the best experience install Gambit from
https://github.com/gambit/gambit.

<!-- 
There are also prebuilt versions of the Ribbit AOT compiler in the `prebuilt` directory, allowing the AOT compiler to be executed using another language interpreter, such as nodejs, CPython, and even just a POSIX shell.
-->
 
The AOT compiler's source code is in a single file: `src/rsc.scm`. This Scheme
file can be executed as a program with the Gambit, Guile, Chicken or Kawa
interpreters. Alternatively the AOT compiler can be executed using the
`src/rsc` shell script, which has the additional `-c` option to select a
specific build of the Ribbit AOT compiler which is useful for bootstrapping
Ribbit or to execute one of the prebuilt versions.

Ribbit currently supports the target languages C, JavaScript, Python, Scheme,
Haskell, Lua, Assembly (x86), Ocaml and POSIX shell which are selectable with
the compiler's `-t` option with `c`, `js`, `py`, `scm`, `hs`, `lua`, `asm`,
`ml`, and `sh` respectively.  The compacted RVM code can be obtained with the
target `rvm` which is the default.

The `-m` option causes a minification of the generated program. This requires a
recent version of Gambit.

The `-l` option allows selecting the Scheme runtime library (located in the
`lib` subdirectory). The `min` library has the fewest procedures and a REPL
that supports the core Scheme forms only. The `max` library has most of the
R4RS predefined procedures, except for file I/O. The `max-tc` library is like
`max` but with run time type checking. The default is the `max-tc` library.

Here are a few examples (all assume that a `cd src` has been done first):

    Use the Gambit interpreter to compile the minimal REPL to Python
    and execute with python3:

      $ ./rsc -t py -l min repl-min.scm
      $ echo "(define f (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))(f 25)" | python3 repl-min.scm.py
      > 0
      > 75025
      >

    Alternatively one of the prebuilt versions can be used to achieve the
    same result:

      $ ./rsc -t py -l min -c "node prebuilt/rsc.js"       repl-min.scm
      $ ./rsc -t py -l min -c "python3 prebuilt/rsc.py"    repl-min.scm
      $ ./rsc -t py -l min -c "runhaskell prebuilt/rsc.hs" repl-min.scm
      $ ./rsc -t py -l min -c "lua prebuilt/rsc.lua"       repl-min.scm
      $ ./rsc -t py -l min -c "scala prebuilt/rsc.scala"   repl-min.scm
      $ ./rsc -t py -l min -c "ksh prebuilt/rsc.sh"        repl-min.scm
      $ gcc -o rsc.exe prebuilt/rsc.c
      $ ./rsc -t py -l min -c ./rsc.exe                    repl-min.scm

    Do the same but generating a JavaScript program:

      $ ./rsc -t js -l min repl-min.scm

    Use Guile instead of Gambit to compile the REPL with type checking to C
    and then compile RVM with gcc:

      $ RSC_SCHEME_INTERPRETER=guile ./rsc -t c -l max-tc repl-max.scm
      $ gcc repl-max.scm.c
      $ echo "(+ 1 (* 2 3))(car 0)" | ./a.out
      > 7
      > *** type error ()
      >

    You can also use RSC_SCHEME_INTERPRETER=kawa to use Kawa Scheme or
    RSC_SCHEME_INTERPRETER="csi -s" to use Chicken Scheme.

    Use the Gambit compiler to compile the AOT compiler and then use it:

      $ gsc -exe -o rsc-gambit.exe rsc.scm
      $ ./rsc -t c -c ./rsc-gambit.exe repl-max.scm

    Use the Chicken compiler to compile the AOT compiler and then use it:

      $ csc -o rsc-chicken.exe -O2 rsc.scm
      $ ./rsc -t c -c ./rsc-chicken.exe repl-max.scm

    Use Ribbit as a pipeline compiler to compile the trivial program
    `(putchar 65) (putchar 10)` to the corresponding compacted RVM code
    (only 23 bytes of code):

      $ echo "(putchar 65) (putchar 10)" | gsi rsc.scm
      );'u?>vR6!(:lkm!':lkv6y

    Use Ribbit as a pipeline compiler to compile the trivial program
    `(putchar 65) (putchar 10)` to compacted RVM code and combine
    it with the Python implementation of the RVM and execute it with python3:

      $ echo "(putchar 65) (putchar 10)" | ./rsc -l empty -t py | python3
      A

    Compile the Ribbit AOT compiler using itself to get a JavaScript
    version of the compiler and use it to compile a program:

      $ ./rsc -t js -l max rsc.scm   # compile rsc.scm to rsc.scm.js
      $ echo '(display "hello!\n")' > h.scm
      $ ./rsc -t py -l max -c "node rsc.scm.js" h.scm # use bootstrapped compiler
      $ python3 h.scm.py
      hello!

    Bootstrap the Ribbit AOT compiler using a POSIX shell (note
    that with ksh this takes over 5 hours on a fast computer and it
    can take substantially more with other POSIX shells):

      $ ./rsc -t sh -l max -o rsc-bootstrap1.sh rsc.scm
      $ ./rsc -t sh -l max -c "ksh rsc-bootstrap1.sh" -o rsc-bootstrap2.sh rsc.scm
      $ echo '(display "hello!\n")' > h.scm
      $ ./rsc -t sh -l max -m -c "ksh rsc-bootstrap2.sh" h.scm
      $ ksh h.scm.sh
      hello!

The makefile in the `src` directory has these make targets:

      $ make check                     # check proper operation for all hosts

      $ HOST=c make check              # check proper operation for specific host

      $ HOST=py PY_HOST_INTERPRETER=pypy make check  # check proper operation for specific host and interpreter

      $ make check-bootstrap           # check that rsc bootstraps

      $ HOST=py make check-bootstrap   # check that rsc bootstraps with specific host

      $ HOST=sh SH_HOST_INTERPRETER=ksh make check-bootstrap # check that rsc bootstraps with specific host and interpreter

      $ make prebuilt                  # creates prebuilt artifacts for all hosts





## Supported targets

| Host\feature | Supported | Markup complete  | Arity feature  |
|--------------|-----------|------------------|----------------|
| py           |   YES     | missing location |        YES     |
| js           |   YES     |      YES         |        YES     |
| c            |   YES     | missing location |        YES     |
| asm          |   YES     | missing location |        YES     |
| sh           |   YES     | missing location |        YES     |
| scm          |   YES     | missing location |        YES     |
| hs           |   YES     | missing location |                |
| lua          |   YES     | missing location |                |
| ml           |   YES     | missing location |                |
| rs           |   NO      |                  |                |
| go           |   NO      |                  |                |
| idr          |   NO      |                  |                |
| scala        |   NO      |                  |                |
| rb           |   NO      |                  |                |
| java         |   NO      |                  |                |

## Research and papers

We are actively developing Ribbit. If you have an idea, you can reach out to @leo-ard or @feeley.

- [Leonard Oest O'Leary and Marc Feeley, A Compact and Extensible Portable Scheme VM. In MoreVMs Workshop (MOREVMS@PROGRAMMING'23), March 2023](http://www.iro.umontreal.ca/~feeley/papers/OLearyFeeleyMOREVMS23.pdf)
- [Samuel Yvon and Marc Feeley, A Small Scheme VM, Compiler, and REPL in 4K. In Workshop on Virtual Machines and Intermediate Languages (VMIL@SPLASH'21), October 2021.](http://www.iro.umontreal.ca/~feeley/papers/YvonFeeleyVMIL21.pdf)
