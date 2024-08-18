# Ribbit

A portable, compact and extensible Scheme implementation that is **fully R4RS complient**.
This includes closures, I/O, tail calls, first-class continuations and a Read Eval Print Loop (REPL).

 - **Compact**. Ribbit compresses the code and *only* includes the features you need. [Read about our R4RS complient REPL in 7KB](https://arxiv.org/abs/2310.13589)
 - **Portable**. Ribbit can run on *almost* anything : *JavaScript*, *Assembly (x86)*, *C*, *Python*, *POSIX Shell* and more. [See all Targets](#supported-targets).
 - **Extensible**. Ribbit let's you add new primitives easily and tailor the RVM to your needs. [Read about our markup system.](http://www.iro.umontreal.ca/~feeley/papers/OLearyFeeleyMOREVMS23.pdf)

For more information about Ribbit, you can look at our papers in the [paper section](#research-and-papers).

## Try it now !

You can [try the R4RS complient repl with types checks here](https://udem-dlteam.github.io/ribbit/repl-min.html) or the version [without types here](https://udem-dlteam.github.io/ribbit/repl-max.html).

## Development

Ribbit is a research project currently **under development**. A lot of enhancements have been made since the last release
(R4RS complience, I/O primitives, define-primitive/define-feature, etc.) and a new release is planned for the end of 2023.
If you do enconter bugs, please report them in the issue section of Github.

If you are interested in contributing, you can look at the [roadmap](#roadmap)
or reach out to @leo-ard or @feeley.

### Usage

Currently, Ribbit has only been tested with Gambit v4.7.5, and may not work with other Scheme implementations.
We are currently [working on this](#roadmap) for the release of Ribbit 2.0.

The Ribbit AOT compiler is written in Scheme and can be executed with Gambit
v4.7.5. For the best experience install Gambit from https://github.com/gambit/gambit.
The compiler's source code is in a single file: `src/rsc.scm`.

Ribbit currently supports the target among C, JavaScript, Python,
Haskell, Assembly (x86) which are selectable with the compiler's `-t`
option with `c`, `js`, `py`, `hs` and `asm` respectively.
See the [Supported targets](#supported-targets) table for all the targets.

The `-m` option causes a minification of the generated program. This requires a
recent version of Gambit.

The `-l` option allows selecting the Scheme runtime library (located in the
`lib` subdirectory). Here are a list of libraries :
 - `r4rs` : Adds all essential R4RS procedures. Includes a REPL that is fully r4rs compliant.
 - `r4rs-tc` : Like `r4rs` but with run time type checking.
 - `min`, `max` : Minimal library for small scheme implementations, incliding a REPL.
 - `min-tc`, `max-tc` : Like `min` and `max` but with run time type checking.
 - `define-macro` : Necessary for using the `define-macro` construct.

To compile an executable of the Ribbit Scheme Compiler (rsc.exe) with Gambit, you can use :

```
make rsc.exe
```

Here are a few examples (all assume that a `cd src` has been done first):

    Use RSC to compile an R4RS complient REPL to Python:

      $ ./rsc.exe -t py -l r4rs lib/r4rs/repl.scm -o repl.py
      $ python3 repl.py
      > (+ 1 2)
      3
      > (define handle (open-output-file "test.txt"))
      0
      > (display "Hello Ribbit!" handle)
      0
      > ^D (Ctrl-D)
      $ cat test.txt
      Hello Ribbit!

    Do the same but generating a JavaScript R4RS repl:

      $ ./rsc.exe -t js -l r4rs lib/r4rs/repl.scm -o repl.js
      $ node repl.js
      > (+ 1 2)
      3

    Try it with different hosts :

      $ ./rsc.exe -t asm -l r4rs lib/r4rs/repl.scm -o repl.s (x86 assembly, need linux as it generates an ELF file)
      $ ./rsc.exe -t c -l r4rs lib/r4rs/repl.scm -o repl.c
      $ ./rsc.exe -t hs -l r4rs lib/r4rs/repl.scm -o repl.hs

    Generate the world's smalest R4RS complient repl:

      $ make repl-asm.exe
      $ ls -la repl-asm.exe (6.5KB)
      -rwxr-xr-x  1 leonard  staff  6639  5 Aug 13:36 repl.exe
      $ echo '(+ 1 2)' | ./repl-asm.exe
      > 3
      >

    Interact with the host language (js and C here):

      $ cat examples/square.scm
      (cond-expand ((host py) ;; Python host
                    (define-primitive (square x)
                      "lambda: push(pop()**2),"))
                   ((host c) ;; C host
                    (define-primitive (square x)
                      "{
                        int x = NUM(pop());
                        push2(TAG_NUM(x*x), PAIR_TAG);
                       }")))

      (##putchar (square 8)) ;; prints '@' as 64 is the ASCII value of '8'
      (##putchar 10) ;; prints a newline

      $ ./rsc.exe -t py examples/square.scm -o square.py
      $ python3 square.py
      @

    For other examples and tests, you can look at the `examples` and `tests` directories.

The makefile in the `src` directory has these make targets:

      $ make check                     # check proper operation for all hosts

      $ HOST=c make check              # check proper operation for specific host

      $ HOST=py PY_HOST_INTERPRETER=pypy make check  # check proper operation for specific host and interpreter

      $ make check-bootstrap           # check that rsc bootstraps

      $ HOST=py make check-bootstrap   # check that rsc bootstraps with specific host

      $ HOST=sh SH_HOST_INTERPRETER=ksh make check-bootstrap # check that rsc bootstraps with specific host and interpreter

      $ make prebuilt                  # creates prebuilt artifacts for all hosts





## Supported targets

Here :
 - `core` means a traditionnal RVM implementation. These support minimal I/O (putchar, getchar only), and min/max repls.
 - `variadics` means that the target supports variadic functions (arity-check and rest-params features).
 - `I/O` means that the target supports the full I/O primitives defined by r4rs (open-input-file, open-output-file, etc.).
 - `r4rs` means that the target supports the full r4rs `essential` standard. This relies on all above features.

| Language             | Core | variadics | I/O  | R4RS |
|----------------------|------|-----------|------|------|
| Python (`py`)        | ✅   |   ✅      |  ✅  |  ✅  |
| JavaScript (`js`)    | ✅   |   ✅      |  ✅  |  ✅  |
| C (`c`)              | ✅   |   ✅      |  ✅  |  ✅  |
| x86 Assembly (`asm`) | ✅   |   ✅      |  ✅  |  ✅  |
| Posix-Shell (`sh`)   | ✅   |   ❌      |  ❌  |  ❌  |
| Haskell (`hs`)       | ✅   |   ❌      |  ❌  |  ❌  |
| Scheme (`scm`)       | ✅   |   ❌      |  ❌  |  ❌  |
| Go (`go`)            | ❌   |   ❌      |  ❌  |  ❌  |
| Prolog (`pro`)       | 🚧   |   ❌      |  ❌  |  ❌  |
| Ruby (`rb`)          | 🚧   |   ❌      |  ❌  |  ❌  |
| Lua (`lua`)          | 🚧   |   ❌      |  ❌  |  ❌  |
| Scala (`scala`)      | 🚧   |   ❌      |  ❌  |  ❌  |
| Java (`java`)        | ❌   |   ❌      |  ❌  |  ❌  |
| Rust (`rs`)          | ❌   |   ❌      |  ❌  |  ❌  |

## Research and papers

We are actively developing Ribbit. If you have an idea, you can reach out to @leo-ard or @feeley.

- [Leonard Oest O'Leary and Marc Feeley, A Compact and Extensible Portable Scheme VM. In MoreVMs Workshop (MOREVMS@PROGRAMMING'23), March 2023](http://www.iro.umontreal.ca/~feeley/papers/OLearyFeeleyMOREVMS23.pdf)
- [Samuel Yvon and Marc Feeley, A Small Scheme VM, Compiler, and REPL in 4K. In Workshop on Virtual Machines and Intermediate Languages (VMIL@SPLASH'21), October 2021.](http://www.iro.umontreal.ca/~feeley/papers/YvonFeeleyVMIL21.pdf)
