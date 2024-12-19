# Ribbit :frog:

A portable, compact and extensible Scheme implementation that is **fully R4RS compliant**.
This includes closures, I/O, tail calls, first-class continuations and a Read Eval Print Loop (REPL).

 - **Very Compact**. Ribbit **removes unused code** and performs a specialized compression according to the
    source code. [Read about our Scheme interpreter implementation (REPL) inside 7KB](https://arxiv.org/abs/2310.13589)
 - **Portable**. Ribbit currently runs on **16 different hosts**, including : *JavaScript*, *Assembly (x86)*,
  *C*, *Python*, *POSIX Shell*, *Prolog* and more. [See all targets](#supported-targets).
 - **Extensible**. Ribbit can easily define new **primitives that interact with any of the 16 host languages**.
   [Read about our markup system.](http://www.iro.umontreal.ca/~feeley/papers/OLearyFeeleyMOREVMS23.pdf)

For more information about Ribbit, you can look at our papers in the [paper section](#research-and-papers) or try the R4RS repl below !

<p align="center" font-size="2em">
 <h3 align="center"><a href="https://udem-dlteam.github.io/ribbit/repl-r4rs-tc.html">🐸 Try the R4RS REPL here 🐸</a></h3>
</p>

## Development

Ribbit is a research project currently **under development**. A lot of enhancements have been made since the first paper
(R4RS compliance, I/O primitives, define-primitive/define-feature, bignum/flonums etc.) and a new release is planned for the end of 2024.
If you do encounter bugs, please report them in the issue section of Github.

Like all systems, Ribbit has some quirks. If you encounter problems that are not documented or if you have any questions, please
reach out in the issues section of Github.

<!-- If you are interested in contributing, you can look at [how to contribute](#how-to-contribute) -->

## Usage

Currently, Ribbit has only been tested with Gambit v4.7.5, and may not work with other
Scheme implementations.

The Ribbit AOT compiler is written in Scheme and can be executed with [Gambit v4.7.5](https://github.com/gambit/gambit).
The compiler's source code is in a single file: `src/rsc.scm`.

Ribbit currently compiles Scheme code to **more than 15 different host languages**. To select
a language, use the `-t` compiler option followed by the extension of the **target** language. For example, the option
can be followed by `js` (JavaScript), `asm` (x86 Assembly), `c` (C), `py` (Python), `hs` (Haskell), `pro` (Prolog) 
or [any of the supported targets](#supported-targets).

The `-m` option causes a minification of the generated program. This requires a
recent version of Gambit and may need aditionnal dependencies depending on the target under minification.
See the `host/<host>/minify` script of a specific host for details.

The `-l` option allows selecting the Scheme runtime library (located in the
`lib` subdirectory). Here is a list of libraries :
 - `r4rs` : Adds all essential R4RS procedures. Includes a REPL that is fully R4RS compliant.
 - `r4rs/tc` : Like `r4rs` but with run time type checking.
 - `min`, `max` : Minimal library for small scheme implementations, including a minimal REPL.
 - `max-tc` : Like `min` and `max` but with run time type checking.
 - `define-macro` : Necessary for using the `define-macro` construct.

To compile an executable of the Ribbit Scheme Compiler (rsc.exe) with Gambit, you can use :

```
cd src
make rsc.exe
```

## Usage Examples

Here are a few examples, all assume that a `cd src` and `make rsc.exe` has been done first :

### Use RSC to compile an R4RS compliant REPL to Python

```
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
```

Do the same but generating a JavaScript R4RS REPL:

```
$ ./rsc.exe -t js -l r4rs lib/r4rs/repl.scm -o repl.js
$ node repl.js
> (+ 1 2)
3
```

Try it with different hosts (make sure they support R4RS in the [supported target list](#supported-targets)) :

```
$ ./rsc.exe -t asm -l r4rs lib/r4rs/repl.scm -o repl.s (x86 assembly, need linux as it generates an ELF file)
$ ./rsc.exe -t c -l r4rs lib/r4rs/repl.scm -o repl.c
$ ./rsc.exe -t hs -l r4rs lib/r4rs/repl.scm -o repl.hs
```

### Generate the world's smallest R4RS compliant REPL (takes 1 minute)

```
$ make repl-asm.exe
$ ls -la repl-asm.exe
-rwxr-xr-x  1 leonard  staff  6639  5 Aug 13:36 repl.exe
!!! 6.5KB !!!
$ echo '(+ 1 2)' | ./repl-asm.exe
> 3
>
```

### Generate a simple 'hello world' program in 16 different languages

```
$ echo '(display "Hello from Ribbit!")' > hello.scm
$ ./rsc.exe -t pro -l max hello.scm -o hello.pro # compile it with prolog
$ swipl hello.pro # run it with swi-prolog
Hello from Ribbit!
```

Then, choose among 16 host languages :

```
$ ./rsc.exe -t asm   -l max hello.scm -o hello.asm
$ ./rsc.exe -t c     -l max hello.scm -o hello.c
$ ./rsc.exe -t hs    -l max hello.scm -o hello.hs
$ ./rsc.exe -t js    -l max hello.scm -o hello.js
$ ./rsc.exe -t py    -l max hello.scm -o hello.py
$ ./rsc.exe -t clj   -l max hello.scm -o hello.clj
$ ./rsc.exe -t lisp  -l max hello.scm -o hello.lisp
$ ./rsc.exe -t pro   -l max hello.scm -o hello.pro
$ ./rsc.exe -t scm   -l max hello.scm -o hello.scm
$ ./rsc.exe -t sh    -l max hello.scm -o hello.sh
$ ./rsc.exe -t go    -l max hello.scm -o hello.go
$ ./rsc.exe -t lua   -l max hello.scm -o hello.lua
$ ./rsc.exe -t ml    -l max hello.scm -o hello.ml
$ ./rsc.exe -t idr   -l max hello.scm -o hello.idr
$ ./rsc.exe -t scala -l max hello.scm -o hello.scala
$ ./rsc.exe -t zig   -l max hello.scm -o hello.zig
```

### Interact with the host language (js and C here):

```
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

(##putchar (square 8)) ;; prints '@' as 64 is the ASCII value of '@'
(##putchar 10) ;; prints a newline

$ ./rsc.exe -t py examples/square.scm -o square.py
$ python3 square.py
@
$ ./rsc.exe -t c examples/square.scm -o square.c
$ gcc square.c -o square
$ ./square
@
```

### Generate a simple typed-checked max REPL in any of the hosts
Note that the incremental compiler used by the repl-max.scm only supports a subset
of the Scheme special forms. In particular procedure definitions should use
`(define f (lambda (x) ...))` instead of `(define (f x) ...)`.

```
$ ./rsc.exe -t pro -l max-tc examples/repl-max.scm -o repl-max.pro
$ swipl repl-max.pro
> (+ 1 2)
3
^D
```

Choose any language that supports the core features in the supported targets table and compile it by
replacing `pro` with the target language.

### Other examples and tests

For other examples and tests, you can look at the [examples](./src/examples) and [tests](./tests)
directories.

The makefile in the `src` directory has these make targets:

      $ make check                     # Run all tests for all hosts (very long)

      $ HOST=c make check              # Run tests for the C host

      $ HOST=py PY_HOST_INTERPRETER=pypy make check  # Run tests for specific host and interpreter


## Supported targets

Here :
 - `core` means a traditional RVM implementation. These support minimal I/O (putchar, getchar only), and min/max/max-tc repls.
 - `variadics` means that the target supports functions with any numbers of parameters, for example, the `(define (f . rest) ...)` form.
 - `I/O` means that the target supports the full I/O primitives defined by R4RS (open-input-file, open-output-file, etc.).
 - `r4rs` means that the target supports the full R4RS `essential` standard. This relies on all the above features.

| Language             | Core | variadics | I/O  | R4RS |
|----------------------|------|-----------|------|------|
| x86 Assembly (`asm`) | ✅   |   ✅      |  ✅  |  ✅  |
| C (`c`)              | ✅   |   ✅      |  ✅  |  ✅  |
| Haskell (`hs`)       | ✅   |   ✅      |  ✅  |  ✅  |
| JavaScript (`js`)    | ✅   |   ✅      |  ✅  |  ✅  |
| Python (`py`)        | ✅   |   ✅      |  ✅  |  ✅  |
| Clojure (`clj`)      | ✅   |   ✅      |  ❌  |  ❌  |
| Lisp (`lisp`)        | ✅   |   ✅      |  ❌  |  ❌  |
| Prolog (`pro`)       | ✅   |   ✅      |  ❌  |  ❌  |
| Scheme (`scm`)       | ✅   |   ✅      |  ❌  |  ❌  |
| Posix-Shell (`sh`)   | ✅   |   ✅      |  ❌  |  ❌  |
| Go (`go`)            | ✅   |   ❌      |  ❌  |  ❌  |
| Lua (`lua`)          | ✅   |   ❌      |  ❌  |  ❌  |
| OCaml (`ml`)         | ✅   |   ❌      |  ❌  |  ❌  |
| Idris 2 (`idr`)      | ✅   |   ❌      |  ❌  |  ❌  |
| Scala (`scala`)      | ✅   |   ❌      |  ❌  |  ❌  |
| Zig (`zig`)          | ✅   |   ❌      |  ❌  |  ❌  |
| Ruby (`rb`)          | 🚧   |   ❌      |  ❌  |  ❌  |
| Java (`java`)        | 🚧   |   ❌      |  ❌  |  ❌  |
| Rust (`rs`)          | 🚧   |   ❌      |  ❌  |  ❌  |

<!--
## How to contribute

If you love Scheme and want to add a new host, improve a host, add a new library
or fix a bug, don't hesitate to contribute with pull requests. More information about the
development of Ribbit can be found in the [CONTRIBUTING.md](./CONTRIBUTING.md) file.
-->

## Research and Papers

We are actively developing Ribbit. If you have an idea, you can reach out to [leo-ard](https://github.com/leo-ard) or [feeley](https://www.iro.umontreal.ca/~feeley/).
All papers concerning Ribbit are available here :

- [Leonard Oest O'Leary, Mathis Laroche and Marc Feeley, An R4RS compliant REPL in 7KB in SCHEME Workshop @ ICPF'23, January 2024](https://arxiv.org/abs/2310.13589)
- [Leonard Oest O'Leary and Marc Feeley, A Compact and Extensible Portable Scheme VM. In MoreVMs Workshop (MOREVMS@PROGRAMMING'23), March 2023](http://www.iro.umontreal.ca/~feeley/papers/OLearyFeeleyMOREVMS23.pdf)
- [Samuel Yvon and Marc Feeley, A Small Scheme VM, Compiler, and REPL in 4K. In Workshop on Virtual Machines and Intermediate Languages (VMIL@SPLASH'21), October 2021.](http://www.iro.umontreal.ca/~feeley/papers/YvonFeeleyVMIL21.pdf)

**If you want to cite our work, [all BibTeX entries are available in the CITATION.bib file](./CITATION.md).**
