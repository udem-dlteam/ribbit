# Contributing to Ribbit

Hi ! And welcome to the Ribbit project. If you are reading this, you are
probably interested in contributing to the project. Ribbit is a fun little
project that aims to bring Scheme everywhere while staying as small as
possible. It is a project that is still in its early stages and you may
encounter some rough edges. We are currently working on making those edges less
rough, and we would love some feedback. If you encounter cryptic error messages
or need help on developping a host or a feature, don't hesitate to reach out by
reporting them as a [Github Issue](https://github.com/udem-dlteam/ribbit/issues/new)
or by creating a pull requests with unfinished code.

One of the most accessible ways to contribute to Ribbit is by either adding or
improving a host. A host is a single file that implements a virtual machine
that can run Ribbit's bytecode. We currently support more than 15 hosts, but we
are always looking for more and to improve them. If you are interested in
adding or improving a new host, you can checkout the [adding a host](#adding-a-host)
or [improving a host](#improving-a-host) sections.

## Development Environment

To develop Ribbit, you will need a Scheme compiler or interpreter. We recommend
using [Gambit Scheme](https://gambitscheme.org/), as it is the compiler that we
use to develop Ribbit. You can install it
[here](https://gambitscheme.org/latest/). If you are working on the Ribbit
compiler (`rsc.scm`), you may want to checkout the [Gambit Scheme debugging documentation](https://gambitscheme.org/latest/manual/#Debugging).

Ribbit hasn't been tested on Windows. We recommand using a Unix-like system
such as Linux or OSX to develop Ribbit, or develop inside a virtual machine.
WLS may work, but it hasn't been tested.

Depending on the host that you are developing on, you may need to install
additional dependencies. For example, if you are developing for python, you
will need to have Python 3 installed on your system.

## Project structure

The Ribbit project's source code is located inside the `src` directory. Inside
of it, you can find several directories and files:

`rsc.scm` : The Ribbit Scheme Compiler (RSC). This files contains all the code
for the compiler. It is written in Scheme and can be executed with Gambit
Scheme.

`lib/` : Contains the standard libraries for Ribbit, such as the R4RS library,
min/max libraries, etc. Code here is appended before the source code with the
`-l` compiler option.

`tests/` : Contains all the tests for Ribbit. The tests are written in Scheme
and contains special annotations that are used in the test runner. refer to
[the documentation in the tests folder for more information](./src/tests).

`host/` : Contains all the Ribbit Virtual Machines (RVMs) written in various
languages. Each subfolder inside `host` is named according to the file
extension of the target language and contains all the necessary file to run the
Ribbit Bytecode on the target language. For example, the `host/py` folder
contains the Python RVM under `host/py/rvm.py` and additionnal information such
as how to minify it `host/py/minify` and how to run it `host/py/makefile`.
Specific tests and libraries can be found under the `host/py/tests` and
`host/py/lib` folders respectively.

 `scripts/` : Contains scripts that are used to automate tasks such as running
 tests, minifying files, etc. `examples/` : Contains examples of Ribbit code
 that can be compiled with the Ribbit compiler.

## Adding a new host

Adding a new host can be a fun and rewarding experience. To start, you will
need to create a new folder inside the `host` directory with the name of the
**extension** of the target language, for example `py` for Python or `sh` for
Shell script. We recommand that you copy the content of an existing host that
is similar to the target language. For example, if you want to create a new
host in Ruby, you can copy the content of the `host/py` (Python) folder and
modify it to fit the Ruby language. Remove any unecessary file.

The host folder **must** contain the following files in order to be considered
a valid host:

- `rvm.<extension>` : The Ribbit Virtual Machine (RVM) written in the target
  language. This file is responsible for embedding the Ribbit bytecode.
- `makefile` : The makefile that is used to compile the RVM. This makefile
  should define the `HOST` variable and either the `HOST_INTERPRETER` or
  `HOST_COMPILER` variable. It should also contain the `TEST_TAGS` variable with
  at least the `core` tag. Don't worry too much about this file for the next section,
  it is only needed when testing.
- `tests/` : A directory that contains optional additionnal tests. This can be empty
  for now.

### Papers and documentation

To follow along, it is recommanded to have a look at the papers that describes
Ribbit. It will give you a better understanding and context of what you are
doing :

- Section 2 of [Samuel Yvon and Marc Feeley, A Small Scheme VM, Compiler, and REPL in 4K](http://www.iro.umontreal.ca/~feeley/papers/YvonFeeleyVMIL21.pdf)
- Section 2 and 3 of [An R4RS complient REPL in 7KB](https://arxiv.org/abs/2310.13589)

### Hello World RVM

The first goal is to have a Hello World RVM. This is a single file
implementation that can execute the hello world ribbit bytecode. This
implementation should be only a few hundred lines long and can be done in an
afternoon of hacking. The hello world bytecode is as follows :

```
);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y
```

It is highly encouraged to look at the other implementations, there are more
than 15 of them! The best implementations to checkout are the ones that only
support the `core` feature in [the supported target table](./README.md#Supported-targets)
as they are the simplest.

To follow along, an annotated hello world RVM is available
[here](./src/host/py/hello_rvm.py). This file is ment to be followed alongside
the instructions in this chapiter to have some concrete examples of everythign
that needs to be implemented.

To start off, pick your favorite editor and your target language and start by
editing the `rvm.<extension>` file. The first step in any RVM implementation is
to add a variable that contains the hello world bytecode above inside a string.
This is done in section 0 in the [hello_rvm.py](./src/host/py/hello_rvm.py).
Next, you want to implement :

 1. **Representation of Ribs**
 Ribs are a 3-field structure that is exclusive to Ribbit. They represent
 everything : symbol table, stack, funcions, values, etc. In the Python
 RVM, ribs are represented as a list of size 3. Accesor on the list are built-in features
 of Python, for example, `rib[1]` accesses the second field of the Rib.

 Some hosts, may need to implement a Garbage Collector (GC) to collect ribs.
 If your host needs one, you can look at the [C](./src/host/c/rvm.c) or
 [Assembly](./src/host/asm/rvm.asm) RVMs, they both implement a stop-and-copy
 GC with tagging. For the ones that already have a GC, nothing much has to be
 done in this step.

 2. **VM definitions**
 Next, you need functions to retrieve the bytecode from the input string. In
 the [hello_rvm.py](./src/host/py/hello_rvm.py), these are defined as
 `get_byte`, `get_code` and `get_int`. 

 You also need the stack logic : `push`, `pop`. Note that the stack needs to be
 implemented with ribs, as a linked list chained on the second field of the
 ribs. This makes the stack transparent to the Ribbit code running. It also
 simplifies memory management when using a GC.

 Finally, you need the `TRUE`, `FALSE` and `NIL` constants ribs. They need
 to be tagged as special value ribs with a 5 in the last field (tag field).

 3. **VM Primitives**
 In the primitive section, you need to implement all the basic primitives. You
 can find the list of primitives in the
 [hello_rvm.py](./src/host/py/hello_rvm.py) or in the [frist paper](#Papers-and-documentation),
 Figure 4. Here are some notes on them:
  - ##id : It is equivalent of doing nothing (pop and push the value back)
  - ##arg1 : Equivalent to pop.
  - ##close : Create a closure from a rib procedure, used by `lambda`.
  - ##rib?/##eqv? : You must return the `TRUE` and `FALSE` ribs, and not the
    host-specific true/false values.
  - ##eqv? : Special values (such as `TRUE`, `FALSE` and `NIL`) should return
    false if compared agains each other. It is often implemented using definitionnal equality
    (pointer equality). In python, the `is` operator is used when ribs are compared.
  - ##< / ##+ / ##- / ##* : Arithmetic operations, operate on numbers. It is undefined 
    behaviour if the arguments are ribs.
  - ##quotient : This is the trucated division, not to be confused with the
    floored division. Negative values are rounded towards zero and positive are
    rounded down.
  - ##getchar : Returns `-1` when eof is reached.
  - ##putchar : You may need to flush the output buffer depending on the host.
  - ##exit : exits the program
  
  Note that the order of the primitives are important. They are used to index
  the primitive table in the Ribbit bytecode. The order is the same as in the
  [hello_rvm.py](./src/host/py/hello_rvm.py) file or in the [first paper](#Papers-and-documentation).

  4. **Symbol table**

  The symbol table is a linked list of ribs. It is constructed from the first half of
  the bytecode. 

  


 - Next, you want to implement some functions around the input string to
   retreive the bytecode. Look at the
 - A function that runs the bytecode
 - A function that prints the bytecode



Don't try to make too much sense of it, its raw bytecode. The first step is to
create a string in the target language that contains this bytecode. You can
look at the [Python](./src/host/py/bare_rvm.py) or [Go](./src/host/go/bare_rvm.go)
bare minimum RVM for inspiration.

Next, you want to create




### Compiling other scheme programs

First, you need to add an annotation to help the Ribbit compiler to know where to embed
the bytecode. These annotations live inside the RVM source code and are contained inside
`@@(` and `)@@`.


### Running the tests

If you haven't already, you need to have the `makefile`





## Improving a host

Ribbit is always looking for new hosts to run its bytecode. Hosts can

The Ribbit Scheme Compiler (RSC) is the core of the Ribbit project. It is
written in Scheme and can be executed with Gambit Scheme. The compiler is
responsible for taking a Ribbit source file and compiling it to a target
language.




