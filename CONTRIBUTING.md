# Contributing to Ribbit

Hi and welcome to the Ribbit project. Ribbit is a fun little project that aims to bring Scheme everywhere while staying as small as possible. If you want to be part of the journey by contributing, this guide will get you up to speed. During the process, don't hesitate to reach out with the [Github Issue](https://github.com/udem-dlteam/ribbit/issues/new) section of github if you encounter bugs or have questions.

One of the most accessible ways to contribute to Ribbit is by adding or
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

Adding a new host is a fun and rewarding experience. To start, you will
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
implementation should be only a few hundred lines long. The hello world bytecode is as follows :

```
);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y
```

It is highly encouraged to look at the other implementations, there are more
than 15 of them! The best implementations to checkout are the ones that only
support the `core` feature in [the supported target table](./README.md#Supported-targets)
as they are the simplest.

To follow along, an annotated hello world RVM is available
[here](./src/host/py/hello.py). This file is ment to be followed alongside
the instructions in this chapiter to have some concrete examples of everything
that needs to be implemented.

To start off, pick your favorite editor and your target language and start by
editing the `rvm.<extension>` file. The first step in any RVM implementation is
to add a variable that contains the hello world bytecode above inside a string.
This is done in section 0 in the [hello.py](./src/host/py/hello.py).
Next, you want to implement :

#### 1. **Representation of Ribs**

Ribs are a 3-field structure that is exclusive to Ribbit. They represent
everything : symbol table, stack, functions, values, etc. In the Python
RVM, ribs are represented as a list of size 3. Access to the list are built-in features
of Python, for example, `rib[1]` accesses the second field of the Rib.
This may not be the case for your language, and you may need to create functions/wrappers
to access the values inside the ribs.

Some hosts, may need to implement a Garbage Collector (GC) to collect ribs.
Skip ahead if your language don't need it.
If your host needs one, you can look at the [C](./src/host/c/rvm.c) or
[Assembly](./src/host/asm/rvm.asm) RVMs, they both implement a stop-and-copy
GC with tagging. Even if the RVM needs a garbage collector, it is recommended
do this step later on, for now you can simply allocate a huge range of memory
and use this one for the allocations of the ribs.

#### 2. **VM definitions**
Next, you need functions to retrieve the bytecode from the input string. In
the [hello_rvm.py](./src/host/py/hello.py), these are defined as
`get_byte`, `get_code` and `get_int`.

You also need the stack logic : `push`, `pop`. Note that the stack needs to be
implemented with ribs, as a linked list chained on the second field of the
ribs. This makes the stack transparent to the Ribbit code running. It also
simplifies memory management when using a GC.

Finally, you need the `TRUE`, `FALSE` and `NIL` constants ribs. They need
to be tagged as special value ribs with a `5` in the last field (tag field).

More details are available inside the reference implementation.

#### 3. **VM Primitives**
In the primitive section, you need to implement all the basic primitives. You
can find the list of primitives in the
[hello.py](./src/host/py/hello.py) or in the [frist paper](#Papers-and-documentation),
Figure 4.

As Ribbit is a stack machine, all primitives **act on the stack**. For example,
the `##rib` primitive will take the three values on the stack and push back a rib
containing these 3 values. All primitives must push one (and only one) value to the
stack before ending, using the `push` and `pop` function defined above.

Section 3 of the reference implementation gives a good idea of how to implement them.
Here are some "good to know" about each primitive to implement them correctly:
 - ##id : It is equivalent of doing nothing (pop and push the value back).
 - ##arg1 : Equivalent to pop.
 - ##close : Create a closure from a rib procedure, used by `lambda`. A closure
   is rib with [continuation, stack, 1] where the continuation are the instructions
   to run and stack is the stack. More details in the paper.
 - ##rib?/##eqv? : The `TRUE` and `FALSE` ribs, must be returned, and not the host-specific
   true/false values.
 - ##eqv? : Special values (such as `TRUE`, `FALSE` and `NIL`) should return
   `FALSE` if compared against each other. It is often implemented using definitionnal equality
   (pointer equality). In python, the `is` operator is used when ribs are compared.
 - ##< / ##+ / ##- / ##* : Arithmetic operations, operate on numbers. It is an undefined
   behavior if the arguments are ribs, in other words, always expects the arguments to be
   numbers.
 - ##quotient : This is the trucated division, not to be confused with the
   floored division. Negative values are rounded towards zero and positive are
   rounded down.
 - ##getchar : Returns `-1` when eof is reached.
 - ##putchar : You may need to flush the output buffer depending on the host.
 - ##exit : exits the program

Note that the order of the primitives are important. They are used to index
the primitive table in the Ribbit bytecode. The order is the same as in the
[hello.py](./src/host/py/hello.py) file or in the [first paper](#Papers-and-documentation).

####  4. **Symbol table**

The first half of the bytecode contains the symbol table, ending with a `;`. The first code
of the encoded symbol table represents the number of empty symbols to add. Following it is all the
symbols in reverse order, with the name of the symbols in reverse order as well.

The bytecode printing `HELLO!` only contains empty symbols:

```
 End of the symbol table
 |
 v
);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y
^
|
|
|
Number of empty symbols, here `)` amounts to 6 (see get_code function)
```

Here is an example with the symbols `##rib` and `##putchar` encoded. Here *First* and *last* symbols
refer to the order in which they are decoded, not the order in the resulting symbol table:

```
 First symbol `##id`
 |
 |    Second symbol `##putchar`
 |    |                        End of the symbol table
 |    |                        |
 v    v                        v
#di##,rahctup##,1gra##,,,,bir##;(vD>?vRD>?vRA>?vRA>?vR:>?vR=>k!(:lkv6!):lkl!':lkmy
^   ^                     ^
|   |                     |
|   |                     Last symbol `##rib`
|   End of the first symbol
|
Number of empty symbols, here `#` amounts to 1 (see get_code function)
```


#### 5. Decoding of the Code Graph

**TODO**


#### 6. Execution of the RVM instructions


**TODO**


<!--
### Compiling other scheme programs

First, you need to add an annotation to help the Ribbit compiler to know where to embed
the bytecode. These annotations live inside the RVM source code and are contained inside
`@@(` and `)@@`.
-->


### Running the tests

To run the tests, you can run the following in the `src` repository of Ribbit:

```
HOST=<your host> make check
```

<!--
## Improving a host

Ribbit is always looking for new hosts to run its bytecode. Hosts can

The Ribbit Scheme Compiler (RSC) is the core of the Ribbit project. It is
written in Scheme and can be executed with Gambit Scheme. The compiler is
responsible for taking a Ribbit source file and compiling it to a target
language.
-->




