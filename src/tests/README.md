# Tests are organized in the following categories :

- `00-ribbit` : Tests that are specific to Ribbit.
- `01-r4rs` : Tests that are specific to R4RS.

# Running the tests

To run the tests, you can simply run the following command in the `src` directory:

```bash
HOST=<host> make check
```

On supported hosts, the REPL can be tested against the r4rs compliance tests by running the following command:

```bash
HOST=<host> make check-repl
```

Where `<host>` is the host language you want to test. For example, <host> can be any of `c`, `js`, `asm`, `hs`, `py`. See
the [front page](./../../README.md) for a list of all supported hosts.

# Test infrastructure

## Test runner

The test runner is a simple POSIX-shell script that can run any tests inside the `tests` directory.
Given a host and test file, it will perform broadly the following steps:

1. Compile the scheme test file using `rsc.scm` to the target host file. Extra arguments can be passed to `rsc.scm` using the `;;;run` comment,
   or with the `TEST_FEATURES` variable in the host Makefile.
2. Run the target host file using the host interpreter or compiler defined in the hosts Makefile. Stdin can be passed using
   the `;;;input:` comment in the source code of the test.
3. Compare the output of the target host file and the expected output specified in the test file with the `;;;expected` comment.

For more details about the test runner, see the [test-runner](./../run_test.sh) script or continue reading.

## Host Makefiles

Ribbit supports a lot of different hosts, each having their own unique way of being
compiled or interpreted. For this reason, Makefiles inside each host directory specify how
to compile or interpret their source code. These Makefiles can
define the following variables :

- `HOST` : The name of the host language.
- `HOST_COMPILER` : The command used to compile the source code. To compile the
   code, the command is used as follows : `$(HOST_COMPILER) <OUTPUT_FILE> <INPUT_FILE>`.
- `HOST_INTERPRETER` : The command used to interpret the source code. To interpret the
   code, the command is used as follows : `$(HOST_INTERPRETER) <INPUT_FILE>`.

Because each host are at different stages of implementations, hosts have
tags that indicate what features are supported. These tags are then used to decide what tests will be executed.
These tags are defined inside the Makefile of each host alongside the `HOST` and other variables :

- `TEST_TAGS` : A list of tags that are supported by the host. For example `c`, `core`, `variadics`, `r4rs`, etc. Only
tests with the matching tags will be run, see the `;;;<tag>-run:` comment below.
- `TEST_FEATURES` : A list of arguments that need to be tested against *all* tests.
This includes features such as compression, minification, encodings. Note that adding an element to this list will
drastically increase the number of tests that will be run, as the whole suite of tests is run for each element in this list.

Note that all of these variables can be redefined in the command line. For example, if you wish to test the `c` host with `clang` and to only run the `r4rs` test suite, you can do :

```bash
HOST=c HOST_INTERPRETER="clang -o" TEST_TAGS=r4rs make check
```

For an example of Makefile, see the [c host makefile](./../host/c/makefile).

## Test files

Tests include comments that are used by the test runner to check whenever this test should be supported by
the host RVM and the correctness of the output. Comments inside the test file can be any of :

`;;;<tag>-run: <argv>` or `;;;run: <argv>` : This comment is used to specify the argument (argv) that need to be passed
to `rsc.scm` when compiling this scheme file. Without any tag, the test will only be run if the RVM supports the
`core` tag. If a tag is specified, the test will only run if the tag is present for the specified host (`TEST_TAGS` inside the host’s Makefile).

`;;;expected:` : This comment is used to specify the expected output of the test. The test must return
this output to be considered correct.

`;;;input:` : This specified a string to be passed as standard input when running the test in the host language.

`;;;cleanup:` : This comment is used to specify a command that will be running after the test is executed. This can be used
to remove generated files, for example.


## Examples

### File test example

For example, here is the (35-eval.scm)[00-ribbit/35-eval.scm] test:

```
(export *)

(write (eval (read)))
(##putchar 10)

;;;run: -l min
;;;run: -l max
;;;r4rs-run: -l r4rs
;;;input:(* 6 7)
;;;expected:
;;;42
```

If the host supports the `core` tag, this test will be run twice. Once linking the `min` library and once linking the
`max` library. In addition, if the host supports the `r4rs` tag, this test will be running a third time with the `r4rs` library.
For each of the executions, the test will read the input `(* 6 7)` and the output will be checked against the expected output `42`.

### Makefile example

Here is an example of a Makefile for the `c` host :

```make
HOST = c
HOST_COMPILER = $${C_HOST_COMPILER:-$(CC) -o}
TEST_TAGS=c core r4rs
TEST_FEATURES ?= -f+ compression/lzss/2b, -e optimal, -f+ compression/lzss/2b -e optimal
include ../../makefile-common.mk
```

This Makefile specifies that the host is `c`, the compiler is `$(CC) -o`, and that the host supports the `c`, `core` and `r4rs` tags.

Here, the `TEST_FEATURES` variable specifies 3 command line arguments that need to be tester **for each** test.
This means that the whole test suite is executed 4 times :
 - Once without any extra features.
 - Once with the `compression/lzss/2b` feature (compression of the RIBN).
 - Once with the `optimal` encoding.
 - Once with both the `compression/lzss/2b` and `optimal` encoding features.

In those settings, during one test execution, the `35-eval.scm` test from the previous subsection is executed a total of 12 times. The whole test
suite is executed 4 times. The test itself will be executed 3 times for each time the whole test suite is executed.
