Ribbit ocaml host notes
=======================

Usage:

    $ ./rsc -t ml -o repl_max.ml repl-max.scm
    $ ocamlc -o repl_max repl_max.ml
    $ ./repl_max

Note you need to use the `-o` option to `rsc` or otherwise rename the
output file to something that ocaml accepts - the default *foo.scm.ml*
format is invalid.

Debugging and stack tracing can be enabled by setting the
`RIBBIT_DEBUG` environment variable at runtime (You should compile
with `-g` if doing so):

    $ RIBBIT_DEBUG=1 ./repl_max
