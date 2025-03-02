# C Host

## Garbage Collectors

Many collectors to choose from:
- **Stop-and-copy** : Default garbage collector
- **Mark-and-sweep** : Supported in the original RVM, use `-f+ c/gc/mark-sweep`
- **Reference counting** : WIP
- **Incremental collector** : see below

### Incremental Garbage Collector

To use the incremental collector, the RVM's path must be redirected to the file
`es.c` using the command `--rvm host/c/es.c` (assuming this was done from the 
`src` directory). To use the R4RS library with the collector, the feature 
`-f+ c/gc/es` must be activated (for the `apply` and IO primitives to work
properly).

For example, the following command will generate a C RVM that, when executed,
will execute the program `fib.scm` using the incremental collector and the
R4RS library:
```
./rsc.exe -t c -r host/c/es.c -l r4rs -f+ c/gc/es fib.scm -o fib.c
```

#### Features:

The incremental collector can be used with several features listed below. Each
feature can be activated with the `-f+ <feature>` flag.

Features from different categories can be combined with one another but it's 
not necessarily possible to combine features within the same category, e.g.
`parent-field` can be combined with `linked-list` but `rank-adoption` and 
`count-adoption` can't be combined with one another.

**Encoding features**. These features modify the basic memory layout of ribs
and the way that referrers are chained together. They result in a more 
performant collector at the cost of additional per-object memory.
- `parent-field` : This adds an additional field to store the parent of an
object explicitely, thus avoiding the need to use the convention that the
first referrer should be the parent and allowing for a faster `set_parent`
procedure as well as a generally faster adoption.
- `double-mirrors` : WIP

**Adoption strategy features**. These features modify the adoption heuristic
and the selection process for an adopter.
- `rank-adoption` : WIP
- `count-adoption` : WIP

**Catch/anchor queue features**. Changes the type of data structure used
for the catch and anchor queue.
- `queue-no-remove` : Same as the default queue used for the anchors and 
catchers but with an extra field to allow a falling object to be in the
anchor queue and drop queue simultaneously, thus avoiding the need to
remove a falling object from the anchor queue until the catch phase.
- `linked-list` : Uses a linked list based priority queue instead of a 
queue. The priority is determined by the rank by default (the higher the
rank, the higher the priority) but this priority will be determined by
the number of referrers if the `count-adoption` feature is used (WIP).

#### Tests

The test suite can be executed using the command:
```
HOST=c TEST_TAGS="c core"  make check-es
```
Individual features can be tested as well using this command but adding
the following `HOST_FEATURES="-f+ <feature>"`. For example, to test the
`parent-field` feature, one can use the following command:
```
HOST=c TEST_TAGS="c core"  HOST_FEATURES="-f+ parent-field" make check-es
```