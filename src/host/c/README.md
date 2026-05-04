# C Host

## Garbage Collectors

The C host can be used with different garbage collectors:
- **Stop-and-copy** : Default GC
- **Mark-and-sweep** : Supported in the original RVM, use `-f+ c/gc/mark-sweep`
- **Reference counting** : WIP
- **Arborescent Garbage Collector** (AGC) : see below

### Arborescent Garbage Collection

To use AGC, the RVM's path must be redirected to the file `agc.c` using the
command `--rvm host/c/es.c` (assuming this was done from the `src` directory).
To use the R4RS library with AGC, the feature `-f+ c/gc/es` must be activated
(for the `apply` and IO primitives to work properly).

For example, the following command will generate a C RVM that, when executed,
will execute the program `fib.scm` using the Arborescent Garbage Collector and
the R4RS library:
```
./rsc.exe -t c -r host/c/es.c -l r4rs -f+ c/gc/es fib.scm -o fib.c
```

#### Features:

AGC can be used with several features listed below. Each feature can be
activated with the `-f+ <feature>` flag.

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
rank, the higher the priority).

#### Tests

The test suite can be executed using the command:
```
HOST=c TEST_TAGS="c core"  make check-es
```
Individual features can be tested as well using this command but adding
the following `HOST_FEATURES="-f+ <feature>"`. For example, to test the
`queue-no-remove` feature, one can use the following command:
```
HOST=c TEST_TAGS="c core"  HOST_FEATURES="-f+ queue-no-remove" make check-es
```