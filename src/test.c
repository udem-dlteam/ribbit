#include "mem.h"
#include "types.h"
#include "io.h"
#include "stdlib.h"


void test_gc() {
  /* obj tmp; */

  /* __init_heap(); */

  /* for (int i=1; i<1001; i++) { */

  /*   __push_clump(); */

  /*   if ((i & 7) == 7) { */
  /*     set_field(0, __stack(), fixnum(i)); */
  /*     set_field(1, __stack(), nil); */
  /*   } else { */
  /*     __pop_clump(); */
  /*   } */

  /*   if (i % 50 == 0) { /1* every 100 iters, print the stack *1/ */
  /*     obj probe = __stack(); */
  /*     set_field(1, __stack(), __stack()); /1* create a cycle... just for testing *1/ */
  /*     while (probe != nil) { */
  /*       print_i(fixnum_to_int(get_field(0, probe))); */
  /*       probe = __get_field(2, probe); */
  /*     } */
  /*     __putc('\n'); */
  /*   } */
  /* } */
}
