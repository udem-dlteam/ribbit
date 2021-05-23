/* Memory management using a Cheney-style stop-and-copy GC */


#include "mem.h"

#ifdef PC

#define mem_base 0
#include "utils.h"
#include "types.h"

#else

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

ptrdiff_t mem_base;

#endif

obj stack;       /* stack of the VM */
obj alloc;       /* heap allocation pointer */
obj alloc_limit; /* allocation limit pointer */

void init_heap() {

#ifndef PC
  mem_base = (ptrdiff_t)malloc(heap_start+2*max_nb_objs*sizeof(struct clump));
#endif

  stack = nil;
  alloc = heap_top;
  alloc_limit = alloc - max_nb_objs*sizeof(struct clump);
}

#define alloc_clump() (alloc -= sizeof(struct clump))

obj update(obj o) {
  if (!mem_allocated(o)) {
    return o;
  } else {
    obj copy = get_header(o);
    if (copy == nil) {
      /* object not yet copied, so make a copy */
      alloc_clump();
      set_header(o, alloc);
      set_header(alloc, nil); /* null forwarding pointer */
      set_field(0, alloc, get_field(0, o));
      set_field(1, alloc, get_field(1, o));
      set_field(2, alloc, get_field(2, o));
      copy = alloc;
    }
   return copy;
  }
}

obj gc() {

  /* alloc pointer is the latest object allocated (which is live) */

  obj latest = alloc;
  obj scan = (alloc_limit == heap_bot) ? heap_top : heap_mid;

  alloc = scan;
  alloc_limit = alloc - max_nb_objs*sizeof(struct clump);

  /* update roots */

  stack = update(stack);   /* copy stack */
  latest = update(latest); /* copy latest object allocated */

  /* copy reachable objects */

  while (scan != alloc) {
    scan -= sizeof(struct clump);
    set_field(0, scan, update(get_field(0, scan)));
    set_field(1, scan, update(get_field(1, scan)));
    set_field(2, scan, update(get_field(2, scan)));
  }

  if (alloc == alloc_limit) {
#ifdef PC
    halt();
#else
    printf("heap overflow!\n");
    exit(1);
#endif
  }

  return latest;
}

obj new_clump(obj field0, obj field1, obj field2) {

  /* allocate and initialize object */

  alloc_clump();
  set_header(alloc, nil); /* null forwarding pointer */
  set_field(0, alloc, field0);
  set_field(1, alloc, field1);
  set_field(2, alloc, field2);

  /* check that free space remains available */

  if (alloc != alloc_limit) return alloc;

  return gc();
}

#ifndef PC

/* test the GC... keeping one out of 8 objects allocated */
int main() {

  obj tmp;
  int i;

  init_heap();

  for (i=0; i<14000; i++) {
    tmp = new_clump(fixnum(i), nil, stack);
    if ((i & 7) == 7) stack = tmp;
    if (i % 100 == 0) { /* every 100 iters, print the stack */
      obj probe = stack;
      while (probe != nil) {
        printf("%d ",fixnum_to_int(get_field(0, probe)));
        probe = get_field(2, probe);
      }
      printf("\n");
    }
  }

  return 0;
}

#endif
