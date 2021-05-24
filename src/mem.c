/* Memory management using a Cheney-style stop-and-copy GC */

#include <stdio.h>

#include "mem.h"

#ifdef PC

#define mem_base 0  /* use real addresses */

#else

#include <stdlib.h>
#include <stddef.h>
ptrdiff_t mem_base;  /* emulate memory using a malloc'ed block */

#endif

obj *alloc;       /* heap allocation pointer, moves up */
obj *alloc_limit; /* allocation limit pointer */
obj *scan;        /* scan pointer */

obj latest;       /* latest allocated object */
obj stack;        /* stack of the VM */

void init_heap() {

#ifndef PC
  mem_base = (ptrdiff_t)malloc(heap_start+2*max_nb_objs*sizeof(clump));
#endif

  alloc = heap_bot;
  alloc_limit = heap_mid;
  stack = nil;
}

void update() {
  obj o = *scan;
  if (mem_allocated(o)) {
    obj *ptr = ptr_from_obj(o);
    obj copy = *ptr;
    if (copy == nil) {
      /* object not yet copied, so make a copy */
      copy = ptr_to_obj(alloc);
      *ptr++ = copy; /* store forwarding pointer */
      *alloc++ = nil;  /* allocate copy with null forwarding pointer */
      *alloc++ = *ptr++; /* copy 3 fields of o */
      *alloc++ = *ptr++;
      *alloc++ = *ptr++;
      /*
        in assembly, if ptr is in SI and alloc is in DI, the copy could
        be done with 3 bytes of code:
        A5   MOVSW
        A5   MOVSW
        A5   MOVSW
      */
    }
    *scan = copy;
  }
  scan++;
}

void gc() {

  obj *start = (alloc_limit == heap_mid) ? heap_mid : heap_bot;
  /*
    in assembly, if the heap size is a power of 2, this could be done
    by flipping a single bit in alloc_limit (1 XOR instruction)
  */

  alloc_limit = start + max_nb_objs*CLUMP_NB_FIELDS;
  alloc = start;

  /* update roots */

  scan = &stack;
  update();  /* copy stack */

  /* copy reachable objects */

  scan = start;

  while (scan != alloc) {
    scan++; /* skip field 0 which is the header */
    update(); /* update fields 1, 2, 3 */
    update();
    update();
    /*
      in assembly, if scan is in register BX and update increments BX
      on every call, the body of the loop could be done
      with 11 bytes of code:
           INC  BX
           INC  BX
           CALL update
           CALL update
           CALL update
    */
  }

  if (alloc == alloc_limit) {
#ifdef PC
    halt();
#else
    printf("heap overflow!\n");
    exit(1);
#endif
  }
}

void push_clump() {

  /* allocate and initialize object and add it to stack */

  *alloc++ = nil; /* null forwarding pointer */
  *alloc++ = stack;
  *alloc++ = stack;
  *alloc++ = stack;

  stack = ptr_to_obj(alloc-CLUMP_NB_FIELDS);

  /* start a garbage collection if free space exhausted */

  if (alloc == alloc_limit) {
    gc(); /* will move objects including the stack */
  }
}

obj pop_clump() {
  obj o = stack;
  stack = ptr_from_obj(stack)[CLUMP_NB_FIELDS-1];
  return o;
}

#ifndef PC

/* test the GC... keeping one out of 8 objects allocated */

int main() {

  obj tmp;
  int i;

  init_heap();

  for (i=1; i<1001; i++) {

    push_clump();

    if ((i & 7) == 7) {
      set_field(1, stack, fixnum(i));
      set_field(2, stack, nil);
    } else {
      pop_clump();
    }

    if (i % 50 == 0) { /* every 100 iters, print the stack */
      obj probe = stack;
      while (probe != nil) {
        printf("%d ",fixnum_to_int(get_field(1, probe)));
        probe = get_field(3, probe);
      }
      printf("\n");
    }
  }

  return 0;
}

#endif
