typedef unsigned short word;
typedef word obj;  /* an object reference */

/*
 * The basic data structure is a clump, which contains CLUMP_NB_FIELDS
 * object references.  It could have been called a "quadruplet" when
 * CLUMP_NB_FIELDS = 4 but by calling it a "clump" it makes it easier
 * to add or remove fields in an evolution of the data structure
 * without changing the type's name.
 */

#define CLUMP_NB_FIELDS 4

typedef struct clump {
  obj fields[CLUMP_NB_FIELDS]; /* field 0 is the header used by the GC */
} clump;

#define nil 0

#define fixnum(n) ((n) << 1)
#define fixnum_to_int(o) ((o) >> 1)

#define obj_tag 1
#define mem_allocated(o) ((o) & 1)

#define ptr_to_obj(ptr) (((ptrdiff_t)(ptr) - mem_base) + obj_tag)
#define ptr_from_obj(o) ((obj*)((o) - obj_tag + mem_base))
#define clump_from_obj(o) ((clump*)(mem_allocated_base(o))

#define get_field(i, o) ptr_from_obj(o)[i]
#define set_field(i, o, x) ptr_from_obj(o)[i] = x

#define heap_start 0x1000
#ifdef PC
#define heap_end   0x7c00
#else
#define heap_end   0x1800 /* small heap for testing */
#endif

#define max_nb_objs ((heap_end - heap_start) / (2*sizeof(clump)))

#define heap_bot ((obj*)(mem_base+heap_start))
#define heap_mid (heap_bot+max_nb_objs*CLUMP_NB_FIELDS)
#define heap_top (heap_bot+max_nb_objs*CLUMP_NB_FIELDS*2)
