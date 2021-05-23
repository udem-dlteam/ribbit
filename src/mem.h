typedef unsigned short word;
typedef word obj;  /* an object reference */

/*
 * The basic data structure is a clump, which contains CLUMP_NB_FIELDS
 * object references.  It could have been called a "triplet" when
 * CLUMP_NB_FIELDS = 3 but by calling it a "clump" it makes it easier
 * to add or remove fields in an evolution of the data structure
 * without changing the type's name.
 */

#define CLUMP_NB_FIELDS 3

struct clump {
  obj header;
  obj fields[CLUMP_NB_FIELDS];
};

#define nil 0

#define fixnum(n) ((n) << 1)
#define fixnum_to_int(o) ((o) >> 1)

#define obj_tag 1
#define mem_allocated(o) ((o) & 1)
#define mem_allocated_base(o) ((o) - obj_tag + mem_base))

#define clump(o) ((struct clump*)(mem_allocated_base(o))

#define get_header(o) clump(o)->header
#define set_header(o, x) clump(o)->header = x

#define get_field(i, o) clump(o)->fields[i]
#define set_field(i, o, x) clump(o)->fields[i] = x

#define heap_start 0x1000
#ifdef PC
#define heap_end   0x7c00
#else
#define heap_end   0x1800 /* small heap for testing */
#endif

#define max_nb_objs ((heap_end - heap_start) / (2*sizeof(struct clump)))

#define heap_bot (heap_start+obj_tag)
#define heap_mid (heap_bot+max_nb_objs*sizeof(struct clump))
#define heap_top (heap_bot+2*max_nb_objs*sizeof(struct clump))
