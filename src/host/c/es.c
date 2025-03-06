/* 
 * File: es.c
 *
 * Author: Frédéric Lahaie-Bertrand
 *
 * Implementation of the Ribbit Virtual Machine in C with an incremental 
 * garbage collector inspired by Even-Shiloach trees
 */

/* 
 * TODO
 * - Ref count (make sure all non-cyclic ribs are collected, adapt io and sys
 *   primitives, apply, ... you know, make it work)
 * - Mirror fields with offset or with explicit mirror fields
 * - Double mirror fields
 * - Call a GC for every instruction to make sure everything is collected...
 *   (do that for the bootstrap, the test suite, and fuzzing)
 * - Cleanup the code and fix the FIXMEs and TODOs in the code
 *
 * When the stars will align
 * - Queues implemented directly on the heap with (potentially) a sorting phase
 *   OR make the heap dynamic
 * - Allocation strategy using ref count rather than ranks
 * - Don't chain FALSE's referrers
 * - Better system for negative ranks
 * - Reranking phase
 * - Finalizers
 * - Concurrent or parallel deallocation (probably won't happen for ribbit)
 * - Memory transactions for mutations
 * - Ref count / mark-and-sweep: last object should be a "null rib", not _NULL
 * - Make it a full RVM: add missing features from the original RVM (encoding,
 *   sys primitives, (DEBUG, NO_STD, lzss compression, ARG_V, clang support, 
 *   stop & copy GC, DEFAULT_REPL_MIN, NOSTART, CHECK_ACCESS, etc.)
 * - ... optimizations?
 */

// @@(location import)@@
// @@(location decl)@@

#define true 1
#define false 0

#include <stdio.h>
#include <stdlib.h>

// @@(feature ref-count
#define REF_COUNT
// )@@

// @@(feature queue-no-remove
#define QUEUE_NO_REMOVE
// )@@

// @@(feature collect-queue
#define COLLECT_QUEUE
// )@@

// @@(feature linked-list
#define LINKED_LIST
// )@@

// @@(feature debug-field
#define DEBUG_FIELD
// )@@

// @@(feature debug/rib-viz
#define VIZ
void viz_heap(char* name);
// )@@

// @@(feature dprint
#define DPRINT
#define dprint(...) printf(__VA_ARGS__)
#define IRUN_COUNTER
// )@@

#ifndef DPRINT
#define dprint(...) 0
#endif

// @@(feature check-spanning-tree (use debug/rib-viz debug-field)
#define CHECK_SPANNING_TREE
void check_spanning_tree_impl();
#define check_spanning_tree() check_spanning_tree_impl()
#include <signal.h>
#define IRUN_COUNTER
// )@@

#ifndef CHECK_SPANNING_TREE
#define check_spanning_tree() 0
#endif


// @@(feature always-adupt
#define ALWAYS_ADUPT
// )@@

// @@(feature no-adupt
#define NO_ADUPT
// )@@

#define GLOBAL_RANK_COUNTER
// @@(feature no-global-rank-counter
#undef GLOBAL_RANK_COUNTER
// )@@

// TODO use limits instead
#define UNALLOCATED_RIB_RANK 1152921504606846976
#define FALLING_RIB_RANK 1152921504606846975
#define MAX_RANK 1152921504606846974
#define MIN_RANK -1152921504606846976
#define MAX_ADUPT_TRIES 10

// @@(feature debug/clean-ribs
#define CLEAN_RIBS
// )@@

// @@(feature (not compression/lzss/2b)
// @@(replace "41,59,39,117,63,62,118,68,63,62,118,82,68,63,62,118,82,65,63,62,118,82,65,63,62,118,82,58,63,62,118,82,61,33,40,58,108,107,109,33,39,58,108,107,118,54,121" (encode-as-bytes "auto" "" "," "")
unsigned char input[] = {41,59,39,117,63,62,118,68,63,62,118,82,68,63,62,118,82,65,63,62,118,82,65,63,62,118,82,58,63,62,118,82,61,33,40,58,108,107,109,33,39,58,108,107,118,54,121,0}; // RVM code that prints HELLO!
// )@@
// )@@

// basic def. of a boolean
typedef unsigned char bool;

// an unsigned byte value for the REPL's code
typedef unsigned char byte;

// a tagged value
typedef unsigned long obj;

// a number
typedef long num;

#ifdef QUEUE_NO_REMOVE
#define QUEUE_NO_REMOVE_count 1
#else
#define QUEUE_NO_REMOVE_count 0
#endif

#ifdef DEBUG_FIELD
#define DEBUG_FIELD_count 1
#else
#define DEBUG_FIELD_count 0
#endif

#ifdef REF_COUNT
#define RIB_NB_FIELDS 4
#else
#define RIB_NB_FIELDS (10+QUEUE_NO_REMOVE_count+DEBUG_FIELD_count)
#endif

typedef struct {
  obj fields[3];   // fields, references or nums
  obj m_fields[3]; // mirror fields
  obj refs;        // list of referrers
  obj rank;        // object's rank
  obj queue;       // drop and anchor/catch queue field
#ifdef QUEUE_NO_REMOVE
  obj catch_queue; // anchor/catch field
#endif
  obj p_field;     // explicit parent field
#ifdef DEBUG_FIELD
  obj debug;       // debug field
#endif
} rib;

#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]

#ifndef REF_COUNT
#define M_CAR(x) RIB(x)->m_fields[0]
#define M_CDR(x) RIB(x)->m_fields[1]
#define M_TAG(x) RIB(x)->m_fields[2]
#define CFR(x) RIB(x)->refs
#define PAR(x) RIB(x)->p_field
#define RANK(x) RIB(x)->rank
#define Q_NEXT(x) RIB(x)->queue
#ifdef QUEUE_NO_REMOVE
#define PQ_NEXT(x) RIB(x)->catch_queue
#else
#define PQ_NEXT(x) Q_NEXT(x)
#endif
#endif

#define get_field(x,i) RIB(x)->fields[i]
#define get_m_field(x,i) RIB(x)->m_fields[i]

#define UNTAG(x) ((x) >> 2)
#define NUM(x) ((num)(UNTAG((num)(x))))
#define RIB(x) ((rib *)(x))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x) && x != _NULL)
#define TAG_NUM(num) ((((obj)(num)) << 2) | 1)
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))

#define MARK(x) ((x)|2) // assumes x is a tagged obj (ul)
#define UNMARK(x) ((x)^2)
#define IS_MARKED(x) ((x)&2)


#define INSTR_AP 0
#define INSTR_SET 1
#define INSTR_GET 2
#define INSTR_CONST 3
#define INSTR_IF 4
#define INSTR_HALT 5

#define EXIT_ILLEGAL_INSTR 6
#define EXIT_NO_MEMORY 7

#define PAIR_TAG TAG_NUM(0)
#define CLOSURE_TAG TAG_NUM(1)
#define SYMBOL_TAG TAG_NUM(2)
#define STRING_TAG TAG_NUM(3)
#define SINGLETON_TAG TAG_NUM(5)

#define _NULL ((obj)NULL)

#define NUM_0 (TAG_NUM(0))

// the only three roots allowed
obj stack = NUM_0;
obj pc = NUM_0;
obj FALSE = NUM_0;

// global but not a root, referenced
obj symbol_table = NUM_0;

size_t pos = 0;

// TRUE and NIL are always live objects so there's no need to chain their
// referrers (they will never be dropped since they're always reachable from
// FALSE, for the check to work in `add_ref`, TRUE and NIL must be defined
// before FALSE become an object, this also allows for FALSE to become their
// parent before we stop chaining their referrers
// FIXME apply the same logic to FALSE, something causes a leak if I don't
// chain FALSE for the moment, not sure why that is
obj TRUE = NUM_0;
obj NIL = NUM_0;
/* #define TRUE (CAR(FALSE)) */
/* #define NIL (CDR(FALSE)) */

#define TOS (CAR(stack))

// Temp values that can be used to shield pointers from the evil GC

// WARNING when using the ES garbage collector, the rank of a temporary
// register must be set to the same rank as the rib's current parent (or
// a lower rank) to avoid breaking the rank order invariant

// FIXME if there's a bug with the `apply` primitive when using the ES
// garbage collector, it might me because the rank should be set to 0
// instead of 1 but the rank should be set back to 0 when clearing the
// register (see the lib files). The rank must be reset before using
// the registers because the IO primitives (among other things) use
// TRUE/NIL which can change their rank.
#define TEMP1 CAR(TRUE)
#define TEMP2 CDR(TRUE)
#define TEMP3 CAR(NIL)
#define TEMP4 CDR(NIL)

#ifndef REF_COUNT
// We use the space of an entire rib on the heap for the end of the free list,
// might as well use that rib to store temporary values (e.g. popped ribs)
obj null_rib; // temporary values
#define TEMP5 (CAR(null_rib))
#define TEMP6 (CDR(null_rib))
#define TEMP7 (TAG(null_rib))
#endif

obj *alloc;
obj *scan;

num alloc_rank = 0;

rib *heap_start;
#define MAX_NB_OBJS 1000000
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_top (heap_bot + (SPACE_SZ))


#ifndef REF_COUNT

//==============================================================================

// Data structures (ES Trees)


// Drop queue 

obj q_head; // dequeue at head => O(1)
obj q_tail; // enqueue at tail => O(1)

#define Q_INIT() q_head = _NULL; q_tail = _NULL

#define Q_IS_EMPTY() (q_head == _NULL)

void q_enqueue(obj o) {
  // In theory there shouldn't be any reason as to why a rib gets enqueued
  // twice in the queue given that its rank gets set to -1 and that we don't
  // enqueue a rib that has a rank -1 (and that we don't reset its rank until
  // we enter the catch phase... could change if we implement the "adopt"
  // mechanism)
  // if (Q_NEXT(o) == _NULL && q_head != o && q_tail != o) { // no duplicates in the queue
    if (Q_IS_EMPTY()){
      q_head = o;
    } else {
      Q_NEXT(q_tail) = o;
    }
    q_tail = o;
    // }
}

obj q_dequeue() {
  if (Q_IS_EMPTY()){
    return _NULL;
  }
  if (q_head == q_tail) {
    q_tail = _NULL;
  }
  obj tmp = q_head;
  q_head = Q_NEXT(tmp);
  Q_NEXT(tmp) = _NULL;
  return tmp;
}


//------------------------------------------------------------------------------

// Catch queue & collect queue

// Picking the right data structure to implement the priority queue is pretty
// important since the complexity of deleting an edge depends a lot on that...
// some options:
//
//  - Linked list (used temporarily): simple and requires only 1 extra field
//    but enqueue, dequeue, and remove are all in O(n)
//
//  - Buckets: O(#ranks) for enqueue, O(1) for dequeue, and O(#ranks + m) where
//    m is the number of ribs in the largest bucket. Requires 2  extra field to
//    keep a reference to the next rib of the same bucket (of the same rank)
//    and one to keep a reference of the first rib in the next bucket (bucket
//    with rank+=1)
//
//  - Red-black trees: O(lg(n)) for enqueue and dequeue/removal but requires 3
//    extra fields: 2 for the left and right subtrees and 1 for the parent.
//    Tagging can be used for the colour
//
//  - Heap
//
//  - Something else...?

obj pq_head;
obj pq_tail;

#define PQ_INIT() pq_head = _NULL; pq_tail = _NULL

#define PQ_IS_EMPTY() (pq_head == _NULL)


#ifdef LINKED_LIST

// Priority queue implemented with a singly linked list and a remove
// operation (requires no extra field)

void pq_enqueue(obj o) {
  // the lower the rank, the closer the rib is to pq_head
  if (PQ_NEXT(o) == _NULL && pq_tail != o) {
    if (PQ_IS_EMPTY()){
      pq_head = o;
      pq_tail = o;
    }
    else if (NUM(RANK(o)) > NUM(RANK(pq_head))) { 
      PQ_NEXT(o) = pq_head;
      pq_head = o;
    }
    else {
      // insert new rib after the first rib that has a lower rank
      obj prev = pq_head;
      obj curr = PQ_NEXT(pq_head);
      while (curr != _NULL && RANK(curr) > RANK(o)){
        prev = curr;
        curr = PQ_NEXT(curr);
      }
      if (curr == _NULL) {
        pq_tail = o;
      }
      PQ_NEXT(o) = curr; // could be _NULL, potentially redundant
      PQ_NEXT(prev) = o;
    }
  }
}

obj pq_dequeue() {
  if (PQ_IS_EMPTY()){
    return _NULL;
  }
  if (pq_head == pq_tail) {
    pq_tail = _NULL;
  }
  obj tmp = pq_head;
  pq_head = PQ_NEXT(tmp); // could be _NULL, potentially redundant
  PQ_NEXT(tmp) = _NULL; // same as above
  return tmp;
}

void pq_remove(obj o) {
  if (PQ_NEXT(o) == _NULL && pq_tail != o) { // o not in set?
    return;
  }
  if (pq_head == o) {
    obj tmp = pq_head;
    pq_head = PQ_NEXT(tmp);
    if (pq_tail == o) {
      pq_tail = _NULL;
      return;
    }
    PQ_NEXT(tmp) = _NULL;
  } else {
    obj curr = PQ_NEXT(pq_head);
    obj prev = pq_head;
    // Assumes that PQ_NEXT of a rib can't contain a reference to itself
    while (curr != o && curr != _NULL) {
      prev = curr;
      curr = PQ_NEXT(curr);
    }
    if (curr == pq_tail) {
      pq_tail = prev;
    }
    PQ_NEXT(prev) = PQ_NEXT(curr);
    PQ_NEXT(curr) = _NULL;
    PQ_NEXT(o) = _NULL;
  }
}

#else

void pq_enqueue(obj o) {
  // In the case of the pqueue, we could attempt to add the
  // same rib twice (e.g. if two ribs have the same co-friend)
  if (PQ_NEXT(o) == _NULL && pq_tail != o) {
    if (PQ_IS_EMPTY()){
      pq_head = o;
    } else {
      PQ_NEXT(pq_tail) = o;
    }
    pq_tail = o;
  }
}

// #define pq_enqueue(o) if (PQ_NEXT(o) == _NULL && pq_tail != o) _pq_enqueue(o)

#ifdef QUEUE_NO_REMOVE

// Queue with no remove, requires an extra field since a falling rib could still
// be in the anchor queue when we enqueue it in the drop queue

obj pq_dequeue() {
  // Get rid of falling ribs
  obj tmp;
  while (!PQ_IS_EMPTY()) {
    if (NUM(RANK(pq_head)) == FALLING_RIB_RANK) { // falling?
      tmp = PQ_NEXT(pq_head);
      PQ_NEXT(pq_head) = _NULL;
      pq_head = tmp;
    } else {
      break; // found a non-falling rib
    }
  }
  if (PQ_IS_EMPTY()){
    pq_tail = _NULL;
    return _NULL;
  }
  if (pq_head == pq_tail) {
    pq_tail = _NULL;
  }
  tmp = pq_head;
  pq_head = PQ_NEXT(tmp);
  PQ_NEXT(tmp) = _NULL;
  return tmp;
}

#define pq_remove(o) NULL


#else

// Default: queue with remove operation, requires no extra field
// Seems to be faster than the priority queue implemented with a singly
// linked list but potentially doesn't rebalance the spanning tree since
// ribs are not dequeued by rank

obj pq_dequeue() {
  if (PQ_IS_EMPTY()){
    return _NULL;
  }
  if (pq_head == pq_tail) {
    pq_tail = _NULL;
  }
  obj tmp = pq_head;
  pq_head = PQ_NEXT(tmp);
  PQ_NEXT(tmp) = _NULL;
  return tmp;
}

void pq_remove(obj o) {
  if (PQ_NEXT(o) == _NULL && pq_tail != o) { // o not in set?
    return;
  }
  if (pq_head == o) {
    obj tmp = pq_head;
    pq_head = PQ_NEXT(tmp);
    if (pq_tail == o) {
      pq_tail = _NULL;
      return;
    }
    PQ_NEXT(tmp) = _NULL;
  } else {
    obj curr = PQ_NEXT(pq_head);
    obj prev = pq_head;
    // Assumes that PQ_NEXT of a rib can't contain a reference to itself
    while (curr != o && curr != _NULL) {
      prev = curr;
      curr = PQ_NEXT(curr);
    }
    if (curr == pq_tail) {
      pq_tail = prev;
    }
    PQ_NEXT(prev) = PQ_NEXT(curr);
    PQ_NEXT(curr) = _NULL;
    PQ_NEXT(o) = _NULL;
  }
}

#endif
#endif


#ifdef COLLECT_QUEUE
// Wipe out the collect queue after dealloc phase
void pq_wipe() {
  obj tmp;
  while (!PQ_IS_EMPTY()){
    obj tmp = pq_head;
    pq_head = PQ_NEXT(pq_head);
    PQ_NEXT(tmp) = _NULL;
  }
  pq_tail = _NULL;
}
#endif


//==============================================================================

// Even-Shiloach trees

// Information that needs to be stored in each ribs
//
//  - Rank: Distance from the root, requires its own field 
//
//  - Parent: Closest co-friend from the root, we adopt the convention that the
//    parent of a rib is simply his first co-friend (the rib accessible
//    through the 7th field of a rib) thus avoiding the need to tag the
//    parent. Consequently, a rib can have at most one parent (a root has none)
//
//  - Children: If X is the parent of Y then Y is the child of X. This is an
//    implicit relationship for now and doesn't really serve any purpose for the
//    implementation (maybe tagging would be more efficient, see below)
//
//  - Friends: Y is said to be a friend of X if one of X's (first 3) fields is
//    a reference to Y. No need to do anything here since the references to a
//    rib's friends are already stored in its first 3 fields
//
//  - Co-friends: Y is said to be a co-friend of X if one of Y's (first 3)
//    fields is a reference to X. A rib's (X) co-friends are chained together
//    starting from the rib's 7th field (P, the parent) and then using one of
//    the 3 mirror fields (the one that mirrors the reference to X) to point
//    to the next co-friend and so on. In the example below, if a new co-friend
//    were added to X, it would be chained to Y using its 6th field because
//    Y points to X using its 3rd field (note that only the first 7th field
//    are shown to simplify my beautiful ASCII art)
//
//     P (X's parent)   +-------------+      Y (X's co-friend but not parent)
//    +---+---+---+---+-|-+---+---+   |     +---+---+---+---+---+---+---+
//    | _ | + | _ | _ | + | _ | _ |   +---->| _ | _ | + | _ | _ | _ | _ |
//    +---+-|-+---+---+---+---+---+         +---+---+-|-+---+---+---+---+
//      ^   |      mirror flds                        +------+
//      |   |                                                |
//      +--------------------------------------------+       |
//          |            X +---+---+---+---+---+---+-|-+     |
//          +------------->| _ | _ | _ | _ | _ | _ | + |<----+
//                         +---+---+---+---+---+---+---+
// 
// Additional notes:
//
//  - The friends/co-friends relationship between each objects represents the
//    object graph(s) while the parent/child relationship forms the spanning
//    tree of those graphs. The reachability of an object is determined by its
//    rank (i.e. its distance from the root which is itself determined by the
//    spanning tree)

#define get_rank(x) (NUM(RANK(x)))
#define set_rank(x, rank) (RANK(x) = TAG_NUM(rank))

// TODO need a more descriptive crash and a "reranking" phase when an overflow
// is detected instead of just crashing (if possible)
#define ovf_set_rank(x, rank) (rank ^ MAX_RANK) ? set_rank(x, rank) : exit(8);
#define dec_alloc_rank() (alloc_rank ^ MIN_RANK) ? alloc_rank-- : exit(8);

// Returns the index of `cfr`'s mirror field of the FIRST field that contains a
// reference to `x`, should only be used to get a reference to `cfr`'s successor
// when `cfr` is known to have at least one reference to `x`
#define get_mirror_index(x, cfr) ((get_field(cfr,0) == x) ? 0 : (get_field(cfr,1) == x) ? 1 : 2)

#define next_cofriend(x, cfr) (get_m_field(cfr, get_mirror_index(x, cfr)))

#define is_collectable(x) (!is_root(x) && !is_protected(x))
#define is_root(x) (x == pc || x == stack || x == FALSE)

// FIXME add FALSE 
#define is_immortal(x) (x == TRUE || x == NIL)


// TODO need to document this section, the comments are the same as the ones
// for the no parent field version

void remove_node(obj x);

#define is_parent(x, p) (PAR(x) == p)
#define get_parent(x) PAR(x)
#define set_parent(x,p,i) get_parent(x) = p


// Note that these can't be used to protect a program's root because `DEC_POP`
// will unprotect them if they're passed as an argument to a primitive. This is
// not a big deal for Ribbit since the roots are known (and there's very few of
// them) but a different mechanism (or another bit should be used) in another
// system to avoid this problem
#define is_protected(o) (IS_RIB(o) && IS_MARKED(RANK(o)))

#define _adUpt(x) ((CFR(x) == _NULL) ? 0 : adUpt(x))
#define remove_root(old_root) if (IS_RIB(old_root) && !_adUpt(old_root)) remove_node(old_root)

/* #define protect(o) RANK(o) = MARK(RANK(o)) */
/* #define unprotect(o)                                                           \ */
/*   do {                                                                         \ */
/*     if (IS_RIB(o) && is_protected(o)) {                                             \ */
/*       RANK(o) = UNMARK(RANK(o));                                               \ */
/*       remove_root(o);                                                          \ */
/*     }                                                                          \ */
/*   } while (0) */

void protect(obj o) {
  if (IS_RIB(o)) {
    if (!IS_MARKED(RANK(o))) {
      RANK(o) = MARK(RANK(o));
      get_parent(o) = TAG_NUM(1);
    }
    else{
      get_parent(o) = TAG_NUM(NUM(get_parent(o)) + 1);
    }
  }
}

bool adUpt(obj x); // needed by unprotect

void unprotect(obj o) {
  if (IS_RIB(o)) {
    if (IS_MARKED(RANK(o))) {
      int new_count = NUM(get_parent(o)) - 1;
      if (new_count == 0) {
        RANK(o) = UNMARK(RANK(o));
        get_parent(o) = _NULL;
        remove_root(o);
      }
      else{
        get_parent(o) = TAG_NUM(new_count);
      }
    }
    else{
      printf("Error, calling unprotect on unprotected node");

    }
  }
}

void add_cofriend(obj x, obj cfr, int i) {
  // Case 1: `x` is parentless, `cfr` becomes the parent by default. This
  // corresponds to the situation where a newly allocated object or structure
  // gets connected to an existing spanning tree in the spanning forest
  if (CFR(x) == _NULL) {
    CFR(x) = cfr;
    // Important: adding a new reference to a parentless root should be treated
    // the same as simply adding a new co-friend even if the referrer is the
    // root's only co-friend, in practice this means that this operation should
    // have no impact on the root's rank (see the paper for a counter-example
    // where a cycle is created and an unsafe adoption occurs because of that)
    if (is_collectable(x)) {
      remove_root(x);
      //get_parent(x) = cfr;
      //ovf_set_rank(x, get_rank(cfr)+1);
    }
    return;
  }
  // Case 2: `cfr` already has a reference to `x` and so the list of co-friends
  // is already valid, only need to add a reference to `cfr` from `x`'s i'th
  // mirror field
  for (int j = 0; j < 3; j++) {
    if (j == i) continue; 
    if (get_field(cfr, j) == x) {
      get_m_field(cfr, i) = get_m_field(cfr, j);
      return;
    }
  }
  get_m_field(cfr, i) = CFR(x);
  CFR(x) = cfr;
}


void remove_cofriend(obj x, obj cfr, int i) {
  // Should be called "remove_cfr_reference" or something because `cfr` is
  // removed from `x`'s list of co-friend iff `cfr` only has one ref to `x`

  // Case 1: `cfr` has more than one reference to `x`, the i'th mirror field
  // should be set to _NULL but `cfr` is not removed from `x`'s co-friends
  for (int j = 0; j < 3; j++) {
    if (j == i) continue;
    if (get_field(cfr, j) == x) {
      get_m_field(cfr, i) = _NULL;
      return;
    }
  }
  if (CFR(x) == cfr) {
    CFR(x) = get_m_field(cfr, i);
    get_m_field(cfr, i) = _NULL;
    return;
  }
  // Case 2: `cfr` only has one reference to `x` and so must be removed from
  // `x`'s co-friends
  obj curr = CFR(x); // assumes `cfr` is not the parent
  obj prev;
  while (curr != cfr && curr != _NULL) { // find `cfr` and his predecessor
    prev = curr;
    curr = next_cofriend(x, curr);
  }
  obj tmp = get_m_field(cfr, i); // `cfr`'s successor, could be _NULL
  get_m_field(cfr, i) = _NULL; // remove ref from `cfr` to old successor
  for (int j = 0; j < 3; j++) { // link predecessor with `cfr`'s old successor
    if (get_field(prev, j) == x) {
      get_m_field(prev, j) = tmp;
    }
  }
}

#ifdef COLLECT_QUEUE

void wipe_cofriend(obj x, obj cfr, int i) {
  // TODO cleanup
  obj curr = CFR(x); // assumes `cfr` is not the parent
  obj prev = _NULL;
  obj next;
  while (curr != _NULL) { // find `cfr` and his predecessor
    next = next_cofriend(x, curr);
    if (get_rank(curr) == FALLING_RIB_RANK || get_rank(curr) == UNALLOCATED_RIB_RANK) {
      if (prev == _NULL) {
        CFR(x) = next_cofriend(x, curr);
        curr = CFR(x);
        continue;
      }
      for (int j = 0; j < 3; j++) { // link predecessor with `cfr`'s old successor
        if (get_field(prev, j) == x) {
          get_m_field(prev, j) = next;
        }
      }
      curr = next;
      continue;
    }
    prev = curr;
    curr = next;
  }
}

#else

void wipe_cofriend(obj x, obj cfr, int i) {
  // Same as above but fully removes the co-friend regardless of the number of
  // references from `cfr` to `x`
  if (CFR(x) == cfr) {
    CFR(x) = get_m_field(cfr, i);
    return;
  }
  obj curr = CFR(x); // assumes `cfr` is not the parent
  obj prev;
  while (curr != cfr && curr != _NULL) { // find `cfr` and his predecessor
    prev = curr;
    curr = next_cofriend(x, curr);
  }  
  obj tmp = get_m_field(cfr, i); // `cfr`'s successor, could be _NULL
  for (int j = 0; j < 3; j++) { // link predecessor with `cfr`'s old successor
    if (get_field(prev, j) == x) {
      get_m_field(prev, j) = tmp;
    }
  }
}

#endif

void remove_parent(obj x, obj p, int i) {
  // Should be called "remove_parent_reference" or something because the
  // parent `p` is removed iff `p` only has one reference to `x`
  
  // Case 1: The parent `p` has more than one reference to the child `x`,
  // the i'th mirror field is set to _NULL but we don't remove the parent
  for (int j = 0; j < 3; j++) {
    if (j == i) continue;
    if (get_field(p, j) == x) {
      get_m_field(p, i) = _NULL;
      return;
    }
  }
  // Case 2: The parent `p` only has one reference to the child `x`, `p` must
  // be removed from the list of co-friends. Because the next co-friend
  // temporarily becomes `x`'s new parent (i.e. the spanning tree encoded by
  // the parent/child relationship is restructured), this operation should be
  // considered "unsafe" and so this function should only be called before a
  // drop phase (like in `remove_edge`)
#ifdef COLLECT_QUEUE
  // FIXME double check for multiple references
  remove_cofriend(x, p, i);
#else
  wipe_cofriend(x, p, i);
#endif
  get_parent(x) = _NULL;
  get_m_field(p, i) = _NULL;
}

#define wipe_parent(x,p,i) wipe_cofriend(x,p,i); get_parent(x) = _NULL



//------------------------------------------------------------------------------

// Edge addition (i.e. add a reference to a rib)

// The entire logic for handling new references was moved to `add_cofriend`
// including the dirty check because it's much more efficient this way (we
// set `from` as the parent of `to` right away if the edge is dirty)
#define add_edge(from, to, i) add_cofriend(to, from, i)

// Assumes `from` is a rib and we ignore self-references (and refs to TRUE/NIL)
#define add_ref(from, to, i) if (IS_RIB(to) && !is_immortal(to) && from != to) add_edge(from, to, i)

//------------------------------------------------------------------------------

// Edge deletion (i.e. removing a reference to a rib)


#define fall(x) set_rank(x, FALLING_RIB_RANK)
#define is_falling(x) (get_rank(x) == FALLING_RIB_RANK)

#define deallocate(x) set_rank(x, UNALLOCATED_RIB_RANK)
#define is_deallocated(x) (get_rank(x) == UNALLOCATED_RIB_RANK)

//#define loosen(x) fall(x); pq_remove(x)

bool adopt(obj x) {
  // Take the first co-friend with a rank lower than `x` and make it `x`'s
  // parent (doesn't matter which one we pick if we don't set the rank of
  // `x` during adoption)
  num rank = get_rank(x);
  obj cfr = CFR(x);
  while (cfr != _NULL) {
    // FIXME need to make sure that the topological order is respected to
    // adopt with a cfr of the same rank as the parent
    if (!is_falling(cfr) && get_rank(cfr) < rank) {
      set_parent(x, cfr, get_mirror_index(x, cfr)-3);
      // ovf_set_rank(x, get_rank(cfr)+1); // not sure FIXME
      return 1;
    }
    cfr = next_cofriend(x, cfr);
  }
  return 0;
}

bool upward_adopt(obj from, obj to, int d) {
  if (from == _NULL) return false;
  if (from == to) return false;
  if (is_falling(from)) return false;
  if (is_protected(from)) return false;

  if (is_root(from) || upward_adopt(get_parent(from), to, d)) {
    set_rank(from, get_rank(from)-d);
    return true;
  }
  return false;
}

bool adUpt(obj x) {
  // adoption with the possibility of an upward adoption for mutations
  if (adopt(x)) return 1;
  obj cfr = CFR(x);
  
  // any way to merge this with the previous adoption loop?
  while (cfr != _NULL) {
    if (upward_adopt(cfr, x, get_rank(cfr)-get_rank(x)+1)) {
      get_parent(x) = cfr;
      return 1;
    }
    cfr = next_cofriend(x, cfr);
  }
  return 0;
}

// FIXME
// #define close_enough(ref) true
/* #define close_enough(ref) (get_rank(ref) - get_rank(stack) < MAX_RANK) */


void drop() {
  obj x; // current "falling" rib
  obj *_x;
  obj cfr;

  long adUpt_tries = 0;

  while (!Q_IS_EMPTY()) {
    x = q_dequeue();
    _x = RIB(x)->fields;
    cfr = CFR(x);
    
    // making x's children "fall" along with him
    for (int i = 0; i < 3; i++) {
      obj child = _x[i];
      if (IS_RIB(child) && is_parent(child, x) && is_collectable(child)) {
        if (!is_falling(child) && 
#ifdef NO_ADUPT
            !adopt(child)
#else
#ifdef ALWAYS_ADUPT
            !adUpt(child)
#else
            !(adUpt_tries++ < MAX_ADUPT_TRIES ? adUpt(child) : adopt(child))
#endif
#endif
        )
        {
          // if we loosen here instead of when we dequeue, we can reuse the
          // queue field for the priority queue
          fall(child); 
          pq_remove(child);
          q_enqueue(child);
        }
      }
    }
    // identify x's co-friends that could be potential "catchers"
    while (cfr != _NULL) {
      if (!is_falling(cfr)) { // potential anchor?
        pq_enqueue(cfr);
      }
      cfr = next_cofriend(x, cfr);
    }
  }
}

void catch() {
  // since we use a priority queue instead of a set for the anchors,
  // we can re-use it (as is) for the catch queue...
  do {
    obj anchor = pq_dequeue();
#ifdef QUEUE_NO_REMOVE
    // When using the "no remove" version of a data structure, the catch queue
    // could empty itself during the the pq_dequeue procedure, need to add an
    // additional check here
    if (anchor == _NULL) return;
#endif
    obj *_anchor = RIB(anchor)->fields;
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(_anchor[i]) && is_falling(_anchor[i])) {
        set_parent(_anchor[i], anchor, i);
        ovf_set_rank(_anchor[i], get_rank(anchor)+1);
        pq_enqueue(_anchor[i]); // add rescued node to potential "catchers"
      }
    }
  } while (!PQ_IS_EMPTY());
}

void dealloc_rib(obj x){
#ifndef REF_COUNT
  deallocate(x); // deallocated rib, this way we just ignore cycles
#endif
  obj *_x = RIB(x)->fields;
  for (int i = 0; i < 3; i++) {
    if (IS_RIB(_x[i])) {
      if (is_parent(_x[i], x)) {
        if (is_deallocated(_x[i])) { // already deallocated?
          continue;
        } else if (is_falling(_x[i])) { // falling?
          dealloc_rib(_x[i]);
        } else { // child is a root or protected
          // Parent field will be set to _NULL, no ambiguity with 1st cofriend
          if (get_parent(_x[i]) != _NULL && CFR(_x[i]) != _NULL 
#ifdef COLLECT_QUEUE
              // FIXME won't work fast enough if we remove from FALSE 
              && PQ_NEXT(_x[i]) == _NULL && pq_tail != _x[i] && _x[i] != FALSE 
#endif
              ) {
            wipe_parent(_x[i], x, i);
#ifdef COLLECT_QUEUE
            pq_enqueue(_x[i]);
#endif
          }
        }
      } else { // not a child, only need to remove x from co-friend's list
        // TODO faster way to check if we try to wipe the same co-friend twice
        if (!is_immortal(_x[i]) && !is_falling(_x[i]) && CFR(_x[i]) != _NULL
#ifdef COLLECT_QUEUE
            // FIXME won't work fast enough if we remove from FALSE
            && PQ_NEXT(_x[i]) == _NULL && pq_tail != _x[i] && _x[i] != FALSE
#endif
            ) {
#ifdef COLLECT_QUEUE
          wipe_cofriend(_x[i], x, i);
          pq_enqueue(_x[i]);
#else
          if (i == 0) {
            wipe_cofriend(_x[i], x, i);
          } else if (i == 1 && _x[1] != _x[0]) {
            wipe_cofriend(_x[i], x, i);
          } else if (i == 2 && _x[2] != _x[0] && _x[2] != _x[1]) {
            wipe_cofriend(_x[i], x, i);
          }
#endif
        }
      }
    }
  }
  CAR(x) = (obj)alloc; // deallocate the rib by adding it to the freelist
  alloc = (obj *)x;
  _x[6] = _NULL;
  get_parent(x) = _NULL;
  //_x[RIB_NB_FIELDS-1] = _NULL;
  
#ifdef CLEAN_RIBS
  for (int i = 1; i < RIB_NB_FIELDS; i++) {
    if (i == 7) continue; // don't set rank to _NULL
    _x[i] = _NULL;
  }
#endif
}
  
void remove_edge(obj from, obj to, int i) {
  // `from` and `to` are assumed to be ribs, `i` is the index where `to` is
  // expected to be found (there could be more than one reference from `from`
  // to `to` and we need to handle the right mirror field)

  // Case 1: `from` is not `to`'s parent, and so the spanning tree remains the
  // same, we only need to remove that reference (and potentially `from` from
  // `to`'s co-friends if there was only one reference)
  if (!is_parent(to, from)) {
    remove_cofriend(to, from, i);
    return;
  }
  // Case 2: `from` is `to`'s parent, if there's only one reference from `from`
  // to `to` (i.e. if the parent is removed), the spanning tree becomes
  // disconnected and so a drop phase must ensue UNLESS `to` is protected or
  // if `to` can be adopted right away
  remove_parent(to, from, i); 
  if (is_collectable(to) && !is_parent(to, from) && !_adUpt(to)) {
    q_enqueue(to);
    fall(to); 
    drop();
    if (!PQ_IS_EMPTY()) catch(); 
    if (is_falling(to)) {
      dealloc_rib(to);
#ifdef COLLECT_QUEUE
      pq_wipe();
#endif
    }
  }
}

// FIXME should we just assume that `from` is a rib to avoid the type check?
#define remove_ref(from, to, i) if (IS_RIB(to) && !is_immortal(to) && from != to) remove_edge(from, to, i)

// FIXME need to generalize this so that it works with any protected field
#define remove_ref_nr(to, i) remove_edge(null_rib, to, i); TEMP5 = _NULL;


void remove_node(obj old_root) {
  q_enqueue(old_root);
  fall(old_root);
  drop();
  if (!PQ_IS_EMPTY()) catch();
  if (is_falling(old_root)) {
    dealloc_rib(old_root);
#ifdef COLLECT_QUEUE
    pq_wipe();
#endif
  }
}


//------------------------------------------------------------------------------

// Write barriers


void set_field(obj src, int i, obj dest) { // write barrier
  // The order differs a bit from the ref count version of the write barrier...
  // TODO explain why we're doing that this way
  
  obj *ref = RIB(src)->fields;

  if (ref[i] == dest) return; // src's i-th field already points to dest
  
  if (IS_RIB(ref[i])) { // no need to dereference _NULL or a num
    if (is_collectable(ref[i]) && is_parent(ref[i], src) && next_cofriend(ref[i], src) == _NULL) {
      // We need to be more careful here since simply removing the edge
      // between src and ref[i] will deallocate ref[i] and potentially
      // some other ribs refered by ref[i]. This is problematic if dest
      // contains a reference to one of ref[i]'s children (or more) since
      // we'll deallocate a rib (or more) that shouldn't be deallocated.
      // We can get around that by using a temporary rib to point to ref[i]
      
      obj tmp = ref[i];
      protect(tmp);
      remove_ref(src, ref[i], i); // new dest
      ref[i] = dest;
      add_ref(src, dest, i);
      unprotect(tmp);
      return;
    }
    remove_ref(src, ref[i], i);
  }
  ref[i] = dest;
  add_ref(src, dest, i);
}

// FIXME assume `src` will always be a rib?

#define SET_CAR(src, dest) if (IS_RIB(src)) set_field(src, 0, dest)
#define SET_CDR(src, dest) if (IS_RIB(src)) set_field(src, 1, dest)
#define SET_TAG(src, dest) if (IS_RIB(src)) set_field(src, 2, dest)

#define set_sym_tbl(new_st)                                                     \
  obj old_st = symbol_table;                                                    \
  symbol_table = new_st;                                                        \
  remove_root(old_st)

void set_stack(obj new_stack) {
  // TODO make set_stack a macro as well (conflict with _pop)
  obj old_stack = stack;
  stack = new_stack;
  if (stack != NUM_0){
    get_parent(stack) = _NULL;
  }
  remove_root(old_stack);
}

#define set_pc(new_pc)                                                          \
  obj old_pc = pc;                                                              \
  pc = new_pc;                                                                  \
  if (pc != NUM_0){                                                             \
    get_parent(pc) = _NULL;                                                       \
  }                                                                            \
  remove_root(old_pc);

#else

//==============================================================================

// Reference counting

// Write barries

void inc_count(obj o) {
  // assumes o is a rib
  obj *ptr = RIB(o)->fields;
  num count = NUM(ptr[3])+1;
  ptr[3] = TAG_NUM(count);
}

void dec_count(obj o) {
  // assumes o is a rib
  obj *ptr = RIB(o)->fields;
  num count = NUM(ptr[3])-1;
  
  if (count < 1) {
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(ptr[i])) {
        dec_count(ptr[i]);
      }
    }
    ptr[0] = (obj)alloc; // reference to next availabe slot in memory
    alloc = &ptr[0];
    
    // FIXME Simplifies the generated graph but redundant with rib allocation
    ptr[1] = _NULL;
    ptr[2] = _NULL;
    ptr[3] = TAG_NUM(0);
  }
  else {
    ptr[3] = TAG_NUM(count);
  }
}

// only increment count of ribs, not nums
#define INC_COUNT(o) if (IS_RIB(o)) inc_count(o)
#define DEC_COUNT(o) if (IS_RIB(o)) dec_count(o)


void set_field(obj src, int i, obj dest) {
  obj *p_src = RIB(src)->fields;
  // always increase before decreasing 
  INC_COUNT(dest);
  DEC_COUNT(p_src[i]);
  p_src[i] = dest;
}

#define SET_CAR(src, dest) set_field(src, 0, dest)
#define SET_CDR(src, dest) set_field(src, 1, dest)
#define SET_TAG(src, dest) set_field(src, 2, dest)

void set_sym_tbl(obj new_sym_tbl) {
  obj old_sym_tbl = symbol_table;
  symbol_table = new_sym_tbl;
  INC_COUNT(symbol_table);
  DEC_COUNT(old_sym_tbl);
}

void set_stack(obj new_stack) {
  obj old_stack = stack;
  stack = new_stack;
  INC_COUNT(stack);
  DEC_COUNT(old_stack);
}

void set_pc(obj new_pc) {
  obj old_pc = pc;
  pc = new_pc;
  INC_COUNT(pc);
  DEC_COUNT(old_pc);
}

#endif

// The basic mark-and-sweep GC is used mainly for collecting cyclic garbage
// not reclaimed when using ref count but we also use if with the incremental
// collector to see if any garbage was left uncollected when running the tests

void mark(obj *o) { // Recursive version of marking phase
  if (IS_RIB(*o)) {
    obj *ptr = RIB(*o)->fields;
    if (!IS_MARKED(ptr[2])) { 
      obj tmp = ptr[2]; 
      ptr[2] = MARK(ptr[2]);
      mark(&ptr[0]);
      mark(&ptr[1]);
      mark(&tmp);
    }
  }
}

void gc() {
  int leftovers = 0;
  int wrongly_collected = 0;
  //printf("\t--GC called\n");
  // Mark (only 3 possible roots)
  mark(&stack);
  mark(&pc);
  mark(&FALSE);
  // Sweep
  scan=heap_bot;
  while (scan != heap_top) {
    obj tag = *(scan+2);
    if (IS_MARKED(tag)) {
      *(scan+2) = UNMARK(tag);
      if (get_rank((obj)scan) == UNALLOCATED_RIB_RANK) {
        wrongly_collected++;
      }
    } else {
#ifdef REF_COUNT
      if (RIB((obj)scan)->fields[3] != 0) leftovers++;
#else
      if (get_rank((obj)scan) != UNALLOCATED_RIB_RANK) leftovers++;
#endif
      *scan = (obj)alloc;
      alloc = scan;
    }
    scan += RIB_NB_FIELDS; // next rib object
  }
  printf("***REMAINING_RIBS = %d\n", leftovers);
  printf("***ALIVE_BUT_COLLECTED = %d\n", wrongly_collected);
#ifdef REF_COUNT
  if (*alloc == _NULL){
    printf("Heap is full\n");
    exit(1);
  }
#else
  if (((obj)alloc) == null_rib) {
    printf("Heap is full\n");
    exit(1);
  }
#endif
}


//==============================================================================

// RVM


// Stack and heap management

#ifndef REF_COUNT

obj pop() {
  obj tos = CAR(stack);
  if (IS_RIB(tos) && M_CAR(stack) == _NULL) {
    // protect TOS only if it gets deallocated otherwise
    protect(tos);
  }
  set_stack(CDR(stack));
  return tos;
}

// to avoid too many preprocessor instructions in the RVM code
#define DEC_POP(o) if (IS_RIB(o) && is_protected(o)) unprotect(o)

#define _protect(var) if (IS_RIB(var) && M_CAR(stack) == _NULL) protect(var)
#define _unprotect(var) if (IS_RIB(var) && is_protected(var)) unprotect(var)

#define _pop(var)                                                               \
  obj var = CAR(stack);                                                         \
  _protect(var);                                                                \
  set_stack(CDR(stack))

#define PRIM1() _pop(x)
#define PRIM2() _pop(y); PRIM1()
#define PRIM3() _pop(z); PRIM2()

#define DEC_PRIM1() _unprotect(x);
#define DEC_PRIM2() _unprotect(y); DEC_PRIM1()
#define DEC_PRIM3() _unprotect(z); DEC_PRIM2()

void push2(obj car, obj tag) {
  obj tmp = *alloc; // next available slot in freelist
  
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;        // field 1
  *alloc++ = stack;      // field 2
  *alloc++ = tag;        // field 3
  *alloc++ = _NULL;      // mirror 1
  *alloc++ = _NULL;      // mirror 2
  *alloc++ = _NULL;      // mirror 3
  *alloc++ = _NULL;      // co-friends
#ifdef GLOBAL_RANK_COUNTER
  *alloc++ = TAG_NUM(alloc_rank); 
#else
  *alloc++ = TAG_NUM(0); 
#endif
  *alloc++ = _NULL;      // queue and priority queue
#ifdef QUEUE_NO_REMOVE
  *alloc++ = _NULL;
#endif
  *alloc++ = _NULL;
#ifdef DEBUG_FIELD
  *alloc++ = _NULL; // debug field
#endif

  obj new_rib = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));
  obj old_stack = stack;
  stack = new_rib;

  // new stack becomes the parent of old stack (avoids the potential drop)
  if (IS_RIB(old_stack)) {
    get_m_field(new_rib, 1) = CFR(old_stack);
    CFR(old_stack) = stack;
    get_parent(old_stack) = stack;

    // ovf_set_rank(new_rib, get_rank(old_stack)-1);
    remove_root(old_stack);
    // set_rank(old_stack, alloc_rank+1);
  }
  dec_alloc_rank();

  add_ref(new_rib, car, 0);
  add_ref(new_rib, tag, 2);
  
  alloc = (obj *)tmp;
}

// We don't need to link a newly allocated rib from the stack since we
// don't trigger a GC cycle when allocating a new rib: since we deallocate
// objects instantly when they're no longer needed (even if they're part of
// a cycle) then the number of live objects at all time in the program
// corresponds to the maximum number of objects allowed by the program.

rib *alloc_rib(obj car, obj cdr, obj tag) {
  // allocates a rib without protecting it from the GC

  obj tmp = *alloc; // next available slot in freelist
  
  *alloc++ = car;        // field 1
  *alloc++ = cdr;        // field 2
  *alloc++ = tag;        // field 3
  *alloc++ = _NULL;      // mirror 1
  *alloc++ = _NULL;      // mirror 2
  *alloc++ = _NULL;      // mirror 3
  *alloc++ = _NULL;      // co-friends
#ifdef GLOBAL_RANK_COUNTER
  *alloc++ = TAG_NUM(alloc_rank); 
#else
  *alloc++ = TAG_NUM(0); 
#endif

  *alloc++ = _NULL;      // queue and priority queue
#ifdef QUEUE_NO_REMOVE
  *alloc++ = _NULL;
#endif
  *alloc++ = _NULL;
#ifdef DEBUG_FIELD
  *alloc++ = _NULL; // debug field
#endif

  dec_alloc_rank();

  obj new_rib =  TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));

  add_ref(new_rib, car, 0);
  add_ref(new_rib, cdr, 1);
  add_ref(new_rib, tag, 2);

  alloc = (obj *)tmp;
  
  return RIB(new_rib);
}

#define alloc_rib2(car, cdr, tag) alloc_rib(car, cdr, tag)

#else

// The count of CAR(stack) must be increased before setting the stack to
// CDR(stack) to make sure we don't dealloc the popped object. This also means
// that the caller of pop must decrease the count when the object is no longer
// needed... this is pretty tedious but that's how it is for now
obj pop() {
  obj x = CAR(stack);
  INC_COUNT(x);
  set_stack(CDR(stack));
  return x;
}

// to avoid too many preprocessor instructions in the RVM code
#define DEC_POP(o) DEC_COUNT(o)

#define PRIM1() obj x = pop()
#define PRIM2() obj y = pop(); PRIM1()
#define PRIM3() obj z = pop(); PRIM2()

#define DEC_PRIM1() DEC_COUNT(x)
#define DEC_PRIM2() DEC_COUNT(y); DEC_PRIM1()
#define DEC_PRIM3() DEC_COUNT(z); DEC_PRIM2()

void push2(obj car, obj tag) {
  obj tmp = *alloc; // next available slot in freelist
  
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;
  *alloc++ = stack;
  *alloc++ = tag;
  *alloc++ = TAG_NUM(1); // ref count of 1 cos pointed by stack
  alloc += (RIB_NB_FIELDS-4);
  
  stack = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));
  alloc = (obj *)tmp;
  
  // no need to increase ref count of stack because it remains unchanged
  // only difference is that the ref comes from the rib pointed by instead
  // of the stack pointer itself
  INC_COUNT(car);
  INC_COUNT(tag);
  
  if (!IS_RIB(tmp) || *alloc == _NULL) { // empty freelist?
    gc();
  }
}

rib *alloc_rib(obj car, obj cdr, obj tag) {
  push2(car, cdr); // tag is set
  
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = TAG(allocated);
  TAG(allocated) = tag;

  INC_COUNT(tag);

  stack = old_stack;

  return RIB(allocated);
}

rib *alloc_rib2(obj car, obj cdr, obj tag) {
  push2(car, tag);
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = cdr;

  INC_COUNT(cdr);

  stack = old_stack;

  return RIB(allocated);
}

#endif


//------------------------------------------------------------------------------

// Program execution

#define vm_exit(code)                                                          \
  do {                                                                         \
    exit((code));                                                              \
  } while (0)

rib *list_tail(rib *lst, num i) {
  return (i == 0) ? lst : list_tail(RIB(lst->fields[1]), i - 1);
}

obj get_opnd(obj o) {
  return (IS_NUM(o) ? RIB(list_tail(RIB(stack), NUM(o))) : RIB(o))->fields[0];
}

obj get_cont() {
  obj s = stack;
  while (!NUM(TAG(s))) {
    s = CDR(s);
  }
  return s;
}

// @@(feature scm2str
char* scm2str(obj s) { // FIXME not tested
    int length = (int) NUM(CDR(s)); 
    obj current = CAR(s);
    char* str = malloc(length + 1);
    for (int i = 0; i < length; i++) {
        str[i] = (char) NUM(CAR(current));
        current = CDR(current);
    }

    str[length] = '\0';

    return str;
};
// )@@

// @@(feature str2scm
obj str2scm(char* s) { // FIXME not tested
    obj chrs = NIL;
    int len = 0;
    int i = 0;

    while (s[len++]); // calculate length
    len--; // remove \0 at the end
    
    // Construct list by the tail
    i = len;
    while (i--) 
      chrs = TAG_RIB(alloc_rib(TAG_NUM(s[i]), chrs, PAIR_TAG));

    return TAG_RIB(alloc_rib(chrs, TAG_NUM(len), STRING_TAG));
}
// )@@

// @@(feature list2scm (use str2scm)
// Warning, this implementation assumes string inside of the list
obj list2scm(char **s, int length) { // FIXME not tested
    obj list = NIL;
    for (int i = length - 1; i >= 0; i--)
        list = TAG_RIB(alloc_rib(str2scm(s[i]), list, PAIR_TAG));
    
    return list;
};
// )@@

// @@(feature bool2scm
obj bool2scm(bool x) { return x ? TRUE : FALSE; }
// )@@


// Primitive procedures
obj prim(int no) {
  switch (no) { 
  // @@(primitives (gen "case " index ":" body)
  case 0: // @@(primitive (##rib a b c)
  {
#ifdef REF_COUNT
    PRIM3();
    obj new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
    CAR(new_rib) = x;
    CDR(new_rib) = y;
    TAG(new_rib) = z;
    push2(new_rib, PAIR_TAG);
    DEC_COUNT(new_rib); // remove redundant new_rib count
    DEC_PRIM3();
#else
    // No need to protect the 3 arguments since they'll  be reachable
    // from the newly allocated rib
    obj z = CAR(stack);
    obj y = CAR(CDR(stack));
    obj x = CAR(CDR(CDR(stack)));
    obj r = TAG_RIB(alloc_rib(x, y, z));
    set_stack(CDR(CDR(CDR(stack))));
    push2(r, PAIR_TAG);
#endif
    break;
  } // )@@
  case 1: // @@(primitive (##id x)
  {
    PRIM1();
    push2(x, PAIR_TAG);
    DEC_PRIM1();
    break;
  } // )@@
  case 2: // @@(primitive (##arg1 x y)
  {
    set_stack(CDR(stack)); // pop without using the argument
    break;
  } // )@@
  case 3: // @@(primitive (##arg2 x y)
  {
    PRIM2();
    push2(y, PAIR_TAG);
    DEC_PRIM2();
    break;
  } //)@@
  case 4: // @@(primitive (##close rib)
  {
    obj x = CAR(TOS); // code
    obj y = CDR(stack); // stack (env)
    obj closure = TAG_RIB(alloc_rib(x, y, CLOSURE_TAG));
    // x and y count increased in push2 and the newly allocated rib as
    // well but need to decrease the count of the initial TOS
    SET_CAR(stack, closure);
#ifdef REF_COUNT
    DEC_COUNT(closure);
    DEC_COUNT(CAR(closure));
#endif
    break;
  } //)@@
  case 5: // @@(primitive (##rib? rib) (use bool2scm)
  {
    /* PRIM1(); */
    /* push2(bool2scm(IS_RIB(x)), PAIR_TAG); */
    /* DEC_PRIM1(); */

    // No need to protect the 2 ribs on the TOS since we only want the
    // result of an operation, saves some GC time
    obj res = bool2scm(IS_RIB(CAR(stack)));
    set_stack(CDR(stack));
    push2(res, PAIR_TAG);
    break;
  } //)@@
  case 6: // @@(primitive (##field0 rib)
  {
    PRIM1();
    push2(CAR(x), PAIR_TAG);
    DEC_PRIM1();
    break;
  } //)@@
  case 7: // @@(primitive (##field1 rib)
  {
    PRIM1();
    push2(CDR(x), PAIR_TAG);
    DEC_PRIM1();
    break;
  } //)@@
  case 8:  // @@(primitive (##field2 rib)
  {
    PRIM1();
    push2(TAG(x), PAIR_TAG);
    DEC_PRIM1();
    break;
  } //)@@
  case 9: // @@(primitive (##field0-set! rib x)
  { 
    PRIM2();
    SET_CAR(x, y);
    push2(y, PAIR_TAG);
#ifdef REF_COUNT
    DEC_COUNT(y);
#endif
    DEC_PRIM2();
    break;
  } //)@@
  case 10:  // @@(primitive (##field1-set! rib x)
  {
    PRIM2();
    SET_CDR(x, y);
    push2(y, PAIR_TAG);
#ifdef REF_COUNT
    DEC_COUNT(y);
#endif
    DEC_PRIM2();
    break;
  } //)@@
  case 11:  // @@(primitive (##field2-set! rib x)
  {
    PRIM2();
    SET_TAG(x, y);
    push2(y, PAIR_TAG);
#ifdef REF_COUNT
    DEC_COUNT(y);
#endif
    DEC_PRIM2();
    break;
  } // )@@
  case 12:  // @@(primitive (##eqv? rib1 rib2) (use bool2scm)
  {
    /* PRIM2(); */
    /* push2(bool2scm(x == y), PAIR_TAG); */
    /* DEC_PRIM2(); */

    // No need to protect the 2 ribs on the TOS since we only want the
    // result of an operation, saves some GC time
    obj res = bool2scm(CAR(stack) == CAR(CDR(stack)));
    set_stack(CDR(CDR(stack)));
    push2(res, PAIR_TAG);
    break;
  } //)@@
  case 13:  // @@(primitive (##< x y) (use bool2scm)
  {
    PRIM2();
    push2(bool2scm(NUM(x) < NUM(y)), PAIR_TAG);
    DEC_PRIM2();
    break;
  } //)@@
  case 14:  // @@(primitive (##+ x y)
  {
    PRIM2();
    push2(x + y - 1, PAIR_TAG);
    DEC_PRIM2();
    break;
  } //)@@
  case 15:  // @@(primitive (##- x y)
  {
    PRIM2();
    push2(x - y + 1, PAIR_TAG);
    DEC_PRIM2();
    break;
  } //)@@
  case 16:  // @@(primitive (##* x y)
  {
    PRIM2();
    push2(TAG_NUM((NUM(x) * NUM(y))), PAIR_TAG);
    DEC_PRIM2();
    break;
  } // )@@
  case 17:  // @@(primitive (##quotient x y)
  {
    PRIM2();
    push2(TAG_NUM((NUM(x) / NUM(y))), PAIR_TAG);
    DEC_PRIM2();
    break;
  } // )@@
  case 18:  // @@(primitive (##getchar)
  {
    int read;
    read = getchar();
    if (EOF == read) read = -1;
    push2(TAG_NUM(read), PAIR_TAG);
    break;
  } // )@@
  case 19:  // @@(primitive (##putchar c)
  {
    PRIM1();
    putchar((char)NUM(x));
    fflush(stdout);
    push2(x, PAIR_TAG);
    DEC_PRIM1();
    break;
  } // )@@
  case 20:  // @@(primitive (##exit n)
  {
    PRIM1();
    vm_exit(NUM(x));
    DEC_PRIM1();
    break;
  } // )@@
  // )@@
  default: {
    vm_exit(EXIT_ILLEGAL_INSTR);
  }
  }
  return TAG_NUM(0);
}

#define ADVANCE_PC() set_pc(TAG(pc))

#ifdef IRUN_COUNTER
int irun = 0;
#endif

void run() { // evaluator
  dprint("Entering run\n");
  while (1) {
#ifdef IRUN_COUNTER
    irun++;
#endif


#ifdef DPRINT
    if(irun == -1) { raise(SIGINT); }
    dprint("Running... %d\n", irun);
#endif
#ifdef CHECK_SPANNING_TREE
    if (irun < 50000 || irun % 5000 == 0){
      check_spanning_tree();
    }
#endif
    num instr = NUM(CAR(pc));
    
    // gc();
    
    // @@(feature es-apply
    if (IS_RIB(TEMP4)) {
      remove_edge(NIL, TEMP4, 1);
      TEMP4 = _NULL;
    }
    // )@@
    
    switch (instr) {
    case INSTR_AP: // call or jump
    {
      bool jump = (TAG(pc) == NUM_0);
      obj proc = get_opnd(CDR(pc));
      while (1) {
        
        if (IS_NUM(CAR(proc))) { // calling a primitive procedure
          // @@(feature (and arity-check (not prim-no-arity))
          set_stack(CDR(stack)); // pop without using the argument
          // )@@
          proc = prim(NUM(CAR(proc)));
          if (IS_RIB(proc)) { // non-tail call?
            continue;
          }
          if (jump) { // tail call
            set_pc(get_cont());
            SET_CDR(stack, CAR(pc));
          }
          ADVANCE_PC();
        }

        else { // calling a lambda
          num nargs = NUM(pop()); // @@(feature arity-check)@@
          obj new_stack = TAG_RIB(alloc_rib(NUM_0, proc, PAIR_TAG));
          proc = CDR(new_stack);
          SET_CDR(new_stack, CDR(proc)); // @@(feature flat-closure)@@
#ifdef REF_COUNT
          // No need to save the procedure for ES
          SET_CAR(pc, CAR(proc)); // save the proc from the mighty gc
#endif
          num nparams_vari = NUM(CAR(CAR(proc)));
          num nparams = nparams_vari >> 1;
          // @@(feature arity-check
          num vari = nparams_vari&1;
          if (vari ? nparams > nargs : nparams != nargs) {
            printf("*** Unexpected number of arguments nargs: %ld nparams: %ld vari: %ld\n", nargs, nparams, vari);
            exit(1);
          }
          // )@@

          // Optimization: we don't update the stack until the end of the two
          // loops below and thus avoid updating the subgraph rooted at the
          // stack nparams time which is completely unnecessary
          obj s = stack;
          
          // @@(feature rest-param (use arity-check)
          nargs-=nparams;
          if (vari){
            obj rest = NIL;
            for(int i = 0; i < nargs; ++i){
#ifdef REF_COUNT
              rest = TAG_RIB(alloc_rib(pop(), rest, PAIR_TAG));
              DEC_COUNT(CDR(rest)); // old rest
              DEC_POP(CAR(rest));
#else
              rest = TAG_RIB(alloc_rib(CAR(s), rest, PAIR_TAG));
              s = CDR(s);
#endif
            }
            new_stack = TAG_RIB(alloc_rib(rest, new_stack, PAIR_TAG));
#ifdef REF_COUNT
            DEC_COUNT(CAR(new_stack)); // rest
            DEC_COUNT(CDR(new_stack)); // old new stack
#endif
          }
          // )@@
          for (int i = 0; i < nparams; ++i) {
#ifdef REF_COUNT
            new_stack = TAG_RIB(alloc_rib(pop(), new_stack, PAIR_TAG));
            DEC_COUNT(CDR(new_stack)); // old new stack
            DEC_POP(CAR(new_stack));
#else
            new_stack = TAG_RIB(alloc_rib(CAR(s), new_stack, PAIR_TAG));
            s = CDR(s);
#endif
          }
#ifndef REF_COUNT
          if (s != stack) { set_stack(s); }
#endif
          
          nparams = nparams + vari; // @@(feature arity-check)@@
          obj new_cont = TAG_RIB(list_tail(RIB(new_stack), nparams));
          if (jump) {
            obj k = get_cont();
            SET_CAR(new_cont, CAR(k)); // CAR(new_cont) = CAR(k);
            SET_TAG(new_cont, TAG(k)); // TAG(new_cont) = TAG(k);
#ifdef REF_COUNT
            DEC_COUNT(k);
#endif
          } else {
            SET_CAR(new_cont, stack); // CAR(new_cont) = stack;
#ifdef REF_COUNT
            DEC_COUNT(stack);
#endif
            SET_TAG(new_cont, TAG(pc)); // TAG(new_cont) = TAG(pc);
          }
#ifdef REF_COUNT
          stack = new_stack;
          obj _new_pc = CAR(pc); // proc entry point
          SET_CAR(pc, TAG_NUM(instr));
          set_pc(TAG(_new_pc));
#else
          set_pc(TAG(CAR(proc)));
          set_stack(new_stack);
#endif
        }
        break;
      }
      break;
    }
    case INSTR_SET: { // set
      obj x = CAR(stack);
      if (IS_NUM(CDR(pc))) {
        SET_CAR(TAG_RIB(list_tail(RIB(stack), NUM(CDR(pc)))), x);
      } else {
        SET_CAR(CDR(pc), x);
      }
      set_stack(CDR(stack)); // stack = CDR(stack);
      ADVANCE_PC();
      break;
    }
    case INSTR_GET: { // get
      push2(get_opnd(CDR(pc)), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_CONST: { // const
      push2(CDR(pc), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_IF: { // if
      // obj p = pop();
      set_pc((CAR(stack) != FALSE) ? CDR(pc) : TAG(pc));
      // DEC_POP(p);
      set_stack(CDR(stack));
      break;
    }
    case INSTR_HALT: { // halt
      gc();
      vm_exit(0);
    }
    default: { // error
      vm_exit(EXIT_ILLEGAL_INSTR);
    }
    }
  }
}

//------------------------------------------------------------------------------

// Program initialization

void init_heap() {
  heap_start = malloc(sizeof(obj) * SPACE_SZ); // (SPACE_SZ+1));
  if (!heap_start) {
    vm_exit(EXIT_NO_MEMORY);
  }
  // initialize freelist
  scan = heap_top;
#ifdef REF_COUNT
  *scan = _NULL;
#else
  null_rib = TAG_RIB((rib *)(scan));
  scan -= RIB_NB_FIELDS; // skip the null rib
  // rank should always be 0 since popped values will be saved there temporarly
  set_rank(null_rib, 0);
#endif
  while (scan != heap_bot) {
    set_rank(scan, UNALLOCATED_RIB_RANK);
    alloc = scan; // alloc <- address of previous slot
    scan -= RIB_NB_FIELDS; // scan <- address of next rib slot
    *scan = (obj)alloc; // CAR(next rib) <- address of previous slot
  }
  alloc = scan;
  stack = NUM_0;
}

#define INIT_FALSE()                                                           \
  FALSE = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),   \
                            TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),   \
                            SINGLETON_TAG));                                   \
  TRUE = CAR(FALSE);                                                           \
  NIL = CDR(FALSE)

// Build symbol table

unsigned char get_byte() { return input[pos++]; }

#define ENCODING_SIZE (00) // @@(replace "00" encoding/ribn-base)@@
#define HALF_ENCODING_SIZE ENCODING_SIZE/2

#if ENCODING_SIZE==92
#define GET_CODE get_code
num get_code() {
  num x = get_byte() - 35;
  return x < 0 ? 57 : x;
}
#else
#define GET_CODE get_byte
#endif

num get_int(num n) {
  num x = GET_CODE();
  n *= HALF_ENCODING_SIZE;
  return x < HALF_ENCODING_SIZE ? n + x : get_int(n + x - HALF_ENCODING_SIZE);
}

obj lst_length(obj list) {
  size_t l = 0;
  while (IS_RIB(list) && NUM(TAG(list)) == 0) {
    ++l;
    list = CDR(list);
  }
  return TAG_NUM(l);
}

rib *create_sym(obj name) {
  rib *list = alloc_rib(name, lst_length(name), STRING_TAG);
  rib *sym = alloc_rib(FALSE, TAG_RIB(list), SYMBOL_TAG);
#ifdef REF_COUNT
  DEC_COUNT(CDR(sym)); // redundant count
#endif
  rib *root = alloc_rib(TAG_RIB(sym), symbol_table, PAIR_TAG);
#ifdef REF_COUNT
  DEC_COUNT(CAR(root)); // redundant count
  DEC_COUNT(CDR(root)); // redundant count
#endif
  return root;
}

void build_sym_table() {
  num n = get_int(0);
  while (n > 0) {
    n--;
    set_sym_tbl(TAG_RIB(create_sym(NIL))); // symbol_table = TAG_RIB(create_sym(NIL));
  }
  obj accum = NIL;
  while (1) {
    byte c = get_byte();
    if (c == 44) {
      set_sym_tbl(TAG_RIB(create_sym(accum))); // symbol_table = TAG_RIB(create_sym(accum));
#ifdef REF_COUNT
    DEC_COUNT(accum);
#endif
      accum = NIL;
      continue;
    }
    if (c == 59)
      break;
    accum = TAG_RIB(alloc_rib(TAG_NUM(c), TAG_RIB(accum), PAIR_TAG));
#ifdef REF_COUNT
    DEC_COUNT(CDR(accum));
#endif
  }
  set_sym_tbl(TAG_RIB(create_sym(accum))); // symbol_table = TAG_RIB(create_sym(accum));
#ifdef REF_COUNT
  DEC_COUNT(accum);
#endif
}


// Decode instruction graph

obj list_ref(rib *lst, num i) { return list_tail(lst, i)->fields[0]; }

rib *symbol_ref(num n) { return RIB(list_ref(RIB(symbol_table), n)); }

rib *inst_tail(rib *lst, num i){
  return (i == 0) ? lst : inst_tail(RIB(lst->fields[2]), i - 1);
}

#ifdef DPRINT
int idecode = 0;
#endif
// @@(feature encoding/optimal
void decode() {
  dprint("Entering decoding...\n");
  // @@(replace "{1,2,3}" (list->host encoding/optimal/start "{" "," "}")
  int weights[] = {1,2,3};
  // )@@
  obj n;
  int d;
  int op;
  int i;
  while (1) {
#ifdef DPRINT
    if(irun == -1) { raise(SIGINT); }
    dprint("Decoding... %d\n", idecode++);
#endif
    check_spanning_tree();
    num x = GET_CODE();
    n = x;
    op = -1;
    while((d=weights[++op])<=n) n-=d;
    if (op < 4) push2(NUM_0, NUM_0); // JUMP
    if (op < 24) n = op % 2> 0 ? get_int(n) : n; 
    if (op < 20) {
      i = (op / 4) - 1;
      i = i < 0 ? 0 : i;
      n = !(op & 0b10)  ? TAG_NUM(n) : TAG_RIB(symbol_ref(n));
#ifdef REF_COUNT
      INC_COUNT(n);
#endif
    }
    else if (op < 22) {
      obj r = TAG_RIB(alloc_rib2(TAG_NUM(n), NUM_0, pop()));
#ifndef REF_COUNT
      DEC_POP(TAG(r));
#endif
      n = TAG_RIB(alloc_rib(r, NIL, CLOSURE_TAG));
#ifdef REF_COUNT
      DEC_COUNT(TAG(r));
      DEC_COUNT(r);
#endif
      i = 3;
      if (stack == NUM_0) {
        break;
      }
    }
    else if (op < 24){
      obj tmp = TAG_RIB(inst_tail(RIB(TOS), n));
      push2(tmp, NUM_0);
#ifdef REF_COUNT
      DEC_COUNT(tmp);
#endif
      continue;
    }
    else if (op < 25){
      n = pop();
      i=4;
    }
    obj c = TAG_RIB(alloc_rib(TAG_NUM(i), n, NUM_0));
    SET_TAG(c, TOS); //  c->fields[2] = TOS;
    SET_CAR(stack, c); // TOS = TAG_RIB(c);
    // SET_CAR(stack, TAG_RIB(alloc_rib(TAG_NUM(i), n, TOS))); // TOS = TAG_RIB(c);
#ifdef REF_COUNT
    DEC_COUNT(CDR(c)); // n
    DEC_COUNT(TOS); // c
#else
    DEC_POP(n);
#endif 
  }
  set_pc(TAG(CAR(n)));
#ifdef REF_COUNT
  DEC_COUNT(n);
#else
  if (IS_RIB(n)) remove_root(n);
#endif
}
// )@@

// @@(feature encoding/original
void decode() {
  
  // TODO adapt original encoding to ref count and ES, original is faster
  
  int weights[6] = {20, 30, 0, 10, 11, 4};
  obj n;
  int d;
  int op;
  while (1) {
    num x = GET_CODE();
    n = x;
    op = -1;
    while (n > 2 + (d = weights[++op])) {
      n -= d + 3;
    }
    if (x > 90) {
      op = INSTR_IF;
      n = pop();
    } else {
      if (!op) {
        push2(NUM_0, NUM_0);
      }
      if (n >= d) {
        n = (n == d) ? TAG_NUM(get_int(0))
                     : TAG_RIB(symbol_ref(get_int(n - d - 1)));
      } else {
        n = (op < 3) ? TAG_RIB(symbol_ref(n)) : TAG_NUM(n);
      }
      if (op > 4) {
        n = TAG_RIB(
            alloc_rib(TAG_RIB(alloc_rib2(n, NUM_0, pop())), NIL, CLOSURE_TAG));
        if (stack == NUM_0) {
          break;
        }
        op = INSTR_CONST;
      } else if (op > 0) {
        op--;
      } else {
        op = 0;
      }
    }
    rib *c = alloc_rib(TAG_NUM(op), n, 0);
    c->fields[2] = TOS;
    TOS = TAG_RIB(c);
  }
  pc = TAG(CAR(n));
}
// )@@


// init global variables and stack

void set_global(obj c) {
  SET_CAR(CAR(symbol_table), c);
  set_sym_tbl(CDR(symbol_table)); // symbol_table = CDR(symbol_table);
}

// initialize primitive 0, FALSE, TRUE, and NIL
#ifdef REF_COUNT
#define INIT_GLOBAL()                                                          \
  obj tmp = TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG));              \
  DEC_COUNT(CDR(tmp));                                                         \
  set_global(tmp);                                                             \
  DEC_COUNT(tmp);                                                              \
  set_global(FALSE);                                                           \
  set_global(TRUE);                                                            \
  set_global(NIL)
#else
#define INIT_GLOBAL()                                                          \
  obj tmp = TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG));              \
  set_global(tmp);                                                             \
  set_global(FALSE);                                                           \
  set_global(TRUE);                                                            \
  set_global(NIL)
#endif

void init_stack() {
  push2(NUM_0, PAIR_TAG);
  push2(NUM_0, PAIR_TAG); 
  
  obj first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;
/* #ifndef REF_COUNT */
/*   add_ref(stack, first, 2); */
/* #endif */

  CAR(first) = TAG_NUM(INSTR_HALT);
  CDR(first) = NUM_0; 
  TAG(first) = PAIR_TAG;
}

void init() {
  init_heap();
#ifndef REF_COUNT
  Q_INIT();
  PQ_INIT();
#endif
  INIT_FALSE(); // don't really care about the ref count of FALSE, TRUE, and NIL
  build_sym_table();
  decode();
  INIT_GLOBAL();
  init_stack();
  run();
}




#ifdef VIZ

FILE* current_graph;

FILE* viz_start_graph(char* name){
  // open a file with the name "name" and write the header
  FILE* file = fopen(name, "w");
  fprintf(file, "digraph G {\n");
  return file;
}

void viz_end_graph(FILE* graph){
  // close the file
  fprintf(graph, "}\n");
  fflush(graph);
  fclose(graph);
}

void viz_add_edge(FILE* graph, obj from, obj to){
  // write the edge from "from" to "to"
  fprintf(graph, "%ld -> %ld\n", from, to);
}

void viz_add_dot_edge(FILE* graph, obj from, obj to){
  // write the edge from "from" to "to"
  fprintf(graph, "%ld -> %ld [style=dotted]\n ", from, to);
}
void viz_add_dot_edge_red(FILE* graph, obj from, obj to){
  // write the edge from "from" to "to"
  fprintf(graph, "%ld -> %ld [style=dotted, color=red]\n ", from, to);
}

void viz_add_node(FILE* graph, obj node, long rank, bool error){
  // write the node "node"
  char* color = error ? "red" : "black";
  char* label = "";
  if (node == stack) label = " stack";
  if (node == pc) label = " pc";
  if (node == FALSE) label = " FALSE";
  if (node == TRUE) label = " TRUE";
  if (node == NIL) label = " NIL";
  fprintf(graph, "%ld [label=\"%ld %s\", color=%s]\n", node, rank,label, color);
}

int _viz_show_tree(FILE* graph, obj root, int version){

  // write the tree rooted at "root"
  if (IS_RIB(root)) {
    rib* r = RIB(root);

    if(r->debug == version) {
      printf("A cycle was found in the spanning tree!\n");
      if (graph==NULL){
        return false;
      }
    }
    r->debug = version;

    bool error = r->p_field != _NULL && get_rank(PAR(r)) > get_rank(r);
    if(error){
      printf("An invalid rank was found in the spanning tree\n");
      if (graph==NULL){
        return false;
      }
    }

    if (graph!=NULL) viz_add_node(graph, root, NUM(r->rank), error);
    for (int i = 0; i < 3; i++){
      if (IS_RIB(r->fields[i]) && is_parent(r->fields[i], root)) {

        // Check duplicate
        bool is_duplicate = false;
        for (int j = 0; j < i; j++){
          if (IS_RIB(r->fields[j]) && r->fields[j] == r->fields[i]){
            is_duplicate = true;
          }
        }
        if (is_duplicate) continue;

        if (graph!=NULL) {
          viz_add_edge(graph, root, r->fields[i]);
        }
        if(!_viz_show_tree(graph, r->fields[i], version)) return false;
      }
    }
  }
  return true;
}

int viz_version = 0;

// A bit confusing, but if name is NULL, the function will only test
// if the graph is valid without writing to it.
int viz_show_tree(char* name, obj root){
  viz_version++;
  if (name != NULL){
    FILE* graph = viz_start_graph(name);
    bool x = _viz_show_tree(graph, root, TAG_NUM(viz_version));
    viz_end_graph(graph);
    return x;
  }
  return _viz_show_tree(NULL, root, TAG_NUM(viz_version));
}


void viz_add_rib_label(FILE* graph, obj rib, obj car, obj cdr, obj tag, obj m_car, obj m_cdr, obj m_tag, obj p, obj rank){
  // write the value of the rib
  char* car_prefix = IS_RIB(car) ? "r" : "";
  char* cdr_prefix = IS_RIB(cdr) ? "r" : "";
  char* tag_prefix = IS_RIB(tag) ? "r" : "";
  char* m_car_prefix = IS_RIB(m_car) ? "r" : "";
  char* m_cdr_prefix = IS_RIB(m_cdr) ? "r" : "";
  char* m_tag_prefix = IS_RIB(m_tag) ? "r" : "";
  char* p_prefix = IS_RIB(tag) ? "r" : "";
  long rib_value = rib - ((long)heap_start);
  long car_value = IS_RIB(car) ? car-((long)heap_start) : NUM(car);
  long cdr_value = IS_RIB(cdr) ? cdr-((long)heap_start) : NUM(cdr);
  long tag_value = IS_RIB(tag) ? tag-((long)heap_start) : NUM(tag);
  long m_car_value = IS_RIB(m_car) ? m_car-((long)heap_start) : NUM(m_car);
  long m_cdr_value = IS_RIB(m_cdr) ? m_cdr-((long)heap_start) : NUM(m_cdr);
  long m_tag_value = IS_RIB(m_tag) ? m_tag-((long)heap_start) : NUM(m_tag);
  long p_value = IS_RIB(p) ? p-((long)heap_start) : NUM(p);
  long rank_value = NUM(rank);
  fprintf(
      graph,
      "%ld [label=\"%ld : [%s%ld,%s%ld,%s%ld] M[%s%ld,%s%ld,%s%ld] CO[%s%ld] -- %ld\"]\n",
      rib,
      rib_value,
      car_prefix, car_value,
      cdr_prefix, cdr_value,
      tag_prefix, tag_value,
      m_car_prefix, m_car_value,
      m_cdr_prefix, m_cdr_value,
      m_tag_prefix, m_tag_value,
      p_prefix, p_value,
      rank_value);
}


void viz_heap(char* name){
  // to check manually if the tests are working properly
  // current_graph = viz_start_graph("graph.dot");
  current_graph = viz_start_graph(name);
  scan=heap_top;
  
  for (int i = 0; i <= MAX_NB_OBJS; i++) {
    obj rank = get_rank(scan);

    // skip unallocated ribs
    if (rank == UNALLOCATED_RIB_RANK) {
      scan-=RIB_NB_FIELDS;
      continue;
    }

    if (IS_RIB(scan[0])) viz_add_edge(current_graph, ((obj)scan), scan[0]);
    if (IS_RIB(scan[1])) viz_add_edge(current_graph, ((obj)scan), scan[1]);
    if (IS_RIB(scan[2])) viz_add_edge(current_graph, ((obj)scan), scan[2]);
    if (IS_RIB(scan[3])) viz_add_dot_edge(current_graph, ((obj)scan), scan[3]);
    if (IS_RIB(scan[4])) viz_add_dot_edge(current_graph, ((obj)scan), scan[4]);
    if (IS_RIB(scan[5])) viz_add_dot_edge(current_graph, ((obj)scan), scan[5]);
    if (IS_RIB(scan[6])) viz_add_dot_edge_red(current_graph, ((obj)scan), scan[6]);
    // viz_add_rib_label(current_graph, scan, scan[0], scan[1], scan[2], rank);
    if (((obj)scan) == stack) {
      viz_add_rib_label(current_graph, ((obj)scan), scan[0], scan[1], scan[2], scan[3], scan[4], scan[5], scan[6], TAG_NUM(-33));
    } else if (((obj)scan) == pc) {
      viz_add_rib_label(current_graph, ((obj)scan), scan[0], scan[1], scan[2], scan[3], scan[4], scan[5], scan[6], TAG_NUM(-444));
    } else if (((obj)scan) == FALSE || ((obj)scan) == TRUE || ((obj)scan) == NIL) {
      viz_add_rib_label(current_graph, ((obj)scan), scan[0], scan[1], scan[2], scan[3], scan[4], scan[5], scan[6], TAG_NUM(-5555));
    } else if (((obj)scan) == symbol_table) {
      viz_add_rib_label(current_graph, ((obj)scan), scan[0], scan[1], scan[2], scan[3], scan[4], scan[5], scan[6], TAG_NUM(-66666));
    } else {
      viz_add_rib_label(current_graph, ((obj)scan), scan[0], scan[1], scan[2], scan[3], scan[4], scan[5], scan[6], TAG_NUM(rank));
    }
    scan-=RIB_NB_FIELDS;
  }
  viz_end_graph(current_graph);
  // exit(1);
}

#endif

#ifdef CHECK_SPANNING_TREE

void check_spanning_tree_impl(){
  if(!viz_show_tree(NULL, pc)){
    viz_show_tree("after.dot", pc);
    raise(SIGINT);
  }

  if(!viz_show_tree(NULL, stack)){
    viz_show_tree("after.dot", stack);
    raise(SIGINT);
  }

  if(!viz_show_tree(NULL, FALSE)){
    viz_show_tree("after.dot", FALSE);
    raise(SIGINT);
  }
}

#endif

#ifdef DPRINT

num drank(obj o){
  return NUM(RIB(o)->rank);
}

obj df(obj o, int i){
  return RIB(o)->fields[i];
}

obj dref(obj o){
  return RIB(o)->refs;
}

obj dpar(obj o){
  return RIB(o)->p_field;
}

rib* r(obj o){
  return RIB(o);
}

#endif
 
int main() { init(); }
