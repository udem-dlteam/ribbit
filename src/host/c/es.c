/* 
 * File: es.c
 *
 * Author: Frédéric Lahaie-Bertrand
 *
 * Implementation of the Ribbit Virtual Machine in C with an incremental 
 * garbage collector inspired by Even-Shiloach trees.
 *
 * This version of the RVM doesn't include all the features defined in the
 * original version (DEBUG, NO_STD, lzss compression, ARG_V, clang support, 
 * stop & copy GC, DEFAULT_REPL_MIN, NOSTART, CHECK_ACCESS, etc.) but can be
 *  used with the R4RS library.
 */

/* TODO (Even-Shiloach)
 * --------------------
 *
 * Bugs
 *  - Deriv bug
 *  - Cleanup features, it's a mess
 *
 *  - [Not a priority, happens rarely] co-friend not found in remove_cofriend
 *  - FIXMEs and TODOs in the code
 *  - Call a GC for every instruction to make sure everything is collected...
 *     => Do that for the bootstrap, the test suite, and fuzzy tests
 *
 * Features
 *  - [Not a priority] Finalizers
 *  - [Not a priority] Add missing features from the original RVM (encoding, 
 *    sys primitives, (DEBUG, NO_STD, lzss compression, ARG_V, clang support, 
 *    stop & copy GC, DEFAULT_REPL_MIN, NOSTART, CHECK_ACCESS, etc.) and then
 *    make it the official RVM for the C host (could have a version that also
 *    contains the ES algorithm for the paper as well...)
 *  - [Not a priority] ... cleanup the code
 *
 * Optimizations
 *  - Tagging (including roots, some ribs are not collected)
 *  - Allocation with predetermined rank
 *  - Negative ranks (for make-list)
 *  - Rank spacing 
 *  - Adoption (optimized and integrated in the co-friend traversal phase)
 *  - Allow for adoption with co-friend of same rank without leaking cycles
 *    (in other words, find a way to keep the ranks stricly increasing)
 *  - Remove PC as a root and flat closures?
 *  - Find a way to remove co-friends more efficiently
 *  - ...?
 */

/* TODO (Reference counting)
 * -------------------------
 * Bugs
 *  - Make sure that all (non-cyclic) ribs are collected, haven't fully tested 
 *
 * Features
 *  - Adapt original compression to RC
 *  - String, chars, and pair features
 *  - Primitives: apply, io, and sys
 *  - Missing features from the original rvm 
 *  - ... cleanup the code
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

// @@(feature update-ranks
#define UPDATE_RANKS
// )@@

// @@(feature es-roots
// FIXME causes a memory leak if used with adoption
#define ES_ROOTS
#define NO_ADOPT
// )@@

// @@(feature no-adopt
#define NO_ADOPT
// )@@

// @@(feature tagging
#define TAGGING
// )@@

// @@(feature debug/rib-viz
#define VIZ
void viz_heap(char* name);
// )@@

#define UNALLOCATED_RIB_RANK 999999
// @@(feature debug/clean-ribs
#define CLEAN_RIBS
#define UNALLOCATED_RIB_RANK 999999
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


// Default data structure used for the anchors/catcher is a queue with a remove
// operation that requires no additional field (than the drop queue).

// Fastest seems to be the queue with no remove so far but...
//   - Requires an extra field which annoys me
//   - Doesn't rebalance the tree (or we can't guarantee that it will rebalance
//     itself even partially) during the catch phase
//   - I only tested with `fib 25`, the only program know to mankind 

// @@(feature queue-no-remove
// Queue with the remove in dequeue, faster but requires one extra field
#define QUEUE_NO_REMOVE
// )@@

// @@(feature linked-list
#define LINKED_LIST
// )@@

// @@(feature linked-list-no-remove
#define LINKED_LIST_NO_REMOVE
// )@@

// @@(feature buckets
#define BUCKETS
// )@@

// @@(feature buckets-no-remove
#define BUCKETS_NO_REMOVE
// )@@

#ifdef REF_COUNT
#define RIB_NB_FIELDS 4
#else
#ifdef BUCKETS_NO_REMOVE
#define RIB_NB_FIELDS 11
#else
#if defined(QUEUE_NO_REMOVE) || defined(LINKED_LIST_NO_REMOVE) || defined(BUCKETS)
#define RIB_NB_FIELDS 10
#else
// Queue with remove or priority queue (singly linked list) with remove
#define RIB_NB_FIELDS 9
#endif
#endif
#endif

typedef struct {
  obj fields[RIB_NB_FIELDS];
} rib;

#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]

#ifndef REF_COUNT
// mirror fields
#define M_CAR(x) RIB(x)->fields[3]
#define M_CDR(x) RIB(x)->fields[4]
#define M_TAG(x) RIB(x)->fields[5]
 // co-friends
#define CFR(x) RIB(x)->fields[6]
#define RANK(x) RIB(x)->fields[7]
// queue next
#define Q_NEXT(x) RIB(x)->fields[8]

#ifdef BUCKETS_NO_REMOVE
#define PQ_NEXT_RIB(x) RIB(x)->fields[9]
#define PQ_NEXT_BKT(x) RIB(x)->fields[10]
#else
#ifdef BUCKETS
#define PQ_NEXT_RIB(x) RIB(x)->fields[8]
#define PQ_NEXT_BKT(x) RIB(x)->fields[9]
#else
#ifdef LINKED_LIST_NO_REMOVE
#define PQ_NEXT(x) RIB(x)->fields[9]
#else
#ifdef LINKED_LIST
#define PQ_NEXT(x) Q_NEXT(x)
#else
#ifdef QUEUE_NO_REMOVE
#define PQ_NEXT(x) RIB(x)->fields[9]
#else
// Default: queue with remove
#define PQ_NEXT(x) Q_NEXT(x)
#endif
#endif
#endif
#endif
#endif
#endif

// FIXME try to be a bit more consistant with the notation...
#define get_field(x,i) RIB(x)->fields[i]

// #define UNTAG(x) ((x) >> 1)
#define UNTAG(x) ((x) >> 2)
#define NUM(x) ((num)(UNTAG((num)(x))))
#define RIB(x) ((rib *)(x))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x) && x != _NULL)
// #define TAG_NUM(num) ((((obj)(num)) << 1) | 1)
#define TAG_NUM(num) ((((obj)(num)) << 2) | 1)
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))

#define MARK(x) ((x)|2) // assumes x is a tagged obj (ul)
#define UNMARK(x) ((x)^2)
#define IS_MARKED(x) ((x)&2)

#define is_protected(o, i) (IS_RIB(o) && IS_MARKED(get_field(o,i)))

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

#define TRUE (CAR(FALSE))
#define NIL (CDR(FALSE))

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

// keep track of number of deallocated objects for debugging
int d_count = 0;

rib *heap_start;
#define MAX_NB_OBJS 1000000
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_top (heap_bot + (SPACE_SZ))

//==============================================================================

// Debugging

// TODO
// print_cfr
// print_queue
// print_pqueue


#ifndef REF_COUNT

//==============================================================================

// Data structures (ES Trees)


// Queue

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

// Priority queue

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


#ifdef BUCKETS_NO_REMOVE

// TODO

#else

#ifdef BUCKETS

// FIXME not sure if it's working

void pq_enqueue(obj o) {
  if (IS_RIB(o)) {
    if (PQ_IS_EMPTY()){
      PQ_NEXT_RIB(o) = _NULL;
      PQ_NEXT_BKT(o) = _NULL;
      pq_head = o;
      return;
    }
    int r = NUM(RANK(o));
    obj curr_bkt = pq_head;
    obj next_bkt = PQ_NEXT_BKT(pq_head);
    
    // curr_bkt <= o <= next_bkt
    while (next_bkt != _NULL && NUM(RANK(curr_bkt)) < r) {
      curr_bkt = next_bkt;
      next_bkt = PQ_NEXT_BKT(next_bkt);
    }
    // Note: only the first rib in the bucket keeps a reference
    // to the first rib in the next bucket
    if (NUM(RANK(curr_bkt)) == r) { // insertion in current bucket
      PQ_NEXT_BKT(curr_bkt) = _NULL;
      PQ_NEXT_RIB(o) = curr_bkt;
      PQ_NEXT_BKT(o) = next_bkt;
    } else if (next_bkt == _NULL) { // new bucket with highest rank
      PQ_NEXT_BKT(curr_bkt) = o;
      PQ_NEXT_RIB(o) = _NULL;
      PQ_NEXT_BKT(o) = _NULL;
    } else if (NUM(RANK(next_bkt)) == r) { // insertion in next bucket
      PQ_NEXT_BKT(curr_bkt) = o;
      PQ_NEXT_RIB(o) = next_bkt;
      PQ_NEXT_BKT(o) = PQ_NEXT_BKT(next_bkt);
      PQ_NEXT_BKT(next_bkt) = _NULL;
    } else { // insertion in a new bucket between curr and next buckets
      PQ_NEXT_BKT(curr_bkt) = o;
      PQ_NEXT_RIB(o) = _NULL;
      PQ_NEXT_BKT(o) = next_bkt;
    }
  }
}

obj pq_dequeue() {
  if (PQ_IS_EMPTY()){
    return _NULL;
  }
  obj tmp = pq_head;
  if (PQ_NEXT_RIB(pq_head) == _NULL) {
    pq_head = PQ_NEXT_BKT(pq_head); // could be _NULL
  } else {
    pq_head = PQ_NEXT_RIB(pq_head);
  }
  PQ_NEXT_RIB(tmp) = _NULL;
  PQ_NEXT_BKT(tmp) = _NULL;
  return tmp;
}

// Set operations

void pq_remove(obj o) {
  if (IS_RIB(o)) {
    if (pq_head == _NULL) {
      return;
    }
    else if (pq_head == o) {
      // dequeue but we don't return the rib
      obj tmp = pq_head;
      if (PQ_NEXT_RIB(pq_head) == _NULL) {
        pq_head = PQ_NEXT_BKT(pq_head); // could be _NULL
      } else {
        pq_head = PQ_NEXT_RIB(pq_head);
      }
      PQ_NEXT_RIB(tmp) = _NULL;
      PQ_NEXT_BKT(tmp) = _NULL;
    }
    else {
      int r = NUM(RANK(o));
      obj curr = pq_head;
      obj prev_bkt;
      while (curr != _NULL && NUM(RANK(curr)) < r) { // find bucket
        prev_bkt = curr;
        curr = PQ_NEXT_BKT(curr);
      }
      if (curr == _NULL || NUM(RANK(curr)) > r) {
        return; // no bucket of rank r;
      }
      // find rib in bucket
      if (curr == o) { // first rib in the bucket
        if (PQ_NEXT_RIB(curr) == _NULL) {
          PQ_NEXT_BKT(prev_bkt) = PQ_NEXT_BKT(curr);
        } else {
          PQ_NEXT_BKT(prev_bkt) = PQ_NEXT_RIB(curr);
        }
        PQ_NEXT_RIB(curr) = _NULL;
        PQ_NEXT_BKT(curr) = _NULL;
      }
    }
  }
}

#else

#ifdef LINKED_LIST_NO_REMOVE

// TODO

#else

#ifdef LINKED_LIST

// Priority queue implemented with a singly linked list and a remove
// operation (requires no extra field)

void pq_enqueue(obj o) {
  // the lower the rank, the closer the rib is to pq_head
  if (PQ_NEXT(o) == _NULL && pq_head != o && pq_tail != o) {
    if (PQ_IS_EMPTY()){
      pq_head = o;
      pq_tail = o;
    }
    else if (RANK(o) == 1) { // root (tagged rank)
      PQ_NEXT(o) = pq_head;
      pq_head = o;
    }
    else {
      // insert new rib after the first rib that has a lower rank
      obj prev = pq_head;
      obj curr = PQ_NEXT(pq_head);
      while (curr != _NULL && RANK(curr) < RANK(o)){
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
  if ((PQ_NEXT(o) == _NULL && pq_head != o && pq_tail != o) || pq_head == _NULL) { // o not in set?
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
    
    // FIXME not sure why this still happens...
    if (curr == _NULL) return;

    if (curr == pq_tail) {
      pq_tail = prev;
    }
    PQ_NEXT(prev) = PQ_NEXT(curr);
    PQ_NEXT(curr) = _NULL;
    PQ_NEXT(o) = _NULL;
  }
}

#else

#ifdef QUEUE_NO_REMOVE

// Queue with no remove, requires an extra field since a falling rib could still
// be in the anchor queue when we enqueue it in the drop queue

void pq_enqueue(obj o) {
  // In the case of the pqueue, we could attempt to add the
  // same rib twice (e.g. if two ribs have the same co-friend)
  if (PQ_NEXT(o) == _NULL && pq_head != o && pq_tail != o) {
    if (PQ_IS_EMPTY()){
      pq_head = o;
    } else {
      PQ_NEXT(pq_tail) = o;
    }
    pq_tail = o;
  }
}

obj pq_dequeue() {
  // Get rid of falling ribs
  obj tmp;
  while (!PQ_IS_EMPTY()) {
    if (NUM(RANK(pq_head)) == -1) { // falling?
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

void pq_enqueue(obj o) {
  // In the case of the pqueue, we could attempt to add the
  // same rib twice (e.g. if two ribs have the same co-friend)
  if (PQ_NEXT(o) == _NULL && pq_head != o && pq_tail != o) {
    if (PQ_IS_EMPTY()){
      pq_head = o;
    } else {
      PQ_NEXT(pq_tail) = o;
    }
    pq_tail = o;
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
  pq_head = PQ_NEXT(tmp);
  PQ_NEXT(tmp) = _NULL;
  return tmp;
}

void pq_remove(obj o) {
  if ((PQ_NEXT(o) == _NULL && pq_head != o && pq_tail != o) || pq_head == _NULL) { // o not in set?
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
    
    // FIXME not sure why this still happens...
    if (curr == _NULL) return;

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
#endif
#endif
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
//
//  - An edge is said to be dirty if rank(to) < rank(from)-1 because rank(to)
//    (i.e. its distance from the root) could be reduced by making `from` the
//    new parent of `to`

#define get_mirror_field(x, cfr) ((get_field(cfr,0) == x) ? 3 : (get_field(cfr,1) == x) ? 4 : 5)

/* int get_mirror_field(obj x, obj cfr) { */
/*   for (int i = 0; i < 3; i++){ */
/*     if (get_field(cfr,i) == x) { // && get_field(cfr,i+3) != cfr) { */
/*       return i+3; */
/*     } */
/*   } */
/*   return -1; // cfr is not x's co-friend */
/* } */

#define get_rank(x) (NUM(RANK(x)))
#define set_rank(x, rank) (RANK(x) = TAG_NUM(rank))

#ifdef TAGGING
#define is_root(x) (x == pc || x == stack || x == FALSE || is_protected(x, 7))
#else
#define is_root(x) (x == pc || x == stack || x == FALSE)
#endif

#define is_dirty(from, to) (get_rank(from) < (get_rank(to) - 1))

#define next_cofriend(x, cfr) (get_field(cfr, get_mirror_field(x, cfr)))

#define is_parent(x, p) (CFR(x) == p)
#define get_parent(x) CFR(x)


void set_parent(obj child, obj new_parent, int k) {
  if (child == new_parent) return; // ignore self-references
  obj old_parent = get_parent(child);
  if (old_parent == _NULL || old_parent == new_parent) { // parentless child
    CFR(child) = new_parent;
    return;
  }
  obj prev;
  obj curr = old_parent;
  while (curr != new_parent && curr != _NULL) {
    prev = curr;
    curr = next_cofriend(child, curr);
  }
  if (curr == _NULL) { // new_parent was not a co-friend (guaranteed to have only one ref)
    get_field(new_parent, k + 3) = old_parent; // make new parent point to old parent
    CFR(child) = new_parent; // new parent becomes... the new parent
    return;
  }

  obj next = next_cofriend(child, curr); // successor of curr (next will have to point to next)
  obj tmp = get_parent(child); // old parent

  for (int i = 0; i < 3; i++){
    if (get_field(curr, i) == child){
      get_field(curr, i+3) = tmp;
    }
  }

  get_parent(child) = curr; // new parent

  // set all mirror field (associated with ref to child in new_parent) to the next co-friend
  for (int i = 0; i < 3; i++){
    if (get_field(prev, i) == child){
      get_field(prev, i+3) = next;
    }
  }
}

void remove_parent(obj x, obj p, int i) {
  // Case 1: The parent `p` has more than one reference to the child `x`,
  // set the mirror field to NULL but don't actually remove the parent
  for (int j = 0; j < 3; j++) {
    if (j == i) continue;
    if (get_field(p, j) == x) {
      get_field(p, i + 3) = _NULL;
      return;
    }
  }
  // Case 2: The parent `p` has only one reference to the child `x`, the
  // parent must be removed from the list of co-friends, the next co-friend
  // is set as the temporary parent (note that its rank might not be minimal)
  get_parent(x) = get_field(p, i + 3); // next co-friend
  get_field(p, i + 3) = _NULL; 
}

void wipe_parent(obj x, obj p, int i) {
  // same as above but fully remove the parent regardless of the number of refs
  get_parent(x) = get_field(p, i + 3); // next co-friend
  get_field(p, i + 3) = _NULL; 
}

void add_cofriend(obj x, obj cfr, int j) {
  // FIXME do we want the co-friends to be ordered by rank? for now the new
  // co-friend is just inserted between the parent and the following co-friend
  obj p = get_parent(x);
  if (p == _NULL) {
    set_parent(x, cfr, j);
    set_rank(x, get_rank(cfr)+1);
    return;
  }

  // Cas 1: No need to update the whole co-friend list as cfr already point
  // to x. We can reuse this field instead and return as the list is already
  // valid
  for (int k = 0; k < 3; k++){
    if (k == j) continue;
    if (get_field(cfr, k) == x){
      get_field(cfr, j+3) = get_field(cfr, k+3);
      return;
    }
  }
  // Other case, cfr is not already a cofriend of x
  // So we need to add it and potentally reupdate all occurences of mirroir field of x in p.
  int i = get_mirror_field(x, p);
  obj tmp = get_field(p,i); // old co-friend pointed by parent

  // Make sure all mirroir field of the parent point to the right
  for (int k = 0; k < 3; k++){
    if (get_field(p, k) == x){
      get_field(p,k+3) = cfr;
    }
  }

  // Set the next cofriend in the list
  get_field(cfr, j+3) = tmp;
}

void remove_cofriend(obj x, obj cfr, int k) {
  // Should be named "remove_link" or something because we don't remove the
  // co-friend if there exist more than one ref from cfr to x

  // If cofriend cfr points to x more than once, just set k's mirror field to
  // NULL
  for (int i = 0; i < 3; i++) {
    if (i == k) continue;
    if (get_field(cfr, i) == x) {
      get_field(cfr, k + 3) = _NULL;
      return;
    }
  }
  // Else we need to find cfr in the list of co-friend and remove it (assumes
  // cfr is not x's parent and that cfr could not be present in the list)
  obj curr = get_parent(x);
  obj prev;
  while (curr != cfr && curr != _NULL) { // get co-friend and his predecessor
    prev = curr;
    curr = next_cofriend(x, curr);
  }
  /* if (curr == _NULL) { */
  /*   // FIXME cfr was not a co-friend of x, not sure if that's a bug */
  /*   /\* if (cfr != null_rib) { *\/ */
  /*   /\*   printf("couldn't find the cofriend when trying to remove\n"); *\/ */
  /*   /\* } *\/ */
  /*   // exit(1); */
  /*   return; */
  /* } */
  // remove reference to next co-friend
  obj tmp = get_field(curr, k+3);
  get_field(curr, k+3) = _NULL;

  // set all mirror field (associated with ref to x in cfr) to the next co-friend
  for (int i = 0; i < 3; i++){
    if (get_field(prev, i) == x){
      get_field(prev, i+3) = tmp;
    }
  }
}

void wipe_cofriend(obj x, obj cfr, int k) {
  // same as above but fully remove the co-friends regardless of the number of refs

  // Else we need to find cfr in the list of co-friend and remove it (assumes
  // cfr is not x's parent and that cfr could not be present in the list)
  obj curr = get_parent(x);
  obj prev;
  while (curr != cfr && curr != _NULL) { // get co-friend and his predecessor
    prev = curr;
    curr = next_cofriend(x, curr);
  }
  if (curr == _NULL) {
    // FIXME cfr was not a co-friend of x, not sure if that's a bug
    /* if (cfr != null_rib) { */
    /*   printf("couldn't find the cofriend when trying to remove\n"); */
    /* } */
    // exit(1);
    return;
  }
  // remove reference to next co-friend
  obj tmp = get_field(curr, k+3);
  get_field(curr, k+3) = _NULL;

  // set all mirror field (associated with ref to x in cfr) to the next co-friend
  for (int i = 0; i < 3; i++){
    if (get_field(prev, i) == x){
      get_field(prev, i+3) = tmp;
    }
  }
}


//------------------------------------------------------------------------------

// Edge addition (i.e. add a reference to a rib)

// Intuition: TODO


#ifdef UPDATE_RANKS

/* void update_ranks(obj root) { */
/*   // FIXME integrate this in add_edge and stop the update as soon as the rank */
/*   // of all children of a rib is clean (or else we just traverse the entire */
/*   // subgraph for no reason) */
/*   q_enqueue(root); */
/*   int r; */
/*   obj curr; */
/*   obj *c; */
/*   do { */
/*     curr = q_dequeue(); */
/*     c = RIB(curr)->fields; */
/*     r = get_rank(curr)+1; */
/*     for (int i = 0; i < 3; i++) { */

/*       // FIXME the `c[i] != root` condition is necessary to sometimes avoid */
/*       // an infinite loop when we update the ranks after setting a new root */
/*       // but not sure if that should also be applied in add_edge? */
/*       if (IS_RIB(c[i]) && c[i] != root && (!is_root(c[i]))) { */

/*         // Only update ranks if the parent's rank was updated or if the edge */
/*         // is dirty */
/*         // FIXME we could avoid A LOT of overhead if we could avoid some updates */
/*         // here without changing how many ribs gets collected, also should note */
/*         // that the algorithm enqueues EVERY friends, which I don't do here to */
/*         // avoid the insane overhead of doing that */
/*         if (is_parent(c[i], curr) && (get_rank(curr) != (get_rank(c[i])+1))) { */
/*           set_rank(c[i], r); */
/*           q_enqueue(c[i]); */
/*         } else if (is_dirty(curr, c[i])) { */
/*           set_parent(c[i], curr, i); */
/*           set_rank(c[i], r); */
/*           q_enqueue(c[i]); */
/*         } */
/*       } */
/*     } */
/*   } while (!Q_IS_EMPTY()); */
/* } */

void update_ranks(obj root) {
  // FIXME integrate this in add_edge and stop the update as soon as the rank
  // of all children of a rib is clean (or else we just traverse the entire
  // subgraph for no reason)
  q_enqueue(root);
  int r;
  obj curr;
  obj *c;
  do {
    curr = q_dequeue();
    c = RIB(curr)->fields;
    r = get_rank(curr)+1;
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(c[i]) && (!is_root(c[i]))) {
        // Only update ranks if the parent's rank was updated or if the edge
        // is dirty
        // FIXME we could avoid A LOT of overhead if we could avoid some updates
        // here without changing how many ribs gets collected, also should note
        // that the algorithm enqueues EVERY friends, which I don't do here to
        // avoid the insane overhead of doing that
        if (is_parent(c[i], curr) && (get_rank(curr) != (get_rank(c[i])+1))) {
          set_rank(c[i], r);
          if (c[i] != root) q_enqueue(c[i]);
        } else if (is_dirty(curr, c[i])) {
          set_parent(c[i], curr, i);
          set_rank(c[i], r);
          if (c[i] != root) q_enqueue(c[i]);
        }
      }
    }
  } while (!Q_IS_EMPTY());
}

#else

#define update_ranks(o) NULL

#endif

void add_edge(obj from, obj to, int i) {
  if (from == to) return; // ignore self-references
  add_cofriend(to, from, i);
  // if (is_dirty(from, to) && !is_dirty(get_parent(to), to)) {
  if (is_dirty(from, to) && get_rank(get_parent(to)) < get_rank(from)) {
    // More likely to have an adoption when the parent/child relationship
    // is kept as dirty as possible (pls don't quote me on that) but not
    // sure if the set_parent is always worth it or if we're better off
    // keeping the same parent when the rank difference is not big enough
    // TODO benchmarks
    set_parent(to, from, i);
  }
  if (is_parent(to, from)) {
    update_ranks(to);
  }
}

// FIXME should we just assume that `from` is a rib to avoid the type check?
#define add_ref(from, to, i) if (IS_RIB(to)) add_edge(from, to, i)

// Only link the null rib if the the popped object would be deallocated
#define add_ref_nr(from, to, i) if (IS_RIB(to) && M_CAR(stack) == _NULL) add_edge(from, to, i)

//------------------------------------------------------------------------------

// Edge deletion (i.e. removing a reference to a rib)

// Intuition: TODO

#ifdef NO_ADOPT

#define adopt(x) false

#define _adopt(x) false

#else

bool adopt(obj x) {
  int rank = get_rank(x);
  obj cfr = get_parent(x);
  while (cfr != _NULL) {
    // Note: because ranks are not strictly increasing (some ranks can be
    // equal, adoption must be done with a rib that has a smaller rank or
    // else some cycles could be left uncollected)...
    // FIXME if we can find a way around that we would get more adoptions
    if (get_rank(cfr) < rank && get_rank(cfr) > -1) {
      set_parent(x, cfr, get_mirror_field(x, cfr)-3);
      // set_rank(x, get_rank(cfr)+1);
      return 1;
    }
    cfr = next_cofriend(x, cfr);
  }
  return 0;
}

#define _adopt(x) ((get_parent(x) == _NULL) ? 0 : adopt(x))

#endif

#define loosen(x) set_rank(x, -1); pq_remove(x)

void drop() {
  obj x; // current "falling" rib
  obj *_x;
  obj cfr;

  while (!Q_IS_EMPTY()) {
    x = q_dequeue();
    _x = RIB(x)->fields;
    cfr = get_parent(x);

    // loosen(x);
    
    // making x's children "fall" along with him
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(_x[i]) && is_parent(_x[i], x) && (!is_root(_x[i]))) {
        if (get_rank(_x[i]) != -1 && (!adopt(_x[i]))) {
          // if we loosen here instead of when we dequeue, we can reuse the
          // queue field for the priority queue
          loosen(_x[i]);
          q_enqueue(_x[i]);
        }
      }
    }
    // identify x's co-friends that could be potential "catchers"
    while (cfr != _NULL) {
      if (get_rank(cfr) != -1) { // potential anchor?
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
#if defined(QUEUE_NO_REMOVE) || defined(LINKED_LIST_NO_REMOVE) || defined(BUCKETS_NO_REMOVE)
    // When using the "no remove" version of a data structure, the catch queue
    // could empty itself during the the pq_dequeue procedure, need to add an
    // additional check here
    if (anchor == _NULL) return;
#endif
    obj *_anchor = RIB(anchor)->fields;
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(_anchor[i]) && (get_rank(_anchor[i]) == -1)) {
        set_parent(_anchor[i], anchor, i);
        set_rank(_anchor[i], get_rank(anchor)+1);
        pq_enqueue(_anchor[i]); // add rescued node to potential "catchers"
      }
    }
  } while (!PQ_IS_EMPTY());
}

void dealloc_rib(obj x){
  set_rank(x, -2); // deallocated rib, this way we just ignore cycles
  obj *_x = RIB(x)->fields;
  for (int i = 0; i < 3; i++) {
    if (IS_RIB(_x[i])) {
      /* if (get_rank(_x[i]) == -2) { // already deallocated, child or not */
      /*          continue; */
      /* } else if (get_rank(_x[i]) == -1) { // falling, child or not */
      /*          dealloc_rib(_x[i]); */
      /* } else if (is_parent(_x[i], x)) { // child is a root */
      /*          remove_parent(_x[i], x, i); */
      /* } else { // not a child, only need to remove x from co-friend's list */
      /*   remove_cofriend(_x[i], x, i); */
      /* } */
      if (is_parent(_x[i], x)) {
        if (get_rank(_x[i]) == -2) { // already deallocated?
          continue;
        } else if (get_rank(_x[i]) == -1) { // falling?
          dealloc_rib(_x[i]);
        } else { // child is a root or protected
          if (get_parent(_x[i]) != _NULL) wipe_parent(_x[i], x, i);
        }
      } else { // not a child, only need to remove x from co-friend's list
        if (get_parent(_x[i]) != _NULL) wipe_cofriend(_x[i], x, i);
      }
    }
  }
  CAR(x) = (obj)alloc; // deallocate the rib by adding it to the freelist
  alloc = (obj *)x;
  d_count++;

  _x[6] = _NULL; // no parent

#ifdef CLEAN_RIBS
  for (int i = 1; i < RIB_NB_FIELDS; i++) {
    _x[i] = _NULL;
  }
#endif

#ifndef REF_COUNT
  set_rank(x, UNALLOCATED_RIB_RANK);
#endif 
}
  
void remove_edge(obj from, obj to, int i) {
  // `from` and `to` are assumed to be ribs

  // The integer i indicates the field where `to` was expected to be found
  // in `from`... we need to do this (at least with my implementation) because
  // the field is often set to another pointer before `remove_edge` is called
  // and so we need to know what the position of `to` was to traverse the list
  // of co-friends and set the mirror fields properly (or else get_mirror_field
  // always return -1). There might be a more elegant way to deal with this but
  // I think it does the job
    
  if (!is_parent(to, from)) {
    // nothing to do if `from` is not the parent of `of` other than remove
    // `from` from `to`'s co-friends since the structure of the subgraph
    // remains the same and `to` won't be deallocated
    remove_cofriend(to, from, i);
    return;
  }
  
  remove_parent(to, from, i); // `to` is "parentless"

  // Second condition happens when we remove an edge between a node and his
  // parent but the parent points to the child more than once
  if (!is_root(to) && (!is_parent(to, from)) && (!adopt(to))) {
    // Q_INIT(); // drop queue i.e. "falling ribs"
    // PQ_INIT(); // anchors i.e. potential "catchers"
    q_enqueue(to);
    set_rank(to, -1); // loosen without removing
    // @@(location gc-start)@@
    drop();
    if (!PQ_IS_EMPTY()) catch(); // avoid function call if no catchers
    if (get_rank(to) == -1) {
      dealloc_rib(to);
    }
    // @@(location gc-end)@@
  }
}

// FIXME should we just assume that `from` is a rib to avoid the type check?
#define remove_ref(from, to, i) if (IS_RIB(to)) remove_edge(from, to, i)

// FIXME need to generalize this so that it works with any protected field
#define remove_ref_nr(to, i) remove_edge(null_rib, to, i); TEMP5 = _NULL;


void remove_node(obj old_root) {
  // @@(location gc-start)@@
#ifndef NO_ADOPT
  if (_adopt(old_root)) return;
#endif
  q_enqueue(old_root);
  set_rank(old_root, -1); // loosen without removing
  drop();
  if (!PQ_IS_EMPTY()) {
    catch(); // avoid function call if no catchers
  }
  if (CFR(old_root) == _NULL) {
    dealloc_rib(old_root);
  }
  // @@(location gc-end)@@
}

#define remove_root(old_root) if (IS_RIB(old_root)) remove_node(old_root)

// end of code specific to ESTrees
#endif 

//------------------------------------------------------------------------------

// Write barriers

#ifdef REF_COUNT

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
    d_count++;
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

#else

#define protect(o, i) get_field(o,i) = MARK(get_field(o,i))

#define unprotect(o, i)                                                        \
  do {                                                                         \
    if (IS_RIB(o)) {                                                           \
      get_field(o,i) = UNMARK(get_field(o,i));                                 \
      remove_node(o);                                                          \
    }                                                                          \
  } while (0)


void set_field(obj src, int i, obj dest) { // write barrier
  // The order differs a bit from the ref count version of the write barrier...
  // TODO explain why we're doing that this way
  
  obj *ref = RIB(src)->fields;

  if (ref[i] == dest) return; // src's i-th field already points to dest
  
  if (IS_RIB(ref[i])) { // no need to dereference _NULL or a num
    if (!is_root(ref[i]) && is_parent(ref[i], src) && next_cofriend(ref[i], src) == _NULL) {
      // We need to be more careful here since simply removing the edge
      // between src and ref[i] will deallocate ref[i] and potentially
      // some other ribs refered by ref[i]. This is problematic if dest
      // contains a reference to one of ref[i]'s children (or more) since
      // we'll deallocate a rib (or more) that shouldn't be deallocated.
      // We can get around that by using a temporary rib to point to ref[i]
      obj tmp = ref[i];
#ifdef TAGGING
      protect(tmp, 7);
#else
      set_rank(NIL, get_rank(src));
      TEMP3 = ref[i]; // protect old dest
      add_edge(NIL, ref[i], 0);
#endif
      remove_ref(src, ref[i], i); // new dest
      ref[i] = dest;
      add_ref(src, dest, i);
#ifdef TAGGING
      unprotect(tmp, 7);
#else
      remove_ref(NIL, tmp, 0); // unprotect old dest
      TEMP3 = _NULL;
      set_rank(NIL, 1);
#endif
      update_ranks(src);
      return;
    }
    remove_ref(src, ref[i], i);
  }
  ref[i] = dest;
  add_ref(src, dest, i);
  if (IS_RIB(src)) update_ranks(src);
}

// FIXME assume `src` will always be a rib?

#define SET_CAR(src, dest) if (IS_RIB(src)) set_field(src, 0, dest)
#define SET_CDR(src, dest) if (IS_RIB(src)) set_field(src, 1, dest)
#define SET_TAG(src, dest) if (IS_RIB(src)) set_field(src, 2, dest)

void set_sym_tbl(obj new_sym_tbl) {
  obj old_sym_tbl = symbol_table;
  symbol_table = new_sym_tbl;
#ifdef ES_ROOTS
  if (IS_RIB(symbol_table)) set_rank(symbol_table, 0);
#endif

  update_ranks(symbol_table);

  remove_root(old_sym_tbl);
}

void set_stack(obj new_stack) {
  obj old_stack = stack;
  stack = new_stack;
#ifdef ES_ROOTS
  if (IS_RIB(stack)) set_rank(stack, 0);
#endif
  if (IS_RIB(stack)) update_ranks(stack);

#ifdef ES_ROOTS
  if (IS_RIB(stack)) {
    obj *_stack = RIB(stack)->fields;
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(_stack[i])) {
        // FIXME not sure how this will behave when pc and stack are both
        // pointing to the same rib
        if (!is_parent(_stack[i], stack)) {
          set_parent(_stack[i], stack, i);
          set_rank(_stack[i], 1);
          update_ranks(_stack[i]);
        }
      }
    }
  }
#endif
  remove_root(old_stack);
}

void set_pc(obj new_pc) {
  obj old_pc = pc;
  pc = new_pc;
#ifdef NO_ADOPT
  set_rank(pc, 0);
#endif
  update_ranks(pc);

#ifdef ES_ROOTS
  obj *_pc = RIB(pc)->fields;
  for (int i = 0; i < 3; i++) {
    if (IS_RIB(_pc[i])) {
      // FIXME not sure how this will behave when pc and stack are both
      // pointing to the same rib
      if (!is_parent(_pc[i], pc)) {
        set_parent(_pc[i], pc, i);
        set_rank(_pc[i], 1);
        update_ranks(_pc[i]);
      }
    }
  }
#endif
  remove_root(old_pc);
}

#endif


//==============================================================================

// RVM

//------------------------------------------------------------------------------

// Cycle detection and collection (ref count)

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

//------------------------------------------------------------------------------

// Stack and heap management

#ifdef REF_COUNT

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
 
#else

obj pop() {
  obj tos = CAR(stack);
  if (IS_RIB(tos) && M_CAR(stack) == _NULL) {
    // protect TOS only if it gets deallocated otherwise
#ifdef TAGGING
    protect(tos, 7);
#else
    TEMP5 = tos;
    add_edge(null_rib, tos, 0);
#endif
  }
  set_stack(CDR(stack));
  return tos;
}

// to avoid too many preprocessor instructions in the RVM code
#ifdef TAGGING
#define DEC_POP(o) if (IS_RIB(o) && is_protected(o, 7)) unprotect(o, 7)
#else
#define DEC_POP(o) if (TEMP5 == o) remove_ref_nr(o, 0)
#endif


#ifdef TAGGING

#define _protect(var, i) if (IS_RIB(var) && M_CAR(stack) == _NULL) protect(var, i)
#define _unprotect(var, i) if (IS_RIB(var) && is_protected(var, i)) unprotect(var, i)

#define _pop(var, i)                                                            \
  obj var = CAR(stack);                                                         \
  _protect(var, 7);                                                             \
  set_stack(CDR(stack))

#define PRIM1() _pop(x, 0)
#define PRIM2() _pop(y, 1); PRIM1()
#define PRIM3() _pop(z, 2); PRIM2()

#define DEC_PRIM1() _unprotect(x, 7);
#define DEC_PRIM2() _unprotect(y, 7); DEC_PRIM1()
#define DEC_PRIM3() _unprotect(z, 7); DEC_PRIM2()

#else
// FIXME!!! only protect a popped object if said object would get deallocated
// otherwise (can easily be checked since M_CAR(stack) would be _NULL)

#define _pop(var, i)                                                            \
  obj var = CAR(stack);                                                         \
  add_ref(null_rib, var, i);                                                    \
  set_stack(CDR(stack));

#define PRIM1() TEMP5 = CAR(stack); _pop(x, 0)
#define PRIM2() TEMP6 = CAR(stack); _pop(y, 1); PRIM1()
#define PRIM3() TEMP7 = CAR(stack); _pop(z, 2); PRIM2()

#define CLEAR_NR()                                                              \
  obj *nr_ptr = RIB(null_rib)->fields;                                          \
  nr_ptr[3] = _NULL;                                                            \
  nr_ptr[4] = _NULL;                                                            \
  nr_ptr[5] = _NULL;

#define DEC_PRIM1()                                                             \
  remove_ref(null_rib, x, 0);                                                   \
  TEMP5 = _NULL;                                                                \
  CLEAR_NR()
#define DEC_PRIM2()                                                             \
  remove_ref(null_rib, y, 1);                                                   \
  TEMP6 = _NULL;                                                                \
  DEC_PRIM1()
#define DEC_PRIM3()                                                             \
  remove_ref(null_rib, z, 2);                                                   \
  TEMP7 = _NULL;                                                                \
  DEC_PRIM2()

#endif
#endif

#ifdef REF_COUNT

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

#else

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
  *alloc++ = TAG_NUM(0); // rank will be 0 since it becomes the new stack
  *alloc++ = _NULL;      // queue and priority queue
#if defined(QUEUE_NO_REMOVE) || defined(LINKED_LIST_NO_REMOVE) || defined(BUCKETS)
  *alloc++ = _NULL;
#endif
#ifdef BUCKETS_NO_REMOVE
  *alloc++ = _NULL;
#endif

  obj new_rib = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));
  
  add_ref(new_rib, stack, 1);
  stack = new_rib;

  add_ref(new_rib, car, 0);
  add_ref(new_rib, tag, 2);

#ifdef ES_ROOTS
  obj *_stack = RIB(stack)->fields;
  for (int i = 0; i < 2; i++) {
    if (IS_RIB(_stack[i])) {
      // FIXME not sure how this will behave when pc and stack are both
      // pointing to the same rib
      if (!is_parent(_stack[i], stack)) {
        set_parent(_stack[i], stack, i);
        set_rank(_stack[i], 1);
        update_ranks(_stack[i]);
      }
    }
  }
#else
  if (IS_RIB(CDR(stack))) set_parent(CDR(stack), stack, 1);
#endif
  
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
  *alloc++ = TAG_NUM(0); 
  *alloc++ = _NULL;      // queue and priority queue
#if defined(QUEUE_NO_REMOVE) || defined(LINKED_LIST_NO_REMOVE) || defined(BUCKETS)
  *alloc++ = _NULL;
#endif
#ifdef BUCKETS_NO_REMOVE
  *alloc++ = _NULL;
#endif

  obj new_rib =  TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));

  add_ref(new_rib, car, 0);
  add_ref(new_rib, cdr, 1);
  add_ref(new_rib, tag, 2);

  alloc = (obj *)tmp;
  
  return RIB(new_rib);
}

#define alloc_rib2(car, cdr, tag) alloc_rib(car, cdr, tag)

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

void run() { // evaluator
  while (1) {
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
          if (s != stack) set_stack(s); 
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
          obj new_pc = CAR(pc); // proc entry point
          SET_CAR(pc, TAG_NUM(instr));
          set_pc(TAG(new_pc));
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

  // calloc is faster with gcc
  // heap_start = calloc(SPACE_SZ, sizeof(obj)); 
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
#ifndef REF_COUNT
#endif
  }
  alloc = scan;
  stack = NUM_0;
}

#define INIT_FALSE()                                                           \
  FALSE = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),   \
                            TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),   \
                            SINGLETON_TAG));

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

// @@(feature encoding/optimal
void decode() {
  // @@(replace "{1,2,3}" (list->host encoding/optimal/start "{" "," "}")
  int weights[] = {1,2,3};
  // )@@
  obj n;
  int d;
  int op;
  int i;
  while (1) {
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
    // Force remove n
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

/* void viz_add_rib_label(FILE* graph, obj rib, obj car, obj cdr, obj tag, obj rank){ */
/*   // write the value of the rib */
/*   char* car_prefix = IS_RIB(car) ? "r" : ""; */
/*   char* cdr_prefix = IS_RIB(cdr) ? "r" : ""; */
/*   char* tag_prefix = IS_RIB(tag) ? "r" : ""; */
/*   long rib_value = rib - ((long)heap_start); */
/*   long car_value = IS_RIB(car) ? car-((long)heap_start) : NUM(car); */
/*   long cdr_value = IS_RIB(cdr) ? cdr-((long)heap_start) : NUM(cdr); */
/*   long tag_value = IS_RIB(tag) ? tag-((long)heap_start) : NUM(tag); */
/*   long rank_value = NUM(rank); */
/*   fprintf( */
/*       graph, */
/*       "%ld [label=\"%ld : [%s%ld,%s%ld,%s%ld] -- %ld\"]\n", */
/*       rib, */
/*       rib_value, */
/*       car_prefix, car_value, */
/*       cdr_prefix, cdr_value, */
/*       tag_prefix, tag_value, */
/*       rank_value); */
/* } */

/* void viz_heap(){ */
/*   // to check manually if the tests are working properly */
/*   current_graph = viz_start_graph("graph.dot"); */
/*   scan=heap_top; */
  
/*   for (int i = 0; i <= MAX_NB_OBJS; i++) { */
/* #ifdef REF_COUNT */
/*     obj rank = scan[3]; */
/* #else */
/*     obj rank = scan[7]; */
/* #endif */
/*     if (IS_RIB(scan[0])) viz_add_edge(current_graph, scan, scan[0]); */
/*     if (IS_RIB(scan[1])) viz_add_edge(current_graph, scan, scan[1]); */
/*     if (IS_RIB(scan[2])) viz_add_edge(current_graph, scan, scan[2]); */
/*     // viz_add_rib_label(current_graph, scan, scan[0], scan[1], scan[2], rank); */
/*     if (scan == stack) { */
/*       viz_add_rib_label(current_graph, scan, scan[0], scan[1], scan[2], TAG_NUM(-33)); */
/*     } else if (scan == pc) { */
/*       viz_add_rib_label(current_graph, scan, scan[0], scan[1], scan[2], TAG_NUM(-444)); */
/*     } else if (scan == FALSE || scan == TRUE || scan == NIL) { */
/*       viz_add_rib_label(current_graph, scan, scan[0], scan[1], scan[2], TAG_NUM(-5555)); */
/*     } else if (scan == symbol_table) { */
/*       viz_add_rib_label(current_graph, scan, scan[0], scan[1], scan[2], TAG_NUM(-66666)); */
/*     } else { */
/*       viz_add_rib_label(current_graph, scan, scan[0], scan[1], scan[2], rank); */
/*     } */
/*     scan-=RIB_NB_FIELDS; */
/*   } */
/*   viz_end_graph(current_graph); */
/*   // exit(1); */
/* } */

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
    int rank = get_rank(scan);

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
 
int main() { init(); }
