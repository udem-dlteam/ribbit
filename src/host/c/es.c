/* 
 * File: es.c
 *
 * Author: Frédéric Lahaie-Bertrand
 *
 * Implementation of the Ribbit Virtual Machine in C with an incremental 
 * garbage collector inspired by Even-Shiloach trees.
 *
 * This version of the RVM doesn't include all the features defined in the
 * original version (DEBUG, NO_STD, lzss compression, original encoding, 
 * stop & copy GC, R4RS lib's chars/strings, etc.) but can be used with
 * the R4RS library.
 */

#include <stdio.h>
#include <stdlib.h>

// README for now the RVM can be used with the ref count GC using the
// `-f+ ref-count` flag to execute a program (if the path to this file is
// properly configured instead of `rvm.c`) OR as a standalone program 
// to test the implementation of ES trees (for example with the commands
// `gcc es.c -o es.exe && ./es.exe`). Note that the RVM includes a simple
// mark-sweep GC because the ref count GC can't deallocate ribs that are
// part of a cycle (e.g. in the case of a recursive function, the closure's
// "subgraph" will contain the symbol of the procedure which itself contains
// a reference to the procedure's code, this means that everything involved
// in that cycle won't be deallocated: the continuation rib, the `get` and
// `const` instruction ribs, the other symbols and primitives/lambdas
// involved in the function and the ribs that they refer to, etc..

// Before the incremental GC can be used, I need to modify all the count
// increment and decrement of the ref count GC as well as the write barrier
// and the other... count management procedures? 

// @@(feature ref-count
#define REF_COUNT // remove comment when using the ref count GC
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

#ifdef REF_COUNT
#define RIB_NB_FIELDS 4
#else
#ifdef BUCKETS
#define RIB_NB_FIELDS 11
#else
#define RIB_NB_FIELDS 10
#endif
#endif
typedef struct {
  obj fields[RIB_NB_FIELDS];
} rib;

#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]

#ifndef REF_COUNT
#define M_CAR(x) RIB(x)->fields[3]   // mirror fields
#define M_CDR(x) RIB(x)->fields[4]
#define M_TAG(x) RIB(x)->fields[5]
#define CFR(x) RIB(x)->fields[6]     // co-friends
#define RANK(x) RIB(x)->fields[7] 
#define Q_NEXT(x) RIB(x)->fields[8]  // queue next

#ifdef BUCKETS
#define PQ_NEXT_RIB(x) RIB(x)->fields[9]
#define PQ_NEXT_BKT(x) RIB(x)->fields[10]
#else
#define PQ_NEXT(x) RIB(x)->fields[9] // pqueue next (linked list)
#endif

/* #ifdef RB_TREES */
/* #define PQ_LEFT(x) RIB(x)->fields[9] */
/* #define PQ_RIGHT(x) RIB(x)->fields[10] */
/* #define PQ_PARENT(x) RIB(x)->fields[11] */
/* #endif */

#endif

// #define UNTAG(x) ((x) >> 1)
#define UNTAG(x) ((x) >> 2)
#define NUM(x) ((num)(UNTAG((num)(x))))
#define RIB(x) ((rib *)(x))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x))
// #define TAG_NUM(num) ((((obj)(num)) << 1) | 1)
#define TAG_NUM(num) ((((obj)(num)) << 2) | 1)
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))

#define MARK(x) ((x)|2) // assumes x is a tagged obj (ul)
#define UNMARK(x) ((x)^2)
#define IS_MARKED(x) ((x)&2)
#define GET_MARK(o) (IS_MARKED(CDR(o)) + (IS_MARKED(TAG(o)) >> 1))

/* #define IS_CHILD(x) ((x)&2) */
/* #define TAG_CHILD(x) ((x)|2) // assumes x is a tagged obj (ul) */
/* #define UNTAG_CHILD(x) ((x)^2) */

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

#define _NULL ((obj) NULL)

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
// Temp values that can be used to shield  pointers from the evil GC
#define TEMP1 CAR(TRUE)
#define TEMP2 CDR(TRUE)
#define TEMP3 CAR(NIL)
#define TEMP4 CDR(NIL)

#define TOS (CAR(stack))

obj *alloc;
obj *scan;

rib *heap_start;
#ifdef REF_COUNT
#define MAX_NB_OBJS 100000000
#else
#define MAX_NB_OBJS 10
#endif
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_top (heap_bot + (SPACE_SZ))


//==============================================================================

// Data structures

#ifndef REF_COUNT

// Queue

obj q_head; // dequeue at head => O(1)
obj q_tail; // enqueue at tail => O(1)

void q_init() {
  q_head = _NULL;
  q_tail = _NULL;
}

#define Q_IS_EMPTY() (q_head == _NULL)

void q_enqueue(obj o) {
  // Only add a rib to the queue if it's not already there
  if (IS_RIB(o) && (Q_NEXT(o) == _NULL)) {
    Q_NEXT(o) = _NULL;
    if (Q_IS_EMPTY()){
      q_head = o;
    } else {
      Q_NEXT(q_tail) = o;
    }
    q_tail = o;
  }
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
//  - Something else...?


obj pq_head;

void pq_init() {
  pq_head = _NULL;
}

#define PQ_IS_EMPTY() (pq_head == _NULL)

#ifdef BUCKETS

// Priority queue implemented with buckets

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

// Priority queue implemented with a singly linked list

void pq_enqueue(obj o) {
  // the lower the rank, the closer the rib is to pq_head
  if (IS_RIB(o)) {
    if (PQ_IS_EMPTY()){
      PQ_NEXT(o) = _NULL;
      pq_head = o;
    }
    else if (RANK(o) == 1) { // root (tagged rank)
      PQ_NEXT(o) = pq_head;
      pq_head = o;
    }
    else {
      // insert new rib after the first rib that has a lower rank
      obj prev = pq_head;
      obj curr = PQ_NEXT(pq_head);
      while (curr != _NULL && RANK(curr) < RANK(o)) {
        prev = curr;
        curr = PQ_NEXT(curr);
      }
      PQ_NEXT(o) = curr;
      PQ_NEXT(prev) = o;
    }
  }
}

obj pq_dequeue() {
  if (PQ_IS_EMPTY()){
    return _NULL;
  }
  obj tmp = pq_head;
  pq_head = PQ_NEXT(tmp);
  PQ_NEXT(tmp) = _NULL;
  return tmp;
}

// Set operations

void pq_remove(obj o) {
  // TODO need a faster way to detect if `o` is not in the pqeueue
  if (IS_RIB(o)) {
    if (pq_head == _NULL) { // || (pq_head != _NULL && PQ_NEXT(*o) == _NULL)) {
      // empty pqueue (set) or the rib is not in the pqueue (set)
      return;
    } else if (pq_head == o) {
      // dequeue but we don't return the rib
      obj tmp = pq_head;
      pq_head = PQ_NEXT(tmp);
      PQ_NEXT(tmp) = _NULL;
    } else {
      obj curr = PQ_NEXT(pq_head);
      obj prev = pq_head;
      while (curr != o && curr != _NULL) {
        prev = curr;
        curr = PQ_NEXT(curr);
      }
      if (curr == _NULL) { // not found in set
        return;
      }
      PQ_NEXT(prev) = PQ_NEXT(curr);
      PQ_NEXT(curr) = _NULL;
    }
  }
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
//
//  - An edge is said to be dirty if rank(to) < rank(from)-1 because rank(to)
//    (i.e. its distance from the root) could be reduced by making `from` the
//    new parent of `to`


// FIXME try to be a bit more consistant with the notation...
#define get_field(x,i) RIB(x)->fields[i]

int get_rank(obj x) {
  return NUM(RANK(x));
}

void set_rank(obj x, int rank) {
  RANK(x) = TAG_NUM(rank);
}

bool is_dirty(obj from, obj to) {
  return (get_rank(from) < (get_rank(to) - 1));
}

int get_mirror_field(obj x, obj cfr) {
  for (int i = 0; i < 3; i++){
    if (get_field(cfr,i) == x) {
      return i+3;
    }
  }
  return -1; // cfr is not x's co-friend 
}

obj next_cofriend(obj x, obj cfr) {
  // get next co-friend in x's list of co-friends
  return get_field(cfr, get_mirror_field(x, cfr));
}

bool is_parent(obj x, obj p) {
  return (CFR(x) == p); 
}

obj get_parent(obj x) {
  return CFR(x);
}

void set_parent(obj child, obj new_parent) {
  obj old_parent = get_parent(child);
  if (old_parent == _NULL || old_parent == new_parent) { // parentless child
    CFR(child) = new_parent;
    // TAG_CHILD(new_parent[get_mirror_field(child, new_parent)-3]);
    return;
  }
  int i = get_mirror_field(child, old_parent);
  int j = get_mirror_field(child, new_parent);
  // UNTAG_CHILD(old_parent[i-3]);
  get_field(old_parent,i) = get_field(new_parent,j); // assumes new_parent is a cfr
  get_field(new_parent,j) = old_parent;
  CFR(child) = new_parent;
  // TAG_CHILD(new_parent[j-3]); 
}

void remove_parent(obj x, obj p) {
  int i = get_mirror_field(x, p);
  // FIXME this could be problematic since we don't tag the parent
  // and the next co-friend is not technically the parent...
  // UNTAG_CHILD(p[i-3]);
  CFR(x) = get_field(p,i);
  get_field(p,i) = _NULL;

  // FIXME see test 3.1 to see how this could happen: after adding the edges
  // r0->r7 and then r7->r0, r7 will become r0's only co-friend (other than
  // r1 which is his parent) even though r0 is r7's parent. When removing the
  // edge r1->r0, r7 will also become r0's parent. This parent/child cycle
  // will create an infinite loop in the drop phase... needs to be handled
  // somehow and this is probably not the proper way to do it...
  if (CFR(x) != _NULL && is_parent(CFR(x), x)) {
    // should probably check if there's another co-friend and wrap this in
    // a loop until we find a co-friend that's not a child and if there's
    // no such co-friend, THEN we assign CFR(x) = _NULL
    CFR(x) = _NULL; 
  }
}

void add_cofriend(obj x, obj cfr) {
  // FIXME do we want the co-friends to be ordered by rank? for now the new
  // co-friend is just inserted between the parent and the following co-friend
  obj p = get_parent(x);
  if (p == _NULL) {
    set_parent(x, cfr);
    return;
  }
  int i = get_mirror_field(x, p);
  int j = get_mirror_field(x, cfr);
  obj tmp = get_field(p,i); // old co-friend pointed by parent
  get_field(p,i) = cfr;
  get_field(cfr,j) = tmp;
}

void remove_cofriend(obj x, obj cfr) {
  // note that we assume that cfr is not x's parent
  obj curr = CFR(x);
  obj prev;
  if (curr != _NULL) {
    while (curr != cfr) {
      prev = curr;
      curr = next_cofriend(x, curr);
    }
    int i = get_mirror_field(x, curr);
    int j = get_mirror_field(x, prev);
    obj tmp = get_field(curr,i);
    get_field(curr,i) = _NULL; // remove reference to next co-friend
    get_field(prev,j) = tmp;
  }
}


//------------------------------------------------------------------------------

// Edge addition (i.e. add a reference to a rib)

// Intuition: TODO

void add_edge(obj from, obj to) {
  // FIXME duplicate edges are allowed for now
  if (IS_RIB(from) && IS_RIB(to)) {
    add_cofriend(to, from);
    if (is_dirty(from, to)) {
      set_parent(to, from);
    }
    // The reference from a new root to an existing rib is not considered dirty
    // so we need to have the `is_parent` check to account for that situation
    // (the check passes if the edge was dirty as well)... this shouldn't be a
    // problem for ribbit but I'll leave it there for now
    if (is_parent(to, from)) {
      q_init(); 
      q_enqueue(to);
      int r = get_rank(from)+1;
      set_rank(to, r);
      obj _to;
      obj *t;
      do {
        _to = q_dequeue();
        t = RIB(_to)->fields;
        r = get_rank(_to)+1;
        for (int i = 0; i < 3; i++) {
          // if the parent of `to` was swapped (i.e. his rank decreased), some
          // of the edges in the subgraph rooted at `to` might now be dirty,
          // see test 1.1 for an example... this is why we need to do a BFS
          if (IS_RIB(t[i]) && (is_parent(t[i], _to) || is_dirty(_to, t[i]))) {
            set_parent(t[i], _to); // FIXME redundant if _to is the parent
            set_rank(t[i], r); 
            q_enqueue(t[i]);
          }
        }
      } while (!Q_IS_EMPTY());
    }
  }
}


//------------------------------------------------------------------------------

// Edge deletion (i.e. removing a reference to a rib)

// Intuition: TODO

void loosen(obj x) {
  set_rank(x, -1);
  // TODO possible optimization: don't remove here, just check if the rib has
  // rank -1 in the catch phase, which is possible since we re-use the anker's
  // "set" in the drop phase for the catch pqueue in the catch phase... this
  // effectively transforms every remove (the most expensive operation in our
  // pqueue) in a dequeue, which is done in O(1)
  pq_remove(x);
}

void drop() {
  obj x; // current "falling" rib
  obj *_x;
  obj cfr;
  while (!Q_IS_EMPTY()) {
    x = q_dequeue();
    _x = RIB(x)->fields;
    cfr = get_parent(x);
    loosen(x);
    // making x's children "fall" along with him
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(_x[i]) && is_parent(_x[i], x)) {
        q_enqueue(_x[i]);
      }
    }
    // identify x's co-friends that could be potential "catchers"
    while (cfr != _NULL) {
      if (get_rank(cfr) != -1) { // potential anker?
        pq_enqueue(cfr);
      }
      cfr = next_cofriend(x, cfr);
    }
  }
}

void catch() {
  // since we use a priority queue instead of a set for the ankers,
  // we can re-use it (as is) for the catch queue...
  while (!PQ_IS_EMPTY()) {
    obj anker = pq_dequeue();
    obj *_anker = RIB(anker)->fields;
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(_anker[i]) && (get_rank(_anker[i]) == -1)) {
        set_parent(_anker[i], anker);
        set_rank(_anker[i], get_rank(anker)+1);
        pq_enqueue(_anker[i]); // add rescued node to potential "catchers"
      }
    }
  }
}

void dealloc_rib(obj x){
  set_rank(x, -2); // deallocated rib, this way we just ignore cycles
  obj *_x = RIB(x)->fields;
  for (int i = 0; i < 3; i++) {
    if (IS_RIB(_x[i])) {
      if (get_rank(_x[i]) == -1) {
        dealloc_rib(_x[i]);
      } else {
        remove_cofriend(_x[i], x);
      }
    }
  }
  CAR(x) = alloc; // deallocate the rib by adding it to the freelist
  alloc = x;
}
  
void remove_edge(obj from, obj to) {
  // not much to do when `from` is not the parent of `to` since the structure
  // of the spanning tree remains identical, i.e all ranks remain the same
  if (!is_parent(to, from)) {
    remove_cofriend(to, from);
    
    // TODO remove, just used my tests but this will be done elsewhere in the GC
    RIB(from)->fields[get_mirror_field(to,from)-3] = TAG_NUM(0);
    
    return;
  }
  remove_parent(to, from); // `to` is "parentless"

  // TODO remove, same as above
  RIB(from)->fields[get_mirror_field(to,from)-3] = TAG_NUM(0);
  
  q_init(); // drop queue i.e. "falling ribs"
  pq_init(); // ankers i.e. potential "catchers"
  
  q_enqueue(to);
  
  drop(); 
  catch();
  if (get_parent(to) == _NULL) {
    dealloc_rib(to); 
  }
}

#endif

//------------------------------------------------------------------------------

// References management

#ifdef REF_COUNT

void inc_count(obj o) {
  // assumes o is a rib object
  obj *ptr = RIB(o)->fields;
  num count = NUM(ptr[3])+1;
  ptr[3] = TAG_NUM(count);
}

int d_count = 0;

void dec_count(obj o) {
  // assumes o is a rib object
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
  }
  else {
    ptr[3] = TAG_NUM(count);
  }
}

// only increment and decrement count of ribs, not nums
#define INC_COUNT(o) if (IS_RIB(o)) inc_count(o)
#define DEC_COUNT(o) if (IS_RIB(o)) dec_count(o)
#define GET_COUNT(o) IS_RIB(o) ? NUM(RIB(o)->fields[3]) : -1

void set_pc(obj new_pc) {
  obj old_pc = pc;
  pc = new_pc;
  INC_COUNT(pc);
  DEC_COUNT(old_pc);
}

void set_stack(obj new_stack) {
  obj old_stack = stack;
  stack = new_stack;
  INC_COUNT(stack);
  DEC_COUNT(old_stack);
}

void set_sym_tbl(obj new_sym_tbl) {
  obj old_sym_tbl = symbol_table;
  symbol_table = new_sym_tbl;
  INC_COUNT(symbol_table);
  DEC_COUNT(old_sym_tbl);
}

void write(obj src, obj dest, int i) {
  obj *p_src = RIB(src)->fields;
  // Order matters: if we decrease before increasing we might deallocate
  // before doing the assignment, resulting in a segfault (same applies
  // for set_pc and set_stack)
  INC_COUNT(dest);
  DEC_COUNT(p_src[i]);
  p_src[i] = dest;
}

#define SET_CAR(src, dest) write(src, dest, 0)
#define SET_CDR(src, dest) write(src, dest, 1)
#define SET_TAG(src, dest) write(src, dest, 2)

#else

void set_field(obj src, int i, obj dest) { // write barrier
  if (IS_RIB(src)) {
    obj *ref = RIB(src)->fields;
    if (IS_RIB(ref[i])) { // no need to dereference _NULL or a num
      remove_edge(src, ref[i]);
    }
    ref[i] = dest;
    if (IS_RIB(dest)) { // only needed if dest is a rib
      add_edge(src, dest);
    }
  }
}

#define SET_CAR(src, dest) set_field(src, 0, dest)
#define SET_CDR(src, dest) set_field(src, 1, dest)
#define SET_TAG(src, dest) set_field(src, 2, dest)

#endif


//==============================================================================

// RVM

#ifdef REF_COUNT

//------------------------------------------------------------------------------

// Memory management

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
  printf("\t--GC called\n");
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
    } else {
      *scan = (obj)alloc;
      alloc = scan;
      *(scan+3) = TAG_NUM(0);
    }
    scan += RIB_NB_FIELDS; // next rib object
  }
  if (*alloc == _NULL){
    printf("Heap is full\n");
  }
}

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

#define PRIM1() obj x = pop()
#define PRIM2() obj y = pop(); PRIM1()
#define PRIM3() obj z = pop(); PRIM2()

#define DEC_PRIM1() DEC_COUNT(x)
#define DEC_PRIM2() DEC_COUNT(y); DEC_PRIM1()

void push2(obj car, obj tag) {
  obj tmp = *alloc; // next available slot in freelist

  // TODO the ref count version of the RVM sets the count to 1 when a rib is
  // allocated on the heap since the newly allocated rib will either be
  // accessible from a local variable if the rib was allocated through
  // alloc_rib(2) or else from the stack global variable. In other words,
  // the count is set to 1 right away because we don't care where the reference
  // to that new rib comes from. Here we need to be a bit more careful since
  // we're dealing with actual references, not a "counter"
  
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;        // field 1
  *alloc++ = stack;      // field 2
  *alloc++ = tag;        // field 3
/* #ifdef REF_COUNT */
  *alloc++ = TAG_NUM(1); // ref count of 1 cos pointed by stack
  alloc += (RIB_NB_FIELDS-4);
/* #else */
/*   *alloc++ = _NULL;      // mirror 1 */
/*   *alloc++ = _NULL;      // mirror 2 */
/*   *alloc++ = _NULL;      // mirror 3 */
/*   *alloc++ = _NULL;      // co-friends */
/*   *alloc++ = TAG_NUM(0); // rank */
/*   *alloc++ = _NULL;      // queue */
/*   *alloc++ = _NULL;      // priority queue */
/* #ifdef BUCKETS */
/*   *alloc++ = _NULL; */
/* #endif */
/* #endif */
  stack = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));

  alloc = (obj *)tmp;
  
/* #ifdef REF_COUNT */
  // no need to increase ref count of stack because it remains unchanged
  // only difference is that the ref comes from the rib pointed by instead
  // of the stack pointer itself
  INC_COUNT(car);
  INC_COUNT(tag);
  
  if (!IS_RIB(tmp) || *alloc == _NULL) { // empty freelist?
    gc();
    // printf("heap is full, ya goofed\n");
  }
/* #endif */
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

// @@(feature bool2scm 
obj bool2scm(bool x) { return x ? TRUE : FALSE; }
// )@@

// Primitive procedures
obj prim(int no) {
  switch (no) { 
  // @@(primitives (gen "case " index ":" body)
  case 0: // @@(primitive (##rib a b c)
  {
    obj new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
    PRIM3();
    CAR(new_rib) = x;
    CDR(new_rib) = y;
    TAG(new_rib) = z;
    push2(new_rib, PAIR_TAG);
    DEC_COUNT(new_rib); // remove redundant new_rib count
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
    DEC_COUNT(closure);
    //
    DEC_COUNT(CAR(closure));
    break;
  } //)@@
  case 5: // @@(primitive (##rib? rib) (use bool2scm)
  {
    PRIM1();
    push2(bool2scm(IS_RIB(x)), PAIR_TAG);
    DEC_PRIM1();
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
    DEC_COUNT(y);
    DEC_PRIM2();
    break;
  } //)@@
  case 10:  // @@(primitive (##field1-set! rib x)
  {
    PRIM2();
    SET_CDR(x, y);
    push2(CDR(x) = y, PAIR_TAG);
    DEC_COUNT(y);
    DEC_PRIM2();
    break;
  } //)@@
  case 11:  // @@(primitive (##field2-set! rib x)
  {
    PRIM2();
    SET_TAG(x, y);
    push2(TAG(x) = y, PAIR_TAG);
    DEC_COUNT(y);
    DEC_PRIM2();
    break;
  } // )@@
  case 12:  // @@(primitive (##eqv? rib1 rib2) (use bool2scm)
  {
    PRIM2();
    push2(bool2scm(x == y), PAIR_TAG);
    DEC_PRIM2();
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

// Execution loop
void run() {
  while (1) {
    num instr = NUM(CAR(pc));
    switch (instr) {
    case INSTR_AP: // call or jump
    {
      bool jump = TAG(pc) == NUM_0;
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
          set_pc(TAG(pc)); // ADVANCE_PC();
        }

        else { // calling a lambda
          num nargs = NUM(pop()); // @@(feature arity-check)@@
          obj new_stack = TAG_RIB(alloc_rib(NUM_0, proc, PAIR_TAG));      
          proc = CDR(new_stack);
          SET_CAR(pc, CAR(proc)); // save the proc from the mighty gc
          
          num nparams_vari = NUM(CAR(CAR(proc)));
          num nparams = nparams_vari >> 1;
          // @@(feature arity-check
          num vari = nparams_vari&1;
          if (vari ? nparams > nargs : nparams != nargs) {
            printf("*** Unexpected number of arguments nargs: %ld nparams: %ld vari: %ld\n", nargs, nparams, vari);
            exit(1);
          }
          // )@@
          // @@(feature rest-param (use arity-check)
          nargs-=nparams;
          if (vari){
            obj rest = NIL;
            for(int i = 0; i < nargs; ++i){
              rest = TAG_RIB(alloc_rib(pop(), rest, PAIR_TAG));
              DEC_COUNT(CAR(rest)); // popped
              DEC_COUNT(CDR(rest)); // old rest
            }
            new_stack = TAG_RIB(alloc_rib(rest, new_stack, PAIR_TAG));
            DEC_COUNT(CAR(new_stack)); // rest
            DEC_COUNT(CDR(new_stack)); // old new stack
          }
          // )@@
          for (int i = 0; i < nparams; ++i) {
            new_stack = TAG_RIB(alloc_rib(pop(), new_stack, PAIR_TAG));
            DEC_COUNT(CAR(new_stack)); // popped
            DEC_COUNT(CDR(new_stack)); // old new stack
          }
          if (CDR(new_stack) != new_stack) { INC_COUNT(CDR(new_stack)); }

          nparams = nparams + vari; // @@(feature arity-check)@@

          obj new_cont = TAG_RIB(list_tail(RIB(new_stack), nparams));
          
          if (jump) {
            obj k = get_cont();
            SET_CAR(new_cont, CAR(k)); // CAR(new_cont) = CAR(k);
            SET_TAG(new_cont, TAG(k)); // TAG(new_cont) = TAG(k);
            DEC_COUNT(k);
          } else {
            SET_CAR(new_cont, stack); // CAR(new_cont) = stack;
            SET_TAG(new_cont, TAG(pc)); // TAG(new_cont) = TAG(pc);
          }
          set_stack(new_stack);
          DEC_COUNT(new_stack);
          
          obj new_pc = CAR(pc); // proc entry point
          SET_CAR(pc, TAG_NUM(instr));
          set_pc(TAG(new_pc));
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
      set_pc(TAG(pc)); // ADVANCE_PC();
      break;
    }
    case INSTR_GET: { // get
      push2(get_opnd(CDR(pc)), PAIR_TAG);
      set_pc(TAG(pc)); // ADVANCE_PC();
      break;
    }
    case INSTR_CONST: { // const
      push2(CDR(pc), PAIR_TAG);
      set_pc(TAG(pc)); // ADVANCE_PC();
      break;
    }
    case INSTR_IF: { // if
      obj p = pop();
      set_pc((p != FALSE) ? CDR(pc) : TAG(pc));
      DEC_COUNT(p);
      break;
    }
    case INSTR_HALT: { // halt
      printf("dealloc count = %d\n", d_count);
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

#endif

void init_heap() {
  heap_start = malloc(sizeof(obj) * SPACE_SZ);
#ifdef REF_COUNT
  if (!heap_start) {
    vm_exit(EXIT_NO_MEMORY);
  }
#endif
  // initialize freelist
  scan = heap_top;
  *scan = _NULL;
  
  while (scan != heap_bot) {
    alloc = scan; // alloc <- address of previous slot
    scan -= RIB_NB_FIELDS; // scan <- address of next rib slot
    *scan = (obj)alloc; // CAR(next rib) <- address of previous slot
  }
  alloc = scan;
  stack = NUM_0;
}

#ifdef REF_COUNT

#define INIT_FALSE()                                                           \
  FALSE = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),   \
                            TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),   \
                            SINGLETON_TAG));                                   \
  DEC_COUNT(CAR(FALSE));                                                       \
  DEC_COUNT(CDR(FALSE));


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
  DEC_COUNT(CDR(sym)); // redundant count 
  rib *root = alloc_rib(TAG_RIB(sym), symbol_table, PAIR_TAG);
  DEC_COUNT(CAR(root)); // redundant count
  DEC_COUNT(CDR(root)); // redundant count
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
      accum = NIL;
      continue;
    }
    if (c == 59)
      break;
    accum = TAG_RIB(alloc_rib(TAG_NUM(c), TAG_RIB(accum), PAIR_TAG));
    DEC_COUNT(CDR(accum));
  }
  set_sym_tbl(TAG_RIB(create_sym(accum))); // symbol_table = TAG_RIB(create_sym(accum));
  DEC_COUNT(accum);
  DEC_COUNT(accum);
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
    if (op < 24) n = op%2> 0 ? get_int(n):n; 
    if (op < 20) {
      i = (op / 4) - 1;
      i = i < 0?0:i;
      n = !(op & 0b10)  ? TAG_NUM(n) : TAG_RIB(symbol_ref(n));
      INC_COUNT(n);
    }
    else if (op < 22) {
      obj r = TAG_RIB(alloc_rib2(TAG_NUM(n), NUM_0, pop()));
      DEC_COUNT(TAG(r)); // popped
      n = TAG_RIB(alloc_rib(r, NIL, CLOSURE_TAG));
      DEC_COUNT(r);
      i = 3;
      if (stack == NUM_0) {
        break;
      }
    }
    else if (op < 24){
      push2(TAG_RIB(inst_tail(RIB(TOS), n)), NUM_0);
      continue;
    }
    else if (op < 25){
      n = pop();
      i=4;
    }
    obj c = TAG_RIB(alloc_rib(TAG_NUM(i), n, NUM_0));
    DEC_COUNT(CDR(c));
    SET_TAG(c, TOS); //  c->fields[2] = TOS;
    SET_CAR(stack, TAG_RIB(c)); // TOS = TAG_RIB(c);
  }
  DEC_COUNT(n);
  set_pc(TAG(CAR(n))); // pc = TAG(CAR(n));
}
// )@@


// init global variables and stack

void set_global(obj c) {
  CAR(CAR(symbol_table)) = c;
  DEC_COUNT(c);
  set_sym_tbl(CDR(symbol_table)); // symbol_table = CDR(symbol_table);
}

// initialize primitive 0, FALSE, TRUE, and NIL
#define INIT_GLOBAL()                                                          \
  set_global(TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG)));            \
  set_global(FALSE);                                                           \
  set_global(TRUE);                                                            \
  set_global(NIL)

void init_stack() {
  push2(NUM_0, PAIR_TAG); 
  push2(NUM_0, PAIR_TAG); 

  obj first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;

  CAR(first) = TAG_NUM(INSTR_HALT);
  SET_CDR(first, NUM_0);
  TAG(first) = PAIR_TAG;
}

void init() {
  init_heap();
  INIT_FALSE();
  build_sym_table();
  decode();
  INIT_GLOBAL();
  init_stack();
  run();
}

#endif


#ifdef REF_COUNT

int main() { init(); }

#else

//==============================================================================

// Tests

obj __alloc_rib(obj car, obj cdr, obj tag) {

  obj tmp = *alloc; // next available slot in freelist
  obj from = alloc;
  
  // allocate memory
  *alloc++ = car;        // field 1
  *alloc++ = cdr;        // field 2
  *alloc++ = tag;        // field 3
  *alloc++ = _NULL;      // mirror 1
  *alloc++ = _NULL;      // mirror 2
  *alloc++ = _NULL;      // mirror 3
  *alloc++ = _NULL;      // co-friends
  *alloc++ = TAG_NUM(0); // rank
  *alloc++ = _NULL;      // queue
  *alloc++ = _NULL;      // priority queue

#ifdef BUCKETS
  *alloc++ = _NULL;
#endif

  // adjust ref counts
  if (IS_RIB(car)) {add_edge(from, car);}
  if (IS_RIB(cdr)) {add_edge(from, cdr);}
  if (IS_RIB(tag)) {add_edge(from, tag);}

  alloc = tmp;
  
  if (*alloc == _NULL) { // empty freelist?
    printf("heap is full, ya goofed\n");
  }
  
  return TAG_RIB(from);
}

void print_heap(){
  // to check manually if the tests are working properly
  scan=heap_bot;
  for (int i = 0; i <= MAX_NB_OBJS; i++) {
    printf("\nAddress = %lu\n", scan);
    printf("Field 1 = %lu\n", *scan++);
    printf("Field 2 = %lu\n", *scan++);
    printf("Field 3 = %lu\n", *scan++);
    printf("Mirror 1 = %lu\n", *scan++);
    printf("Mirror 2 = %lu\n", *scan++);
    printf("Mirror 3 = %lu\n", *scan++);
    printf("Parent = %lu\n", *scan++);
    printf("Rank = %d\n", UNTAG(*scan++));
    scan+=2; // skip queue and pqueue fields
  }
}

// this monster checks that the graph is back to its initial state after a test,
// you can (should) probably ignore this
#define CHECK_OG(r0,r1,r2,r3,r4,r5,r6,i,j)                              \
  if (CAR(r0) != TAG_NUM(0) || CDR(r0) != TAG_NUM(0) || TAG(r0) != TAG_NUM(0)||\
      M_CAR(r0) != _NULL || M_CDR(r0) != _NULL || M_TAG(r0) != _NULL ||        \
      get_parent(r0) != r1 || get_rank(r0) != 4 ||                             \
      CAR(r1) != r0 || CDR(r1) != TAG_NUM(1) || TAG(r1) != TAG_NUM(1) ||       \
      M_CAR(r1) != _NULL || M_CDR(r1) != _NULL || M_TAG(r1) != _NULL ||        \
      get_parent(r1) != r3 || get_rank(r1) != 3 ||                             \
      CAR(r2) != r1 || CDR(r2) != TAG_NUM(2) || TAG(r2) != TAG_NUM(2) ||       \
      M_CAR(r2) != _NULL || M_CDR(r2) != _NULL || M_TAG(r2) != _NULL ||        \
      get_parent(r2) != r3 || get_rank(r2) != 3 ||                             \
      CAR(r3) != r2 || CDR(r3) != TAG_NUM(3) || TAG(r3) != r1 ||               \
      M_CAR(r3) != r4 || M_CDR(r3) != _NULL || M_TAG(r3) != r2 ||              \
      get_parent(r3) != r5 || get_rank(r3) != 2 ||                             \
      CAR(r4) != TAG_NUM(4) || CDR(r4) != r2 || TAG(r4) != TAG_NUM(4) ||       \
      M_CAR(r4) != _NULL || M_CDR(r4) != _NULL || M_TAG(r4) != _NULL ||        \
      get_parent(r4) != r5 || get_rank(r4) != 2 ||                             \
      CAR(r5) != r4 || CDR(r5) != r3 || TAG(r5) != TAG_NUM(5) ||               \
      M_CAR(r5) != _NULL || M_CDR(r5) != _NULL || M_TAG(r5) != _NULL ||        \
      get_parent(r5) != r6 || get_rank(r5) != 1 ||                             \
      CAR(r6) != TAG_NUM(6) || CDR(r6) != TAG_NUM(6) || TAG(r6) != r5 ||       \
      M_CAR(r6) != _NULL || M_CDR(r6) != _NULL || M_TAG(r6) != _NULL ||        \
      get_parent(r6) != _NULL || get_rank(r6) != 0) {                          \
    printf("graph is not back to its original state after test %d.%d\n",i,j);  \
  }

int main() {
  // FIXME need a better test suite!
  
  init_heap();

  // Edge addition tests: building Olivier's example graph (somewhat)
  // Drawings provided on demand for free

  obj r0 = __alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  obj r1 = __alloc_rib(r0, TAG_NUM(1), TAG_NUM(1));
  obj r2 = __alloc_rib(r1, TAG_NUM(2), TAG_NUM(2));
  obj r3 = __alloc_rib(r2, TAG_NUM(3), r1);
  obj r4 = __alloc_rib(TAG_NUM(4), r2, TAG_NUM(4));
  obj r5 = __alloc_rib(r4, r3, TAG_NUM(5));
  obj r6 = __alloc_rib(TAG_NUM(6), TAG_NUM(6), r5);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,1,0);

  // Test 1.1: OK -- why we need to do a BFS when adding a new edge
  // Add an edge from r6 to r4, r4's parent becomes r6, r4's rank decreases
  // from 2 to 1, the edge between r4 and r2 becomes dirty and so r4 becomes
  // r2's parent and r'2 rank decreases from 3 to 2, r6's first mirror field
  // now points to r1, r4's second mirror field now points to r3 and r3's
  // first mirror field now points to _NULL
  SET_CAR(r6,r4);
  if (get_parent(r4) != r6 || get_rank(r4) != 1 || M_CAR(r6) != r5 ||
      get_parent(r2) != r4 || get_rank(r2) != 2 || M_CDR(r4) != r3 ||
      M_CAR(r3) != _NULL) {
    printf("Test 1.1 failed\n");
  }
  remove_edge(r6,r4);
  SET_CAR(r6,TAG_NUM(6));
  set_parent(r2,r3);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,1,1);
  

  //----------------------------------------------------------------------------

  // Edge deletion tests (and more)

  obj *tmp;

  // Test 2.1: OK -- Deallocation without children involved
  // Remove edge between r0 and r1, should remove r0 i.e. make alloc point to r0
  remove_edge(r1,r0);
  if (alloc != r0) {
    printf("Test 2.1 failed\n");
  }
  r0 = __alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  SET_CAR(r1,r0);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2,1);

  // Test 2.2: OK  -- No deallocation or children/parent involved
  // Remove edge between r2 and r1, this shouldn't deallocate any memory but
  // should remove r2 from r1's co-friends, i.e. mirror 3 of r3 should be _NULL
  tmp = alloc;
  remove_edge(r2,r1);
  if (alloc != tmp || CAR(r2) == r1 || M_TAG(r3) == r2) {
    printf("Test 2.2 failed\n");
  }
  SET_CAR(r2,r1);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2,2);

  // Test 2.3: OK -- Parent removal with deallocation, no children involved
  // Remove edge between r5 and r4, should dealloc r4 (i.e. make alloc point to
  // r4), and remove r4 from r2's co-friends (i.e. mirror 1 of r3 => _NULL)
  remove_edge(r5,r4);
  if (alloc != r4 || CAR(r5) == r4 || M_CAR(r3) == r4) {
    printf("Test 2.3 failed\n");
  }
  r4 = __alloc_rib(TAG_NUM(4), TAG_NUM(4), TAG_NUM(4));
  SET_CAR(r5,r4);
  SET_CDR(r4,r2);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2,3);

  // Test 2.4: OK -- Parent swap but no deallocation, children involved
  // Remove edge between r3 and r2, r4 "catches" r2 and becomes the parent and
  // so r3's first mirror field no longer points to r2
  tmp = alloc;
  remove_edge(r3,r2);
  if (alloc != tmp || get_parent(r2) != r4 || M_CAR(r3) == r4) {
    printf("Test 2.4 failed\n");
  }
  //print_heap();
  SET_CAR(r3,r2);
  set_parent(r2,r3);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2,4);

  // Test 2.5: OK --  Parent removal with deallocation and children involved
  // Olivier's example: remove edge betweem r5 and r3 => r3, r2, r1, and r0
  // all "drop", r3 is deallocated, r4 becomes r2's parent, r2 becomes r1's
  // parent, and the rank of both r1 and r0 decreases to 4 and 5, respectively
  remove_edge(r5,r3);
  if (alloc != r3 || get_parent(r2) != r4 || get_parent(r1) != r2 ||
      get_rank(r1) != 4 || get_rank(r0) != 5) {
    printf("Test 2.5 failed\n");
  }
  r3 = __alloc_rib(r2, TAG_NUM(3), r1);
  SET_CDR(r5,r3);
  set_parent(r2,r3);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2,5);

  // Test 2.6: OK -- Several deallocations
  // Same as above but this time the edge between r2 and r1 is deleted first
  // and so r2 doesn't "catch" r1 => r3, r1, and r0 all get deallocated
  remove_edge(r2,r1);
  remove_edge(r5,r3);
  if (alloc != r3 || get_parent(r2) != r4 || get_rank(r3) != -2 ||
      get_rank(r1) != -2 || get_rank(r0) != -2) {
    printf("Test 2.6 failed\n");
  }
  r3 = __alloc_rib(TAG_NUM(3), TAG_NUM(3), TAG_NUM(3));
  SET_CDR(r5,r3);
  SET_CAR(r3,r2);
  set_parent(r2,r3);
  r1 = __alloc_rib(TAG_NUM(1), TAG_NUM(1), TAG_NUM(1));
  r0 = __alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  SET_TAG(r3,r1);
  SET_CAR(r2,r1);
  SET_CAR(r1,r0);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2,6);
  
  //----------------------------------------------------------------------------
  
  // Cycle detection tests

  // Some tests to add (pretty sure some of them won't pass currently)
  //  - Cycle detection when a root is involved => need to make sure entire tree
  //    is not wiped out (which is probably what would happen right now)
  //  - Loops: ribs that point to themselves for whatever reason
  //  - Bigger and more sophisticated cycles
  //  - ...

  // Test 3.1: OK -- Simple cycle detection during deletion
  // Remove edge bewteen r1 and r0 after creating a cycle between r0 and the
  // newly allocated rib r7 => their ref count won't be 0 but they should both
  // be deallocated since they're no longer accessible from a root
  obj r7 = __alloc_rib(TAG_NUM(7), TAG_NUM(7), TAG_NUM(7));
  SET_CAR(r0,r7);
  SET_CDR(r7,r0); // cycle created
  remove_edge(r1,r0); // <- infinite loop in drop (see comment in remove_parent)
  if (alloc != r0 || *alloc != r7 || get_rank(r0) != -2 || get_rank(r7) != -2) {
    printf("Test 3.1 failed\n");
  }
  r0 = __alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  SET_CAR(r1,r0);
  r7 = __alloc_rib(TAG_NUM(7), TAG_NUM(7), TAG_NUM(7));
  SET_CAR(r0,r7);
  SET_CDR(r7,r0); // <- infinite loop in drop (see comment in remove_parent)

  // Test 3.2: OK -- Another object to be deallocated after cycle is detected
  // Same as above but this time we need to deallocate another rib before
  // breaking the loop where the cycle was detected (no longer applies)
  obj r8 = __alloc_rib(TAG_NUM(8), TAG_NUM(8), TAG_NUM(8));
  SET_TAG(r7,r8);
  remove_edge(r1,r0);
  if (get_rank(r0) != -2 || get_rank(r7) != -2 || get_rank(r8) != -2) {
    printf("Test 3.2 failed\n");
  }
  r0 = __alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  SET_CAR(r1,r0);
  r7 = __alloc_rib(TAG_NUM(7), TAG_NUM(7), TAG_NUM(7));
  SET_CAR(r0,r7);
  SET_CDR(r7,r0);
  r8 = __alloc_rib(TAG_NUM(8), TAG_NUM(8), TAG_NUM(8));
  SET_TAG(r7,r8);

  // Test 3.3: OK -- A rib is involved in two cycles
  // Same as the two above but now r8 also points to r7, meaning that r7 is
  // now part of two cycles (r0/r7 and r7/r8 cycles)
  SET_CAR(r8,r7); // new cycle created
  remove_edge(r1,r0);
  if (get_rank(r0) != -2 || get_rank(r7) != -2 || get_rank(r8) != -2) {
    printf("Test 3.3 failed\n");
  }
  
  return 0;
}

#endif
