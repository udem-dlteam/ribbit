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
 * stop & copy GC, DEFAULT_REPL_MIN, NOSTART, CHECK_ACCESS, etc.) but can be used with
 * the R4RS library.
 */

/* Know bugs:
 * - Co-friend not found when trying to remove it from a list of co-friend
 * - set_field and problem with when trying to protect with NIL
 * - Some rib's rank are not updated throught the program's execution 
 * - Infinite loop when a function is redefined, didn't investigate at all
 * - ... anything else?
 */

/* TODOs (other than fixing the bugs listed above, not in order of priority)
 * - Adapt original compression to ref count and ES
 * - Adapt strings/chars and list primitives for ref count and ES
 * - Adapt eval primitive to ref count and ES
 * - Adapt the IO primitives for ref count and ES garbage collector
 * - RC: Make sure all (non-cyclic) ribs are collected
 * - ES: Implement a better test suite
 * - ES: Check that my way to detect uncollected ribs in `gc` is not faulty
 * - ES: Make sure there's no redundant rank updates that slows down the execution time
 * - ES ... _NULL?
 * - ES: Only clear a rib's field during allocation or deallocation, not both
 * - ES: Only protect a popped rib if it would get deallocated otherwise 
 * - ES: Fix the potential bug in set_field 
 * - ES: Implement finalizers 
 * - ES: Re-evaluate my dealloc_rib strategy, might not be optimal
 * - ES: Add the "reference" encoding for chaining co-friends
 * - ES: Add co-friends by rank?
 * - ES: Find a way to deal with the PC updates
 * - ES: Find the source of the uncollected ribs with r4rs's display 
 * - ES: Merge the queue and priority queue in one field
 * - ES: Explore other priority queue implementations
 * - ES: Determine a good heuristic for heap size and a resizing strategy?
 * - ES: More efficient way to allocate the free list?
 * - ES: Add the missing features from the original RVM (esp. r4rs chars/strings)
 * - ES: Explore other ways than linking the null rib to protect popped ribs 
 * - ES: Optimizations, optimizations, and more optimizations 
 * - ... anything else?
 */

#include <stdio.h>
#include <stdlib.h>

// @@(feature ref-count
#define REF_COUNT 
// )@@

// @@(feature buckets
#define BUCKETS
// )@@

// @@(feature linked-list
#define LINKED_LIST
// )@@

// @@(feature test-es
#define TEST_ES
// )@@

// @@(feature debug/rib-viz
#define VIZ
// )@@

// @@(feature optimal-es
// To test some optimizations (e.g. deferred rank updates, etc.)
#define OPTIMAL_ES
// )@@

// @@(feature alt-encoding
// This feature activates the second encoding for the way we track references
// i.e. we track references themselves rather than co-friend when traversing
// the list of co-friends... this is used to compare the performance
#define ALT_ENCODING
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
#if defined(BUCKETS) || defined(LINKED_LIST) 
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
// mirror fields
#define M_CAR(x) RIB(x)->fields[3]
#define M_CDR(x) RIB(x)->fields[4]
#define M_TAG(x) RIB(x)->fields[5]
 // co-friends
#define CFR(x) RIB(x)->fields[6]
#define RANK(x) RIB(x)->fields[7]
// queue next
#define Q_NEXT(x) RIB(x)->fields[8]  

// priority queue
#ifdef BUCKETS
#define PQ_NEXT_RIB(x) RIB(x)->fields[9]
#define PQ_NEXT_BKT(x) RIB(x)->fields[10]
#else
// singly linked list
#define PQ_NEXT(x) RIB(x)->fields[9]
// #define PQ_NEXT(x) Q_NEXT(x)
// doubly linked list
#ifdef LINKED_LIST
#define PQ_PREV(x) RIB(x)->fields[10]
#endif
#endif
#endif

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

// Temp values that can be used to shield  pointers from the evil GC
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
#ifdef TEST_ES
#define MAX_NB_OBJS 14 
#else
#define MAX_NB_OBJS 100000000
#endif
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_top (heap_bot + (SPACE_SZ))


//==============================================================================

// Debugging

// TODO
// print_cfr
// print_queue
// print_pqueue

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
#ifdef REF_COUNT
    obj rank = scan[3];
#else
    obj rank = scan[7];
#endif
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
      viz_add_rib_label(current_graph, ((obj)scan), scan[0], scan[1], scan[2], scan[3], scan[4], scan[5], scan[6], rank);
    }
    scan-=RIB_NB_FIELDS;
  }
  viz_end_graph(current_graph);
  // exit(1);
}

#endif

#ifndef REF_COUNT

//==============================================================================

// Data structures (ESTrees)


// Queue

obj q_head; // dequeue at head => O(1)
obj q_tail; // enqueue at tail => O(1)

#define Q_INIT() q_head = _NULL; q_tail = _NULL

#define Q_IS_EMPTY() (q_head == _NULL)

void q_enqueue(obj o) {
  if (Q_NEXT(o) == _NULL && q_tail != o) { // no duplicates in the queue
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
//  - Heap
//
//  - Something else...?


// This section is a bit of a mess right now, I played with a lot of stuff,
// need to cleanup and make sur the pqueue is implemented properly


obj pq_head;
obj pq_tail;

#define PQ_INIT() pq_head = _NULL; pq_tail = _NULL

#define PQ_IS_EMPTY() (pq_head == _NULL)

#ifdef BUCKETS

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

#ifdef LINKED_LIST

// Priority queue implemented with a doubly linked list

// No idea why this is (much) slower than the singly linked list
// given that the delete operation (for the set) is now in O(1)
// instead of O(n). Leaving this here for now because I probably
// missed something

void pq_enqueue(obj o) {
  // the lower the rank, the closer the rib is to pq_head
  if (PQ_PREV(o) == _NULL && pq_head != o) {
    if (PQ_IS_EMPTY()){
      pq_head = o;
    }
    else if (RANK(o) == 1) { // root (tagged rank)
      PQ_PREV(pq_head) = o;
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
      PQ_NEXT(prev) = o; // could be _NULL
      if (curr != _NULL) { // o becomes the tail?
        PQ_PREV(curr) = o;
        PQ_NEXT(o) = _NULL;
      }
      PQ_PREV(o) = prev;
    }
  }
}

obj pq_dequeue() {
  if (PQ_IS_EMPTY()){
    return _NULL;
  }
  obj tmp = pq_head;
  pq_head = PQ_NEXT(tmp);
  if (pq_head != _NULL) {
    PQ_PREV(pq_head) = _NULL;
    PQ_NEXT(tmp) = _NULL;
  }
  return tmp;
}

// Set operations

void pq_remove(obj o) {
  if (PQ_PREV(o) != _NULL) { // in pqueue but not pq_head
    PQ_NEXT(PQ_PREV(o)) = PQ_NEXT(o); // could be _NULL
    if (PQ_NEXT(o) != _NULL) { // not last element in pqueue
      PQ_PREV(PQ_NEXT(o)) = PQ_PREV(o);
      PQ_NEXT(o) = _NULL;
    }
    PQ_PREV(o) = _NULL;
  } else if (pq_head == o) { // pq_head, else o is not in set
    pq_head = _NULL;
  }
}

#else

// Priority queue implemented with a singly linked list

// In the previous version of the singly linked list, I had a bug
// where I would never add an element to the pqueue if it was the
// last element before the tail in the priority queue. This is
// obviously wrong but somehow all the programs that I tested
// worked (although they were quite simple) and the execution time
// was about 30% faster (and the same number of rib was deallocated)

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

// Set operations

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

int get_mirror_field(obj x, obj cfr) {
  for (int i = 0; i < 3; i++){
    if (get_field(cfr,i) == x) { // && get_field(cfr,i+3) != cfr) {
      return i+3;
    }
  }
  return -1; // cfr is not x's co-friend
}

// Saves us a few seconds in execution time if we define these as macros

#define get_rank(x) (NUM(RANK(x)))
#define set_rank(x, rank) (RANK(x) = TAG_NUM(rank))
#define is_root(x) (x == stack || x == pc || x == FALSE)
// FIXME uncollected ribs when an object of rank 0 or the symmbol table is considered a root 
// #define is_root(x) (get_rank(x) == 0 || x == stack || x == pc || x == FALSE || x == symbol_table)
#define is_dirty(from, to) (get_rank(from) < (get_rank(to) - 1))
#define next_cofriend(x, cfr) (get_field(cfr, get_mirror_field(x, cfr)))
#define is_parent(x, p) (CFR(x) == p)
#define get_parent(x) CFR(x)

/* bool is_root(obj x) { */
/*   return (x == stack || x == pc || x == FALSE || x == symbol_table); */
/* } */

/* int get_rank(obj x) { */
/*   return NUM(RANK(x)); */
/* } */

/* void set_rank(obj x, int rank) { */
/*   RANK(x) = TAG_NUM(rank); */
/* } */

/* bool is_dirty(obj from, obj to) { */
/*   return (get_rank(from) < (get_rank(to) - 1)); */
/* } */

/* obj next_cofriend(obj x, obj cfr) { */
/*   // get next co-friend in x's list of co-friends (following cfr) */
/*   return get_field(cfr, get_mirror_field(x, cfr)); */
/* } */

/* bool is_parent(obj x, obj p) { */
/*   return (CFR(x) == p);  */
/* } */

/* obj get_parent(obj x) { */
/*   return CFR(x); */
/* } */

void set_parent(obj child, obj new_parent, int k) {
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
  /* int i = get_mirror_field(child, prev); */
  /* int j = get_mirror_field(child, curr); */
  /* get_field(prev, i) = get_field(curr, j); // remove new_parent from cfr chain */
  /* get_field(curr, j) = old_parent; // make new parent point to old parent */
  /* CFR(child) = new_parent; // new parent becomes... the new parent */
}

/* void remove_parent(obj x, obj p, int i) { */
/*   // FIXME this could be problematic since we don't tag the parent */
/*   // and the next co-friend is not technically the parent... */
/*   CFR(x) = get_field(p,i+3); */
/*   get_field(p,i+3) = _NULL; */

/*   // FIXME see test 3.1 to see how this could happen: after adding the edges */
/*   // r0->r7 and then r7->r0, r7 will become r0's only co-friend (other than */
/*   // r1 which is his parent) even though r0 is r7's parent. When removing the */
/*   // edge r1->r0, r7 will also become r0's parent. This parent/child cycle */
/*   // will create an infinite loop in the drop phase... needs to be handled */
/*   // somehow and this is probably not the proper way to do it... */
/*   /\* if (CFR(x) != _NULL && is_parent(CFR(x), x)) { *\/ */
/*   /\*   // should probably check if there's another co-friend and wrap this in *\/ */
/*   /\*   // a loop until we find a co-friend that's not a child and if there's *\/ */
/*   /\*   // no such co-friend, THEN we assign CFR(x) = _NULL ... FIXME *\/ */
/*   /\*   CFR(x) = _NULL; *\/ */
/*   /\* } *\/ */
/* } */

void remove_parent(obj x, obj p, int i) {
  // FIXME this could be problematic since we don't tag the parent
  // and the next co-friend is not technically the parent...

  // Only remove the parent if it points only once to the child, else
  // just set the mirror field to _NULL

  // Case 1: 
  for (int j = 0; j < 3; j++) {
    if (j == i) continue;
    if (get_field(p, j) == x) {
      get_field(p, i+3) = _NULL;
      return;
    }
  }
  // Case 2: Only one ref from p to x
  get_parent(x) = get_field(p, i+3); // get next co-friend
  get_field(p, i+3) = _NULL; // set paren't mirror field to _NULL
}

/* void add_cofriend(obj x, obj cfr, int j) { */
/*   // FIXME do we want the co-friends to be ordered by rank? for now the new */
/*   // co-friend is just inserted between the parent and the following co-friend */
/*   obj p = get_parent(x); */
/*   if (p == _NULL) { */
/*     set_parent(x, cfr, j); */
/*     set_rank(x, get_rank(cfr)+1); */
/*     return; */
/*   } */
/*   int i = get_mirror_field(x, p); */
/*   obj tmp = get_field(p,i); // old co-friend pointed by parent */
/*   get_field(p,i) = cfr; */
/*   // get_field(cfr, get_mirror_field(x, cfr)) = tmp; */
/*   get_field(cfr, j+3) = tmp; */
/* } */

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

/* void add_cofriend(obj x, obj cfr) { */
/*   // cofriends are inserted by rank, this way we can easily check if */
/*   // we're trying to add a duplicate */
/*   obj p = get_parent(x); */
/*   if (p == _NULL) { */
/*     set_parent(x, cfr); */
/*     set_rank(x, get_rank(cfr)+1); */
/*     return; */
/*   } */
/*   obj prev = _NULL; */
/*   obj curr = p; */
/*   while (curr != _NULL && curr != cfr && get_rank(curr) <= get_rank(cfr)) { */
/*     prev = curr; */
/*     curr = next_cofriend(x, curr); */
/*   } */
/*   if (curr == cfr) return; // duplicates */
/*   if (prev == _NULL) { */
/*     CFR(x) = cfr; // new parent; */
/*   } */
/*   if (prev == _NULL) { */
/*     // cfr will become the new parent FIXME redundant */
/*     CFR(x) = cfr; */
/*   } else { */
/*     // insert cfr between prev and curr */
/*     get_field(prev, get_mirror_field(x, prev)) = cfr; */
/*   } */
/*   get_field(cfr, get_mirror_field(x, cfr)) = curr; */
/* } */

/* void remove_cofriend(obj x, obj cfr, int k) { */
/*   // assumes that cfr is not x's parent */
/*   obj curr = CFR(x); */
/*   obj prev; */
/*   obj tmp = NUM_0; */
/*   while (curr != cfr && curr != _NULL) { */
/*     prev = curr; */
/*     curr = next_cofriend(x, curr); */
/*   } */
/*   if (curr == _NULL) { */
/*     // FIXME cfr was not a co-friend of x, not sure if that's a bug */
/*     return; */
/*   } */
/*   int i = get_mirror_field(x, curr); */
/*   int j = get_mirror_field(x, prev); */
/*   obj tmp2 = get_field(curr,i); */
/*   get_field(curr,i) = _NULL; // remove reference to next co-friend */
/*   get_field(prev,j) = (tmp2 != x) ? tmp2 : _NULL; */
/* } */

void foo(){}

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
  if (curr == _NULL) {
    // FIXME cfr was not a co-friend of x, not sure if that's a bug
    /* if (cfr != null_rib) { */
    /*   foo(); */
    /*   printf("couldn't find the cofriend when trying to remove\n\n"); */
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

      // FIXME the `c[i] != root` condition is necessary to sometimes avoid
      // an infinite loop when we update the ranks after setting a new root
      // but not sure if that should also be applied in add_edge?
      if (IS_RIB(c[i]) && c[i] != root && (!is_root(c[i]))) {

        // Only update ranks if the parent's rank was updated or if the edge
        // is dirty
        // FIXME we could avoid A LOT of overhead if we could avoid some updates
        // here without changing how many ribs gets collected, also should note
        // that the algorithm enqueues EVERY friends, which I don't do here to
        // avoid the insane overhead of doing that
        if (is_parent(c[i], curr) && (get_rank(curr) != (get_rank(c[i])+1))) {
          set_rank(c[i], r);
          q_enqueue(c[i]);
        } else if (is_dirty(curr, c[i])) {
          set_parent(c[i], curr, i); 
          set_rank(c[i], r);
          q_enqueue(c[i]);
        }
      }
    }
  } while (!Q_IS_EMPTY());
}

void add_edge(obj from, obj to, int i) {
  add_cofriend(to, from, i);
  if (is_dirty(from, to)) {
    set_parent(to, from, i);
  }
  // The reference from a new root to an existing rib is not considered dirty
  // so we need to have the `is_parent` check to account for that situation
  // (the check passes if the edge was dirty as well)... this shouldn't be a
  // problem for ribbit but I'll leave it there for now
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

#define loosen(x) set_rank(x, -1); pq_remove(x)

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
      if (IS_RIB(_x[i]) && is_parent(_x[i], x) && (!is_root(_x[i]))) {
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
  do {
    obj anker = pq_dequeue();
    obj *_anker = RIB(anker)->fields;
    for (int i = 0; i < 3; i++) {
      if (IS_RIB(_anker[i]) && (get_rank(_anker[i]) == -1)) {
        set_parent(_anker[i], anker, i);
        set_rank(_anker[i], get_rank(anker)+1);
        pq_enqueue(_anker[i]); // add rescued node to potential "catchers"
      }
    }
  } while (!PQ_IS_EMPTY());
}

void remove_edge(obj from, obj to, int i); // cos no header file

void dealloc_rib(obj x){
  set_rank(x, -2); // deallocated rib, this way we just ignore cycles
  obj *_x = RIB(x)->fields;
  for (int i = 0; i < 3; i++) {
    if (IS_RIB(_x[i])) {
      if (get_rank(_x[i]) == -1) { // falling?
        dealloc_rib(_x[i]);
      }
      else { // if (get_rank(_x[i]) != -2) {
        remove_edge(x, _x[i], i);
      }
    }
  }
  CAR(x) = (obj)alloc; // deallocate the rib by adding it to the freelist
  alloc = (obj *)x;
  d_count++;
  // simplifies the generated graph (assumes singly linked list)
  _x[1] = _NULL;
  _x[2] = _NULL;
  _x[3] = _NULL;
  _x[4] = _NULL;
  _x[5] = _NULL;
  _x[6] = _NULL;
  _x[7] = TAG_NUM(0);
  _x[8] = _NULL;
  _x[9] = _NULL; // FIXME assumes singly linked list
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
#ifdef TEST_ES
    // TODO remove, just used my tests but this will be done elsewhere in the GC
    if (get_field(from, i) == to) {
      RIB(from)->fields[get_mirror_field(to,from)-3] = TAG_NUM(0);
    }
#endif
    return;
  }
    
  remove_parent(to, from, i); // `to` is "parentless"

  // Second condition happens when we remove an edge between a node and his
  // parent but the parent points to the child more than once
  if (!is_root(to) && (!is_parent(to, from))) {
    // TODO remove, same as above
#ifdef TEST_ES
    if (get_field(from, i) == to) {
      RIB(from)->fields[get_mirror_field(to,from)-3] = TAG_NUM(0);
    }
#endif
    // Q_INIT(); // drop queue i.e. "falling ribs"
    // PQ_INIT(); // ankers i.e. potential "catchers"
  
    q_enqueue(to);
  
    drop();
    if (!PQ_IS_EMPTY()) catch(); // avoid function call if no catchers
    if (get_parent(to) == _NULL) {
      dealloc_rib(to);
    }
  }
}

// FIXME should we just assume that `from` is a rib to avoid the type check?
#define remove_ref(from, to, i) if (IS_RIB(to)) remove_edge(from, to, i)

// FIXME need to generalize this so that it works with any protected field
#define remove_ref_nr(to, i) remove_edge(null_rib, to, i); TEMP5 = _NULL;


//------------------------------------------------------------------------------

// Specific node deletion

void remove_root(obj old_root) {
  if (IS_RIB(old_root)) {
    if (CFR(old_root) == _NULL) {
      // Q_INIT(); // drop queue i.e. "falling ribs"
      // PQ_INIT(); // ankers i.e. potential "catchers"      
      q_enqueue(old_root);
      drop();
      if (!PQ_IS_EMPTY()) catch(); // avoid function call if no catchers
      dealloc_rib(old_root);
    }
    else {
      set_rank(old_root, get_rank(CFR(old_root))+1);

      // FIXME FIXME FIXME FIXME FIXME
      // This is essential (I think) for the GC to collect all ribs but it slows
      // down the execution time.... A LOT, need to find a way to reduce that
      
      // IMPORTANT: doing the rank update allows the GC to collect all rib when
      // we run the tests with `max-tc` but this is the only library that
      // requires this to collect all ribs, probably should check if we can
      // do the rank update ONLY under certain circumstances 
      update_ranks(old_root);
    }
  }
}

void remove_stack(obj old_root) {
  // Similar to remove_root but specialized for the stack pointer which has at
  // most two rib pointers (one of which will be the new stack). Doing the
  // deallocation of the old stack directly in this function and calling
  // remove_ref directly on the first two fields is faster than going directly
  // through remove_edge or using remove_root. This might seem like a trivial
  // optimization but that saves us a few  million iterations even for just a
  // small program given how often we change the stack pointer. The same
  // optimization for the pc pointer doesn't change anything.
  if (IS_RIB(old_root)) {
    if (CFR(old_root) == _NULL) { // deallocate old stack
      set_rank(old_root, -2);
      remove_ref(old_root, CAR(old_root), 0);
      remove_ref(old_root, CDR(old_root), 1);
      CAR(old_root) = (obj)alloc;
      CDR(old_root) = _NULL;
      alloc = (obj *)old_root;
      d_count++;
      obj *_x = RIB(old_root)->fields;
      
      // FIXME redundant with rib allocation
      _x[1] = _NULL;
      _x[2] = _NULL;
      _x[3] = _NULL;
      _x[4] = _NULL;
      _x[5] = _NULL;
      _x[6] = _NULL;
      _x[7] = TAG_NUM(0);
      _x[8] = _NULL;
      _x[9] = _NULL; // FIXME assumes singly linked list
    } else {
      set_rank(old_root, get_rank(CFR(old_root))+1);
      
      // FIXME same as above
      update_ranks(old_root);
    }
  }
}

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

void set_field(obj src, int i, obj dest) { // write barrier
  // The order differs a bit from the ref count version of the write barrier...
  // TODO explain why we're doing that this way
  obj *ref = RIB(src)->fields;
  
  if (IS_RIB(ref[i])) { // no need to dereference _NULL or a num
    if (next_cofriend(ref[i], src) == _NULL) {
      // We need to be more careful here since simply removing the edge
      // between src and ref[i] will deallocate ref[i] and potentially
      // some other ribs refered by ref[i]. This is problematic if dest
      // contains a reference to one of ref[i]'s children (or more) since
      // we'll deallocate a rib (or more) that shouldn't be deallocated.
      // We can get around that by using a temporary rib to point to ref[i]
      // (if we give that temporary rib the same rank as src we avoid
      // potentially changing the structure of our graph twice)
      obj tmp = ref[i];
      set_rank(NIL, get_rank(src));
      TEMP3 = ref[i];
      add_edge(NIL, ref[i], 0);
      remove_ref(src, ref[i], i);
      ref[i] = dest;
      add_ref(src, dest, i);
      remove_ref(NIL, tmp, 0);
      TEMP3 = _NULL;
      set_rank(NIL, 1);
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
  remove_root(old_sym_tbl);
  if (IS_RIB(symbol_table)) set_rank(symbol_table, 0);

  /* // FIXME integrate this in the ES logic */
  update_ranks(symbol_table);
}

void set_stack(obj new_stack) {
  obj old_stack = stack;
  stack = new_stack;
  if (IS_RIB(stack)) set_rank(stack, 0);
  remove_stack(old_stack);
  
  // FIXME integrate this in the ES logic
  if (IS_RIB(stack)) update_ranks(stack);

  // FIXME not sure why this is not working
  /* if (IS_RIB(stack)) { */
  /*   obj *_stack = RIB(stack)->fields; */
  /*   for (int i = 0; i < 3; i++) { */
  /*     if (IS_RIB(_stack[i])) { */
  /*    // FIXME not sure how this will behave when pc and stack are both */
  /*    // pointing to the same rib */
  /*    if (!is_parent(_stack[i], stack)) { */
  /*      set_parent(_stack[i], stack, i); */
  /*      set_rank(_stack[i], 1); */
  /*      update_ranks(_stack[i]); */
  /*    } */
  /*     } */
  /*   } */
  /* } */
}

void set_pc(obj new_pc) {
  obj old_pc = pc;
  pc = new_pc;
  set_rank(pc, 0);
  remove_root(old_pc);
  
  // FIXME integrate this in the ES logic
  // update_ranks(pc);

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
}

#ifdef OPTIMAL_ES

void set_stack2(obj new_stack) {
  obj old_stack = stack;
  stack = new_stack;
  if (IS_RIB(stack)) set_rank(stack, 0);
  remove_stack(old_stack);

  /* if (IS_RIB(stack)) { */
  /*   obj *_stack = RIB(stack)->fields; */
  /*   for (int i = 0; i < 3; i++) { */
  /*     if (IS_RIB(_stack[i])) { */
  /*    if (!is_parent(_stack[i], stack)) { */
  /*      set_parent(_stack[i], stack, i); */
  /*    } */
  /*     } */
  /*   } */
  /* } */
}

void set_pc2(obj new_pc) {
  obj old_pc = pc;
  pc = new_pc;
  if (IS_RIB(pc)) set_rank(pc, 0);
  remove_root(old_pc);

  obj *_pc = RIB(pc)->fields;
  for (int i = 0; i < 3; i++) {
    if (IS_RIB(_pc[i])) {
      if (!is_parent(_pc[i], pc)) {
        set_parent(_pc[i], pc, i);
      }
    }
  }
}

#else

#define set_stack2(new_stack) set_stack(new_stack)
#define set_pc2(new_pc) set_pc(new_pc)

#endif
#endif


//==============================================================================

// RVM

//------------------------------------------------------------------------------

// Cycle detection and collection (ref count)

int leftovers = 0;

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
#ifdef REF_COUNT
      if (RIB((obj)scan)->fields[3] != 0) leftovers++;
#else
      if (get_rank((obj)scan) != 0) leftovers++;
#endif
      *scan = (obj)alloc;
      alloc = scan;
#ifdef VIZ
      scan++;
      *scan++ = _NULL;
      *scan++ = _NULL;
#ifdef REF_COUNT
      *scan++ = TAG_NUM(0);
#else
      *scan++ = _NULL;
      *scan++ = _NULL;
      *scan++ = _NULL;
      *scan++ = _NULL;
      *scan++ = TAG_NUM(0);
      *scan++ = _NULL;
      *scan++ = _NULL;
#endif
      scan -= RIB_NB_FIELDS;
#endif
    }
    scan += RIB_NB_FIELDS; // next rib object
  }
  printf("uncollected ribs = %d\n", leftovers);
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

// Can we just set the rank of the popped object to 0 to avoid having it being
// deallocated (i.e. make it a temporary root), it's a bit faster this way
obj pop() {
  obj tos = CAR(stack);
  if (IS_RIB(tos) && M_CAR(stack) == _NULL) {
    // protect TOS only if it gets deallocated otherwise
    TEMP5 = tos;
    add_edge(null_rib, tos, 0);
  }
  // if (IS_RIB(tos)) set_rank(tos, 0);
  set_stack(CDR(stack));
  return tos;
}

// to avoid too many preprocessor instructions in the RVM code
#define DEC_POP(o) if (TEMP5 == o) remove_ref_nr(o, 0)


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
  *alloc++ = _NULL;      // queue
  *alloc++ = _NULL;      // priority queue
#if defined(BUCKETS) || defined(LINKED_LIST) 
  *alloc++ = _NULL;
#endif

  obj new_rib = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));

  add_ref(new_rib, stack, 1);
  set_stack(new_rib);
  
  add_ref(new_rib, car, 0);
  add_ref(new_rib, tag, 2);
  
  alloc = (obj *)tmp;
}

// We don't need to link a newly allocated rib from the stack since we
// don't trigger a GC cycle when allocating a new rib: since we deallocate
// objects instantly when they're no longer needed (even if they're part of
// a cycle) then the number of live objects at all time in the program
// corresponds to the maximum number of objects allowed by the program.

// Note that assumes that we don't defer deallocation and that we in fact
// collect all the objects once they're no longer needed otherwise we'll
// trigger a GC cycle and the newly allocated rib might get collected

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
  *alloc++ = _NULL;      // queue
  *alloc++ = _NULL;      // priority queue
#if defined(BUCKETS) || defined(LINKED_LIST) 
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
    PRIM3();
#ifdef REF_COUNT
    obj new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
    CAR(new_rib) = x;
    CDR(new_rib) = y;
    TAG(new_rib) = z;
    push2(new_rib, PAIR_TAG);
    DEC_COUNT(new_rib); // remove redundant new_rib count
#else
    push2(TAG_RIB(alloc_rib(x, y, z)), PAIR_TAG);
    DEC_PRIM3();
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

#define ADVANCE_PC() set_pc(TAG(pc))

void run() { // evaluator
  while (1) {
    num instr = NUM(CAR(pc));
    // printf("instr = %d\n", instr);
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
#ifdef REF_COUNT
            set_pc(get_cont());
#else
            set_pc2(get_cont()); // FIXME can we avoid updating the ranks here?
#endif
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
#ifdef REF_COUNT
      ADVANCE_PC();
#else
      // FIXME no rank updates?
      set_pc2(TAG(pc)); // ADVANCE_PC();
#endif
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
      printf("deallocation count = %d\n", d_count);
      /* viz_heap("graph.dot"); */
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
  // rank should always be 0 since popped values will be saved there temporarly
  set_rank(null_rib, 0);
#endif
  while (scan != heap_bot) {
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
// FIXME infinite loop here when we use SET_CAR for the intial primitive, we
// create a cycle so we keep enqueuing in add_edge->set_field ad infinitum
#define INIT_GLOBAL()                                                          \
  obj tmp = TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG));              \
  CAR(CAR(symbol_table)) = tmp;                                                \
  obj tmp2 = CAR(symbol_table);                                                \
  remove_ref(symbol_table, tmp2, 0);                                           \
  add_edge(tmp2, tmp, 0);                                                      \
  add_edge(symbol_table, tmp2, 0);                                             \
  set_sym_tbl(CDR(symbol_table));                                              \
  set_global(FALSE);                                                           \
  set_global(TRUE);                                                            \
  set_global(NIL);                                                             \
  update_ranks(tmp);                                                           \
  set_rank(symbol_table, 0);                                                   \
  update_ranks(symbol_table)
#endif

void init_stack() {
  push2(NUM_0, PAIR_TAG);
  push2(NUM_0, PAIR_TAG); 
  
  obj first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;
#ifndef REF_COUNT
  add_ref(stack, first, 2);
#endif

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

#ifndef TEST_ES
 
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
#if defined(BUCKETS) || defined(LINKED_LIST) 
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

#define _INIT_FALSE()                                                           \
  FALSE = TAG_RIB(__alloc_rib(TAG_RIB(__alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),\
                              TAG_RIB(__alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),\
                              SINGLETON_TAG));

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

  _INIT_FALSE();

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
  remove_edge(r6,r4,0);
  SET_CAR(r6,TAG_NUM(6));
  set_parent(r2,r3);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,1,1);

  
  // Test 1.2: ?? -- a rib pointing to the same rib twice
  

  //----------------------------------------------------------------------------

  // Edge deletion tests (and more)

  obj *tmp;

  // Test 2.1: OK -- Deallocation without children involved
  // Remove edge between r0 and r1, should remove r0 i.e. make alloc point to r0
  remove_edge(r1,r0,0);
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
  remove_edge(r2,r1,0);
  if (alloc != tmp || CAR(r2) == r1 || M_TAG(r3) == r2) {
    printf("Test 2.2 failed\n");
    // print_heap();
  }
  SET_CAR(r2,r1);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2,2);

  // Test 2.3: OK -- Parent removal with deallocation, no children involved
  // Remove edge between r5 and r4, should dealloc r4 (i.e. make alloc point to
  // r4), and remove r4 from r2's co-friends (i.e. mirror 1 of r3 => _NULL)
  remove_edge(r5,r4,0);
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
  remove_edge(r3,r2,0);
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
  remove_edge(r5,r3,1);
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
  remove_edge(r2,r1,0);
  remove_edge(r5,r3,1);
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

  // Edge "replacement" tests

  // Test 3.1: OK -- replace a reference by another one, no deallocation
  // Replace edge (r3, r2) by (r3, r0) (SET_CAR(r3,r0)): r2's parent will become
  // r4, r0's rank will decrease to 3 and its parent will become r3, and r3's
  // first mirror field will point to r1 instead of r4.
  SET_CAR(r3,r0);
  if (get_parent(r2) != r4 || CAR(r3) != r0 || M_CAR(r3) != r1 ||
      get_parent(r0) != r3 || get_rank(r0) != 3) {
    printf("Test 3.1 failed\n");
  }
  SET_CAR(r3,r2);
  set_parent(r2,r3); // not an error, just depends on the order

  
  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,3,1);


  // Test 3.2: ?? -- replace reference but with deallocation (write barrier)
  // Remove edge (r3,r1) and then replace edge (r3,r1) by (r3,r0). The trick
  // here is that if the write barrier deallocates r1 before adding the edge
  // (r3,r0), r0 will be deallocated because r1 itself is being deallocated
  // (r1 is r0's only cofriend/parent at this stage and r1's other co-friend,
  // r2, is no longer is co-friend after removing the first edge).
  remove_edge(r2,r1,0);
  SET_TAG(r3,r0);
  if (CAR(r2) == r1 || TAG(r3) != r0 || M_TAG(r3) != _NULL ||
      get_rank(r1) != -2 || get_rank(r0) != 3 || get_parent(r0) != r3) {
    printf("Test 3.2 failed\n");
  }
  r1 = __alloc_rib(TAG_NUM(1), TAG_NUM(1), TAG_NUM(1));
  SET_CAR(r1,r0);
  SET_TAG(r3,r1);
  SET_CAR(r2,r1);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,3,2);
  
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
  remove_edge(r1,r0,0); // <- infinite loop in drop (see comment in remove_parent)
  if (alloc != r0 || *alloc != r7 || get_rank(r0) != -2 || get_rank(r7) != -2) {
    printf("Test 4.1 failed\n");
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
  remove_edge(r1,r0,0);
  if (get_rank(r0) != -2 || get_rank(r7) != -2 || get_rank(r8) != -2) {
    printf("Test 4.2 failed\n");
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
  remove_edge(r1,r0,0);
  if (get_rank(r0) != -2 || get_rank(r7) != -2 || get_rank(r8) != -2) {
    printf("Test 4.3 failed\n");
  }
  
  return 0;
}

#endif
