/* 
 * File: es.c
 *
 * Author: Frédéric Lahaie-Bertrand
 *
 * Implementation of Even-Shiloach trees
 */


#include <stdio.h>
#include <stdlib.h>


// basic def. of a boolean
typedef unsigned char bool;

// a tagged value
typedef unsigned long obj;

// a number
typedef long num;

#define RIB_NB_FIELDS 10
typedef struct {
  obj fields[RIB_NB_FIELDS];
} rib;

// GC constants
rib *heap_start;
#define MAX_NB_OBJS 10
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_top (heap_bot + (SPACE_SZ))
#define _NULL ((obj) NULL)
// end GC constants

obj *alloc; 
obj *scan;

#define UNTAG(x) ((x) >> 1)
#define RIB(x) ((rib *)(x))
#define NUM(x) ((num)(UNTAG((num)(x))))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x))
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))
#define TAG_NUM(num) ((((obj)(num)) << 1) | 1)

/* #define IS_CHILD(x) ((x)&2) */
/* #define TAG_CHILD(x) ((x)|2) // assumes x is a tagged obj (ul) */
/* #define UNTAG_CHILD(x) ((x)^2) */

#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]

// mirror fields for co-friends lists
#define M_CAR(x) RIB(x)->fields[3]
#define M_CDR(x) RIB(x)->fields[4]
#define M_TAG(x) RIB(x)->fields[5]

// rib's own co-friends
#define CFR(x) RIB(x)->fields[6]

// rank
#define RANK(x) RIB(x)->fields[7]

// queue and pqueue, respectively
#define Q_NEXT(x) RIB(x)->fields[8]
#define PQ_NEXT(x) RIB(x)->fields[9]

// Structure of a rib:
// 
//    +------------+
// 1  |   Field 1  |
//    +------------+
// 2  |   Field 2  |
//    +------------+
// 3  |   Field 3  |
//    +------------+
// 4  |  Mirror 1  | 
//    +------------+
// 5  |  Mirror 2  |
//    +------------+
// 6  |  Mirror 3  |
//    +------------+
// 7  | Co-friends | 
//    +------------+
// 8  |    Rank    | 
//    +------------+
// 9  |  Next (Q)  |
//    +------------+
// 10 |  Next (PQ) |
//    +------------+


//==============================================================================

// Data structures 


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
//    but insertion and deletion is in O(n)
//
//  - Buckets: O(#ranks) for insertion and deletion but requires 2 extra fields
//    to keep a reference to the next rib of the same bucket (of the same rank)
//    and one to keep a reference of the first rib in the next bucket (bucket
//    with rank+=1). This is the best option (I think) if we can show that
//    #ranks <= lg(n)
//
//  - Red-black trees: O(lg(n)) for insertion and deletion but requires 3 extra
//    fields: 2 for the left and right subtrees and 1 for the parent. Tagging
//    can be used for the colour. This is a better option than buckets when
//    #ranks > lg(n)
//
//  - Something else...?

obj pq_head;

void pq_init() {
  pq_head = _NULL;
}

#define PQ_IS_EMPTY() (pq_head == _NULL)

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
  // FIXME need a faster way to detect if `o` is not in the pqeueue
  if (pq_head == _NULL) { // || (pq_head != _NULL && PQ_NEXT(*o) == _NULL)) {
    // empty pqueue (set) or the rib is not in the pqueue (set)
    return;
  }
  obj curr = pq_head;
  obj prev;
  while (curr != o && curr != _NULL) {
    prev = curr;
    curr = PQ_NEXT(curr);
  }
  if (curr == pq_head) { 
    pq_head = _NULL;
    return;
  } else if (curr == _NULL) { // not found in set
    return;
  }
  PQ_NEXT(prev) = PQ_NEXT(curr);
  PQ_NEXT(curr) = _NULL;
}


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

// My implementation does a DFS instead of a BFS. This works because only the
// ribs accessible via their parents can potentially have their rank modified
// and since the `to` rib is the root of the subtree of the "rank update
// traversal", we can safely assume that the ranks will always be updated in the
// right order (or else a rib could have more than one parent). This means that
// we don't need to use a queue and that, given the similarity of this procedure
// with the marking phase of a mark-and-sweep GC, we can probably use a graph
// marking algorithm (like the Deutsch-Schorr-Waite algorithm) to avoid the
// recursive function calls

void update_rank(obj x){
  // update rank of x's children recursively
  set_rank(x, get_rank(get_parent(x))+1);
  obj *_x = RIB(x)->fields;
  for (int i = 0; i < 3; i++) {
    // FIXME? not sure if the is_parent check is slower than tagging the child
    if (IS_RIB(_x[i]) && is_parent(_x[i], x)) {
      update_rank(_x[i]);
    }
  }
}

void add_edge(obj from, obj to) {
  // FIXME duplicate edges (refs) are allowed for now, could be problematic
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
      set_rank(to, get_rank(from)+1);
      obj *_to = RIB(to)->fields;
      for (int i = 0; i < 3; i++) {
        if (IS_RIB(_to[i]) && is_parent(_to[i], to)) {
          update_rank(_to[i]);
        }
      }
    }
  }
}


//------------------------------------------------------------------------------

// Edge deletion (i.e. removing a reference to a rib)

// Intuition: TODO

void loosen(obj x) {
  set_rank(x, -1);
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


//==============================================================================

// Tests

void init_heap() {
  
  heap_start = malloc(sizeof(obj) * SPACE_SZ);
 
  // initialize freelist
  scan = heap_bot;
  *scan = _NULL;
  
  while (scan != heap_top) {
    alloc = scan; // alloc <- address of previous slot
    scan += 10; // scan <- address of next rib slot
    *scan = alloc; // CAR(next rib) <- address of previous slot
  }
  alloc = scan;
}

obj alloc_rib(obj car, obj cdr, obj tag) {

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
#define CHECK_OG(r0,r1,r2,r3,r4,r5,r6,i)                                       \
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
    printf("graph is not back to its original state after test 2.%d\n",i);     \
  }

int main() {

  // FIXME need a better test suite!
  
  init_heap();

  // Edge addition tests: building Olivier's example graph (somewhat)
  // Drawings provided on demand for free

  obj r0 = alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  obj r1 = alloc_rib(r0, TAG_NUM(1), TAG_NUM(1));
  obj r2 = alloc_rib(r1, TAG_NUM(2), TAG_NUM(2));
  obj r3 = alloc_rib(r2, TAG_NUM(3), r1);
  obj r4 = alloc_rib(TAG_NUM(4), r2, TAG_NUM(4));
  obj r5 = alloc_rib(r4, r3, TAG_NUM(5));
  obj r6 = alloc_rib(TAG_NUM(6), TAG_NUM(6), r5);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,0);
  

  //----------------------------------------------------------------------------

  // Edge deletion tests (and more)

  obj *tmp;

  // Test 2.1: OK -- Deallocation without children involved
  // Remove edge between r0 and r1, should remove r0 i.e. make alloc point to r0
  remove_edge(r1,r0);
  if (alloc != r0) {
    printf("Test 2.1 failed\n");
  }
  r0 = alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  CAR(r1) = r0;
  add_edge(r1,r0);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,1);

  // Test 2.2: OK  -- No deallocation or children/parent involved
  // Remove edge between r2 and r1, this shouldn't deallocate any memory but
  // should remove r2 from r1's co-friends, i.e. mirror 3 of r3 should be _NULL
  tmp = alloc;
  remove_edge(r2,r1);
  if (alloc != tmp || CAR(r2) == r1 || M_TAG(r3) == r2) {
    printf("Test 2.2 failed\n");
  }
  CAR(r2) = r1;
  add_edge(r2,r1);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,2);

  // Test 2.3: OK -- Parent removal with deallocation, no children involved
  // Remove edge between r5 and r4, should dealloc r4 (i.e. make alloc point to
  // r4), and remove r4 from r2's co-friends (i.e. mirror 1 of r3 => _NULL)
  remove_edge(r5,r4);
  if (alloc != r4 || CAR(r5) == r4 || M_CAR(r3) == r4) {
    printf("Test 2.3 failed\n");
  }
  r4 = alloc_rib(TAG_NUM(4), TAG_NUM(4), TAG_NUM(4));
  CAR(r5) = r4;
  add_edge(r5,r4);
  CDR(r4) = r2;
  add_edge(r4,r2);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,3);

  // Test 2.4: OK -- Parent swap but no deallocation, children involved
  // Remove edge between r3 and r2, r4 "catches" r2 and becomes the parent and
  // so r3's first mirror field no longer points to r2
  tmp = alloc;
  remove_edge(r3,r2);
  if (alloc != tmp || get_parent(r2) != r4 || M_CAR(r3) == r4) {
    printf("Test 2.4 failed\n");
  }
  //print_heap();
  CAR(r3) = r2;
  add_edge(r3,r2);
  set_parent(r2,r3);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,4);

  // Test 2.5: OK --  Parent removal with deallocation and children involved
  // Olivier's example: remove edge betweem r5 and r3 => r3, r2, r1, and r0
  // all "drop", r3 is deallocated, r4 becomes r2's parent, r2 becomes r1's
  // parent, and the rank of both r1 and r0 decreases to 4 and 5, respectively
  remove_edge(r5,r3);
  if (alloc != r3 || get_parent(r2) != r4 || get_parent(r1) != r2 ||
      get_rank(r1) != 4 || get_rank(r0) != 5) {
    printf("Test 2.5 failed\n");
  }
  r3 = alloc_rib(r2, TAG_NUM(3), r1);
  CDR(r5) = r3;
  add_edge(r5,r3);
  set_parent(r2,r3);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,5);

  // Test 2.6: OK -- Several deallocations
  // Same as above but this time the edge between r2 and r1 is deleted first
  // and so r2 doesn't "catch" r1 => r3, r1, and r0 all get deallocated
  remove_edge(r2,r1);
  remove_edge(r5,r3);
  if (alloc != r3 || get_parent(r2) != r4 || get_rank(r3) != -2 ||
      get_rank(r1) != -2 || get_rank(r0) != -2) {
    printf("Test 2.6 failed\n");
  }
  r3 = alloc_rib(TAG_NUM(3), TAG_NUM(3), TAG_NUM(3));
  CDR(r5) = r3;
  add_edge(r5,r3);
  CAR(r3) = r2;
  add_edge(r3,r2);
  set_parent(r2,r3);
  r1 = alloc_rib(TAG_NUM(1), TAG_NUM(1), TAG_NUM(1));
  r0 = alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  TAG(r3) = r1;
  add_edge(r3,r1);
  CAR(r2) = r1;
  add_edge(r2,r1);
  CAR(r1) = r0;
  add_edge(r1,r0);

  CHECK_OG(r0,r1,r2,r3,r4,r5,r6,6);
  
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
  obj r7 = alloc_rib(TAG_NUM(7), TAG_NUM(7), TAG_NUM(7));
  CAR(r0) = r7;
  add_edge(r0,r7);
  CDR(r7) = r0;
  add_edge(r7,r0); // cycle created
  remove_edge(r1,r0); // <- infinite loop in drop (see comment in remove_parent)
  if (alloc != r0 || *alloc != r7 || get_rank(r0) != -2 || get_rank(r7) != -2) {
    printf("Test 3.1 failed\n");
  }
  r0 = alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  CAR(r1) = r0;
  add_edge(r1,r0);
  r7 = alloc_rib(TAG_NUM(7), TAG_NUM(7), TAG_NUM(7));
  CAR(r0) = r7;
  add_edge(r0,r7);
  CDR(r7) = r0;
  add_edge(r7,r0); // <- infinite loop in drop (see comment in remove_parent)

  // Test 3.2: OK -- Another object to be deallocated after cycle is detected
  // Same as above but this time we need to deallocate another rib before
  // breaking the loop where the cycle was detected (no longer applies)
  obj r8 = alloc_rib(TAG_NUM(8), TAG_NUM(8), TAG_NUM(8));
  TAG(r7) = r8;
  add_edge(r7,r8);
  remove_edge(r1,r0);
  if (get_rank(r0) != -2 || get_rank(r7) != -2 || get_rank(r8) != -2) {
    printf("Test 3.2 failed\n");
  }
  r0 = alloc_rib(TAG_NUM(0), TAG_NUM(0), TAG_NUM(0));
  CAR(r1) = r0;
  add_edge(r1,r0);
  r7 = alloc_rib(TAG_NUM(7), TAG_NUM(7), TAG_NUM(7));
  CAR(r0) = r7;
  add_edge(r0,r7);
  CDR(r7) = r0;
  add_edge(r7,r0);
  r8 = alloc_rib(TAG_NUM(8), TAG_NUM(8), TAG_NUM(8));
  TAG(r7) = r8;
  add_edge(r7,r8);

  // Test 3.3: OK -- A rib is involved in two cycles
  // Same as the two above but now r8 also points to r7, meaning that r7 is
  // now part of two cycles (r0/r7 and r7/r8 cycles)
  CAR(r8) = r7;
  add_edge(r8,r7); // new cycle created
  remove_edge(r1,r0);
  if (get_rank(r0) != -2 || get_rank(r7) != -2 || get_rank(r8) != -2) {
    printf("Test 3.3 failed\n");
  }
    
  return 0;
}
