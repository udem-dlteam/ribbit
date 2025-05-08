/*
 * The Ribbit VM implementation in C
 */

// @@(location import)@@

// @@(feature debug
#define DEBUG_I_CALL
// )@@

// @@(feature debug-gc
#define DEBUG_GC
// )@@

// @@(feature c/gc/mark-sweep
#define MARK_SWEEP
// )@@

// @@(feature c/gc/mark-sweep-dsw
#define MARK_SWEEP_DSW // Deutsch-Schorr-Waite graph marking algorithm version
// )@@

#ifdef MARK_SWEEP_DSW
#define MARK_SWEEP
#endif

#ifdef DEBUG_I_CALL
#define DEBUG
#endif

// @@(feature c/gc/bigger-heap-2
#define BIGGER_HEAP_2
// )@@

// @@(feature c/gc/bigger-heap-4
#define BIGGER_HEAP_4
// )@@

// @@(feature c/gc/bigger-heap-8
#define BIGGER_HEAP_8
// )@@

// @@(feature c/gc/bigger-heap-16
#define BIGGER_HEAP_16
// )@@

// @@(feature c/gc/bigger-heap-32
#define BIGGER_HEAP_32
// )@@

// @@(feature c/gc/bigger-heap-64
#define BIGGER_HEAP_64
// )@@


#ifdef DEBUG

#include <stdio.h>

#define PRINTLN()                                                              \
  do {                                                                         \
    printf("\n");                                                              \
  } while (0)

#endif

#ifndef NO_STD

#include <stdio.h>
#include <stdlib.h>

#endif

#define ARG_V // @@(feature argv)@@

#ifdef ARG_V

char** argv=NULL;
int argc=0;

#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-pragmas"
#pragma ide diagnostic ignored "ConstantFunctionResult"

#ifdef DEFAULT_REPL_MIN

char *input =
    "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,"
    "enifed,!tes-rotcev,?rotcev,=,cc/"
    "llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,"
    "lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,"
    "lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,"
    "etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,"
    "htgnel,,,,,rddac,rdac,,-,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!"
    "K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/"
    ":kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~"
    "F_|!S+#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~RL^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_"
    "k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/"
    "NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#"
    "ZCex>#d~TbZBi&:EiS/"
    "NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?"
    "x=^G_~F_#bUk``m~YL_|!93_@J^{]%3uy]?'i$9?C_@J^G^~F^z]I'i$'i$9IC^@YGG^~F^@"
    "JvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^3vL@ZIC^@YGG^@JvK~F^89vLvK~T^"
    "89vS;vF~?i%^89vS-vF~Z$^z!G8E^3vE@Z?i%YD^@JvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'"
    "^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?"
    "vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?"
    "vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^"
    "z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Lc^@YS'Hc^BBZ>i$"
    "zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^"
    "z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FKKvR%`YNbuC_~IvR/"
    "^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$4_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~"
    "IakAb^YHKYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&K`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,"
    "AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Kl^~I_k|]"
    "L9Ji&`^{]A'^9AKl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!"
    "76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/"
    "Qk!A'i$'i$'i$'i$8ALaL_~YABaB_~YAHaH_~R`~R_'^~^?`^{]$(i$^z!:9>'i$(bL^~R^zz!"
    "S.Kmk!S0Klk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!5:lks!S':lkt!S):lku!"
    "S&:lkv.!(:lkv/!2:lkv0!H:lkv1!4:lkv2!N:lkv3]&:lkv4!S#:lkv5!3:lkv6y";

#else


// @@(feature (not compression/lzss/2b)
// @@(replace "41,59,39,117,63,62,118,68,63,62,118,82,68,63,62,118,82,65,63,62,118,82,65,63,62,118,82,58,63,62,118,82,61,33,40,58,108,107,109,33,39,58,108,107,118,54,121" (encode-as-bytes "auto" "" "," "")
unsigned char input[] = {41,59,39,117,63,62,118,68,63,62,118,82,68,63,62,118,82,65,63,62,118,82,65,63,62,118,82,58,63,62,118,82,61,33,40,58,108,107,109,33,39,58,108,107,118,54,121,0}; // RVM code that prints HELLO!
// )@@
// )@@

// @@(feature compression/lzss/2b

// @@(replace "41,59,39,117,63,62,118,68,63,62,118,82,68,63,62,118,82,65,63,62,118,82,65,63,62,118,82,58,63,62,118,82,61,33,40,58,108,107,109,33,39,58,108,107,118,54,121" (encode-as-bytes "auto" "" "," "")
unsigned char compressed_input[] = {41,59,39,117,63,62,118,68,63,62,118,82,68,63,62,118,82,65,63,62,118,82,65,63,62,118,82,58,63,62,118,82,61,33,40,58,108,107,109,33,39,58,108,107,118,54,121,0}; // RVM code that prints HELLO!
// )@@

#define BYTE_BASE 00            // @@(replace "00" compression/lzss/2b/byte-base)@@
#define SIZE_BASE 00            // @@(replace "00" compression/lzss/2b/size-base)@@
#define RIBN_BASE 00            // @@(replace "00" compression/lzss/2b/ribn-base)@@
#define RIBN_SIZE 00            // @@(replace "00" compression/lzss/2b/ribn-size)@@
#define COMPRESSED_RIBN_SIZE 00 // @@(replace "00" compression/lzss/2b/compressed-ribn-size)@@

unsigned char input[RIBN_SIZE]; 

void decompress(){
  int j = 0;
  int i = 0;

  while(i < sizeof(compressed_input)){
    unsigned char c1 = compressed_input[i++];
    if (c1 >= RIBN_BASE){
      unsigned char c2 = compressed_input[i++];
      unsigned int combined = (c1 - RIBN_BASE) * BYTE_BASE + c2;
      unsigned int offset = combined / SIZE_BASE;
      unsigned int length = (combined % SIZE_BASE) + 3;

      while(length--) 
        input[j++] = input[j-offset];

    }
    else{
      input[j++] = c1;
    }
  }
}

// )@@

#endif

#ifndef NULL

#define NULL 0
typedef unsigned long size_t;

#endif

// basic def. of a boolean
typedef unsigned char bool;

#define true (1)

// an unsigned byte value for the REPL's code
typedef unsigned char byte;

// a tagged value
typedef unsigned long obj;

// a number
typedef long num;


// GC constants

// ATTENTION PLEASE: don't change the base size during testing
#ifndef BASE_HEAP_SIZE_FIELDS
#define BASE_HEAP_SIZE_FIELDS 12000000
#endif

#define RIB_NB_FIELDS 3

#if defined(BIGGER_HEAP_2)
#define HEAP_SIZE_FACTOR 2
#elif defined(BIGGER_HEAP_4)
#define HEAP_SIZE_FACTOR 4
#elif defined(BIGGER_HEAP_8)
#define HEAP_SIZE_FACTOR 8
#elif defined(BIGGER_HEAP_16)
#define HEAP_SIZE_FACTOR 16
#elif defined(BIGGER_HEAP_32)
#define HEAP_SIZE_FACTOR 32
#elif defined(BIGGER_HEAP_64)
#define HEAP_SIZE_FACTOR 64
#else
#define HEAP_SIZE_FACTOR 1
#endif

#ifndef HEAP_SIZE_FIELDS
#define HEAP_SIZE_FIELDS (BASE_HEAP_SIZE_FIELDS * HEAP_SIZE_FACTOR)
#endif

#ifndef HEAP_SIZE_BYTES
#define HEAP_SIZE_BYTES (HEAP_SIZE_FIELDS * sizeof(obj))
#endif

#define MAX_NB_OBJS (HEAP_SIZE_FIELDS / RIB_NB_FIELDS)

typedef struct {
  obj fields[RIB_NB_FIELDS];
} rib;

rib *heap_start;

#define heap_bot ((obj *)(heap_start))
#define heap_top (heap_bot + (HEAP_SIZE_FIELDS))
#ifdef MARK_SWEEP
#define _NULL ((obj)NULL)
#else
obj *alloc_limit;
#define heap_mid (heap_bot + (HEAP_SIZE_FIELDS >> 1))
#endif
// end GC constants

#define EXIT_ILLEGAL_INSTR 6
#define EXIT_NO_MEMORY 7

#ifdef MARK_SWEEP
// If the mark and sweep GC is used, we need 2 bits: one to mark a live object
// and one to tag integers and ribs
#define UNTAG(x) ((x) >> 2)
#define TAG_NUM(num) ((((obj)(num)) << 2) | 1)
#define MARK(x) ((x)|2) // assumes x is a tagged obj (ul)
#define UNMARK(x) ((x)^2)
#define IS_MARKED(x) ((x)&2)
#define GET_MARK(o) (IS_MARKED(CDR(o)) + (IS_MARKED(TAG(o)) >> 1))
#else
#define UNTAG(x) ((x) >> 1)
#define TAG_NUM(num) ((((obj)(num)) << 1) | 1)
#endif
#define RIB(x) ((rib *)(x))
#define NUM(x) ((num)(UNTAG((num)(x))))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x))
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))

#define PRIM1() obj x = pop()
#define PRIM2()                                                                \
  obj y = pop();                                                               \
  PRIM1()
#define PRIM3()                                                                \
  obj z = pop();                                                               \
  PRIM2()

// WARNING : the CHECK_ACCESS macro should only be used with stop and copy GC
// CHECK_ACCESS will check if pointers are in the right space range when 
// accessing them with CAR, CDR or TAG
// #define CHECK_ACCESS
#ifdef CHECK_ACCESS
obj check_access(obj x){
  if (IS_NUM(x)){
    printf("ERROR ACCESSING NUMBER AS RIB %ld\n", NUM(x));
  }
  obj to_space_start = (obj)((alloc_limit == heap_mid) ? heap_mid : heap_bot);
  obj to_space_end   = (obj)(to_space_start + (HEAP_SIZE_FIELDS/2));

  if (to_space_start < x && x < to_space_end){
    printf("ERROR ACCESSING OUTSIDE SPACE %ld\n", NUM(x));
  }
  return x;
}

#define CAR(x) RIB(check_access(x))->fields[0]
#define CDR(x) RIB(check_access(x))->fields[1]
#define TAG(x) RIB(check_access(x))->fields[2]
#else
#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]
#endif
#define TOS CAR(stack)

#define NUM_0 (TAG_NUM(0))

#define INSTR_AP 0
#define INSTR_SET 1
#define INSTR_GET 2
#define INSTR_CONST 3
#define INSTR_IF 4
#define INSTR_HALT 5

#define PAIR_TAG TAG_NUM(0)
#define CLOSURE_TAG TAG_NUM(1)
#define SYMBOL_TAG TAG_NUM(2)
#define STRING_TAG TAG_NUM(3)
#define SINGLETON_TAG TAG_NUM(5)

// @@(location decl)@@
// the only three roots allowed
obj stack = NUM_0;
obj pc = NUM_0;
obj FALSE = NUM_0;

// global, but not a root, referenced
obj symbol_table = NUM_0;

size_t pos = 0;


#ifdef NO_STD
#define vm_exit(code)                                                          \
  do {                                                                         \
    asm volatile("mov $0x01, %%eax\nmov %0, %%ebx\nint $0x80" : : "i"(code));  \
  } while (0)
#else
#define vm_exit(code)                                                          \
  do {                                                                         \
    exit((code));                                                              \
  } while (0)
#endif

#if defined(NO_STD) && !defined(NO_REG)
register obj *alloc asm("edi");
#else
obj *alloc;
#endif
obj *alloc_limit;
obj *scan;

#ifdef NO_STD

void *sys_brk(void *addr) {
  long ptr;

  asm volatile("mov %0, %%ebx\n"
               "mov $0x2d, %%eax\n"
               "int $0x80\n"
               : "=a"(ptr)
               : "g"((long)addr)
               : "ebx");

  return (void *)ptr;
}

#endif

void init_heap() {
#ifdef NO_STD
  heap_start = sys_brk((void *)NULL);
  void *new_brk = sys_brk((void *)heap_top);

  if (new_brk == heap_start) {
    vm_exit(EXIT_NO_MEMORY);
  }

#else

  fprintf(stderr, "allocated space size = %lu \n", HEAP_SIZE_BYTES);
  heap_start = malloc(HEAP_SIZE_BYTES);
  
  if (!heap_start) {
    vm_exit(EXIT_NO_MEMORY);
  }
#endif

#ifdef MARK_SWEEP
  // initialize freelist
  scan = heap_top;
  scan -= RIB_NB_FIELDS;
  *scan = _NULL;
  
  while (scan != heap_bot) {
    alloc = scan; // alloc <- address of previous slot
    scan -= RIB_NB_FIELDS; // scan <- address of next rib slot
    *scan = (obj)alloc; // CAR(next rib) <- address of previous slot
  }
  alloc = scan;
#else
  alloc = heap_bot;
  alloc_limit = heap_mid;
#endif
  stack = NUM_0;
}

// GC algorithms
#ifdef MARK_SWEEP

#ifdef MARK_SWEEP_DSW
void mark(obj *o) {
  // Deutsch-Schorr-Waite algorithm for marking phase
  if (IS_RIB(*o)) {
    
    obj curr = *o;
    obj tmp;
    obj prev = _NULL;

    if (curr == stack) { // initialize descend for TOS
      prev = curr;
      curr = CDR(prev);
      CDR(prev) = _NULL;
      CDR(prev) = MARK(CDR(prev));
    }

  forward:
    if (curr == _NULL) {
      TAG(prev) = MARK(TAG(prev));
      goto backward;
    }
    if (IS_RIB(curr) && (GET_MARK(curr) == 0)) { // car descend
      TAG(curr) = MARK(TAG(curr)); // mark = 01
      tmp = curr;
      curr = CAR(curr);
      CAR(tmp) = prev;
      prev = tmp;
      goto forward;
    }
    
  backward:
    if (prev != _NULL) {
     
      switch(GET_MARK(prev)) {

        // cdr descend
        case 1:
          TAG(prev) = UNMARK(TAG(prev)); // mark will be 10 so unmark tag
          tmp = CAR(prev); 
          CAR(prev) = curr; 
          curr = CDR(prev); 
          CDR(prev) = tmp; 
          CDR(prev) = MARK(CDR(prev)); // mark = 10
          goto forward;
          
        // tag descend
        case 2:
          tmp = UNMARK(CDR(prev)); 
          CDR(prev) = curr; 
          CDR(prev) = MARK(CDR(prev)); // mark will be 11
          curr = TAG(prev); // shouldn't be marked so no need to unmark
          TAG(prev) = tmp;
          TAG(prev) = MARK(TAG(prev)); // mark = 11
          goto forward;

        // retreat... only 3rd field will remain marked (01) so that we can
        // re-use the same logic in the GC function as with the recursive version
        case 3:
          tmp = UNMARK(TAG(prev));
          TAG(prev) = curr; 
          TAG(prev) = MARK(TAG(prev)); // want for 3rd field to stay tagged
          CDR(prev) = UNMARK(CDR(prev)); // mark = 01
          curr = prev;
          prev = tmp;
          goto backward;
      } 
    }
  }
}

#else

void mark(obj *o) { // Recursive version of marking phase
  obj z = *o;
  while (IS_RIB(z)) {
    obj *ptr = RIB(z)->fields;
    if (!IS_MARKED(ptr[1])) { 
      obj tmp = ptr[1]; 
      ptr[1] = MARK(ptr[1]);
      mark(&ptr[0]);
      mark(&ptr[2]);
      z = tmp;
    } else {
      break;
    }
  }
}

#endif

void gc() {
  // @@(location gc-start)@@
#ifdef DEBUG_GC
  printf("\t--GC called\n");
#endif
  // Mark (only 3 possible roots)
  mark(&stack);
  mark(&pc);
  mark(&FALSE);
  // Sweep
  scan=heap_bot;
  while (scan != heap_top) {
    obj tag = *(scan+1);
    if (IS_MARKED(tag)) {
      *(scan+1) = UNMARK(tag);
    } else {
      *scan = (obj)alloc;
      alloc = scan;
    }
    scan += RIB_NB_FIELDS; // next rib object
  }
  if (*alloc == _NULL){
    printf("Heap is full\n");
  }
  // @@(location gc-end)@@
}

#else

// Cheney style stop & copy GC

// NULL is a pointer (0) but would represent NULL
// so it is never present in an obj field, and
// cannot be a number because it is even. This
// saves a couple of bytes v.s having STACK
// as the broken heart value
#define GC_COPIED_OBJ ((obj)NULL)

void copy() {
  obj o = *scan;
  // we sometime reference rib that are allocated in BSS,
  // we do not want to copy those
  if (IS_RIB(o)) {
    obj *ptr = RIB(o)->fields;
    obj field0 = ptr[0];
    obj copy;

    if (field0 == GC_COPIED_OBJ) {
      copy = ptr[1]; // copied, get new address
    } else {
      copy = TAG_RIB(alloc);
      *ptr++ = GC_COPIED_OBJ; // ptr points to CDR
      *alloc++ = field0;
      *alloc++ = *ptr++; // ptr points to TAG
      *alloc++ = *ptr;
      ptr[-1] = copy; // set forward ptr. Since it points to TAG, ptr[-1]
                      // rewrites the CDR
    }
    *scan = copy; // overwrite to new address.
  }
  scan++;
}

void gc() {
  // @@(location gc-start)@@
#ifdef DEBUG_GC
  obj *from_space = (alloc_limit == heap_mid) ? heap_bot : heap_mid;

  size_t objc = alloc - from_space;
  printf("\t--GC %zu -> ", objc);
#endif

  // swap
  obj *to_space = (alloc_limit == heap_mid) ? heap_mid : heap_bot;
  alloc_limit = to_space + (HEAP_SIZE_FIELDS/2);

  alloc = to_space;

  // root: stack
  scan = &stack;
  copy();

  // root: pc
  scan = &pc;
  copy();

  // root: false
  scan = &FALSE;
  copy();

  // scan the to_space to pull all live references
  scan = to_space;
  while (scan != alloc) {
    copy();
  }

#ifdef DEBUG_GC

  objc = alloc - to_space;
  printf("%zu\n", objc);
  fflush(stdout);

#endif
  // @@(location gc-end)@@
}
#endif // end of GC algorithms

obj pop() {
  obj x = CAR(stack);
  stack = CDR(stack);
  return x;
}

void push2(obj car, obj tag) {
#ifdef MARK_SWEEP
  obj tmp = *alloc; // next available slot in freelist
#endif  
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;
  *alloc++ = stack;
  *alloc++ = tag;
  stack = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));
  
#ifdef MARK_SWEEP
  alloc = (obj *)tmp;
  if (!IS_RIB(tmp) || *alloc == _NULL) { // empty freelist?
    gc();
  }
#else
  if (alloc == alloc_limit) {
    gc();
  }
#endif
}

// Simple version of push for stack operations
static inline void push(obj car){
  push2(car, PAIR_TAG);
}

/**
 * Allocate a rib that is not kept on the stack (can be linked
 * from anywhere). The car and cdr can be live references to other
 * ribs.
 * @param car
 * @param cdr
 * @param tag
 * @return
 */
rib *alloc_rib(obj car, obj cdr, obj tag) {
  push2(car, cdr); // tag is set
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = TAG(allocated);
  TAG(allocated) = tag;

  stack = old_stack;

  return RIB(allocated);
}

rib *alloc_rib2(obj car, obj cdr, obj tag) {
  push2(car, tag); // tag is set
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = cdr;

  stack = old_stack;

  return RIB(allocated);
}

unsigned char get_byte() { return input[pos++]; }



#define ENCODING_SIZE  (00)                // @@(replace "00" encoding/ribn-base)@@
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

rib *list_tail(rib *lst, num i) {
  return (i == 0) ? lst : list_tail(RIB(lst->fields[1]), i - 1);
}

rib *inst_tail(rib *lst, num i){
  return (i == 0) ? lst : inst_tail(RIB(lst->fields[2]), i - 1);
}

obj list_ref(rib *lst, num i) { return list_tail(lst, i)->fields[0]; }

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

#define TRUE (CAR(FALSE))
#define NIL (CDR(FALSE))
// Temp values that can be used to shield 
//  pointers from the evil GC
#define TEMP1 CAR(TRUE)
#define TEMP2 CDR(TRUE)
#define TEMP3 CAR(NIL)
#define TEMP4 CDR(NIL)

#ifdef DEBUG

void chars2str(obj o) {
  if (o != NIL) {
    printf("%c", (char)(NUM(RIB(o)->fields[0]) % 256));
    chars2str(RIB(o)->fields[1]);
  }
}

void sym2str(rib *c) { chars2str(RIB(c->fields[1])->fields[0]); }

void show_operand(obj o) {
  if (IS_NUM(o)) {
    printf("int %ld", NUM(o));
  } else if (TAG(o) == SYMBOL_TAG) {
    printf("sym ");
    sym2str(RIB(o));
  }
}

#endif

// @@(feature scm2str
char* scm2str(obj s) {
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
obj str2scm(char* s) {
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
obj list2scm(char **s, int length) {
    obj list = NIL;
    for (int i = length - 1; i >= 0; i--)
        list = TAG_RIB(alloc_rib(str2scm(s[i]), list, PAIR_TAG));
    
    return list;
};
// )@@

// @@(feature bool2scm 
obj bool2scm(bool x) { return x ? CAR(FALSE) : FALSE; }
// )@@

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
    break;
    
  } // )@@
  case 1: // @@(primitive (##id x)
  {
    PRIM1();
    push2(x, PAIR_TAG);
    break;
  } // )@@
  case 2: // @@(primitive (##arg1 x y)
  {
    pop();
    break;
  } // )@@
  case 3: // @@(primitive (##arg2 x y)
  {
    obj x = pop();
    pop();
    push2(x, PAIR_TAG);
    break;
  } //)@@
  case 4: // @@(primitive (##close rib)
  {
    obj x = CAR(TOS);
    obj y = CDR(stack);
    TOS = TAG_RIB(alloc_rib(x, y, CLOSURE_TAG));
    break;
  } //)@@
  case 5: // @@(primitive (##rib? rib) (use bool2scm)
  {
    PRIM1();
    push2(bool2scm(IS_RIB(x)), PAIR_TAG);
    break;
  } //)@@
  case 6: // @@(primitive (##field0 rib)
  {
    PRIM1();
    push2(CAR(x), PAIR_TAG);
    break;
  } //)@@
  case 7: // @@(primitive (##field1 rib)
  {
    PRIM1();
    push2(CDR(x), PAIR_TAG);
    break;
  } //)@@
  case 8:  // @@(primitive (##field2 rib)
  {
    PRIM1();
    push2(TAG(x), PAIR_TAG);
    break;
  } //)@@
  case 9: // @@(primitive (##field0-set! rib x)
  { 
    PRIM2();
    push2(CAR(x) = y, PAIR_TAG);
    break;
  } //)@@
  case 10:  // @@(primitive (##field1-set! rib x)
  {
    PRIM2();
    push2(CDR(x) = y, PAIR_TAG);
    break;
  } //)@@
  case 11:  // @@(primitive (##field2-set! rib x)
  {
    PRIM2();
    push2(TAG(x) = y, PAIR_TAG);
    break;
  } // )@@
  case 12:  // @@(primitive (##eqv? rib1 rib2) (use bool2scm)
  {
    PRIM2();
    push2(bool2scm(x == y), PAIR_TAG);
    break;
  } //)@@
  case 13:  // @@(primitive (##< x y) (use bool2scm)
  {
    PRIM2();
    push2(bool2scm(NUM(x) < NUM(y)), PAIR_TAG);
    break;
  } //)@@
  case 14:  // @@(primitive (##+ x y)
  {
    PRIM2();
    push2(x + y - 1, PAIR_TAG);
    break;
  } //)@@
  case 15:  // @@(primitive (##- x y)
  {
    PRIM2();
    push2(x - y + 1, PAIR_TAG);
    break;
  } //)@@
  case 16:  // @@(primitive (##* x y)
  {
    PRIM2();
    push2(TAG_NUM((NUM(x) * NUM(y))), PAIR_TAG);
    break;
  } // )@@
  case 17:  // @@(primitive (##quotient x y)
  {
    PRIM2();
    push2(TAG_NUM((NUM(x) / NUM(y))), PAIR_TAG);
    break;
  } // )@@
  case 18:  // @@(primitive (##getchar)
  {
    int read;
#ifdef NO_STD
    asm volatile("push %%eax\n"
                 "mov $0x03, %%eax\n"    // sys_call
                 "mov $0, %%ebx\n"       // fd
                 "lea 0(%%esp), %%ecx\n" // ptr
                 "mov $1, %%edx\n"       // count
                 "int $0x80\n"
                 "pop %%eax\n"
                 : "=a"(read)
                 :
                 : "ebx", "ecx", "edx", "esi", "edi");
    read &= 0xFF;
#else
    read = getchar();
    if (EOF == read) read = -1;
#endif
    push2(TAG_NUM(read), PAIR_TAG);
    break;
  } // )@@
  case 19:  // @@(primitive (##putchar c)
  {
    PRIM1();
#ifdef NO_STD
    {
      asm volatile("mov %0, %%eax\n"
                   "push %%eax\n"
                   "mov $1, %%ebx\n"       // fd
                   "lea 0(%%esp), %%ecx\n" // ptr
                   "mov $1, %%edx\n"       // count
                   "mov $0x04, %%eax\n"    // sys_call
                   "int $0x80\n"
                   "pop %%eax\n"
                   :
                   : "g"((int)(NUM(x) & 0xFF))
                   : "eax", "ebx", "ecx", "edx", "esi", "edi");
    }
#else
    putchar((char)NUM(x));
    fflush(stdout);
#endif
    push2(x, PAIR_TAG);
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

#ifdef DEBUG
void show_rib(obj s, int depth){
    if (depth < 0){
        if (IS_RIB(s)){
            printf("[...]");
        }
        else{
            printf("...");
        }
        return;
    }
    if (IS_RIB(s)){
      if (TAG(s) == SINGLETON_TAG){
        if (s == NIL) { printf("'()"); }
        if (s == TRUE) { printf("#t"); }
        if (s == FALSE) { printf("#f"); }
      }
      else{
        printf("[");
        show_rib(CAR(s), depth-1);
        printf(", ");
        show_rib(CDR(s), depth-1);
        printf(", ");
        show_rib(TAG(s), depth-1);
        printf("]");
      }
    }
    else{
        printf("%d", NUM(s));
    }
}

void show_stack(){
    obj itr = stack;
    PRINTLN();
    if (NUM(TAG(itr))){
        printf("[]");
        return;

    }
    printf("[ ");
    int first = 0;
    while(!NUM(TAG(itr))){
        if (first){
            printf(", ");
        }
        else{
            first = 1;
        }
        show_rib(CAR(itr), 0);
        itr = CDR(itr);
    }
    printf(" ]");

}

#endif

void run() {
#define ADVANCE_PC()                                                           \
  do {                                                                         \
    pc = TAG(pc);                                                              \
  } while (0)
  while (1) {
    num instr = NUM(CAR(pc));
    switch (instr) {
    default: { // error
      vm_exit(EXIT_ILLEGAL_INSTR);
    }
    case INSTR_HALT: { // halt
      vm_exit(0);
    }
    case INSTR_AP: // call or jump
    {
      bool jump = TAG(pc) == NUM_0;
#ifdef DEBUG_I_CALL
      printf(jump ? "--- jump " : "--- call ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      obj proc = get_opnd(CDR(pc));
      while (1) {
#define code CAR(proc)
          if (IS_NUM(code)) {
            pop(); // @@(feature (and arity-check (not prim-no-arity)))@@
            proc=prim(NUM(code));

            if (IS_RIB(proc)) continue;

            if (jump) {
              // jump
              pc = get_cont();
              CDR(stack) = CAR(pc);
            }
            pc = TAG(pc);
          } else {
            num nargs = NUM(pop()); // @@(feature arity-check)@@
            obj s2 = TAG_RIB(alloc_rib(NUM_0, proc, PAIR_TAG));
            proc = CDR(s2);
            CDR(s2) = CDR(proc); // @@(feature flat-closure)@@
            CAR(pc) = CAR(proc); // save the proc from the mighty gcrvm.c


            num nparams_vari = NUM(CAR(code));
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
                    TEMP1 = s2;
                    rest = TAG_RIB(alloc_rib(pop(), rest, PAIR_TAG));
                    s2=TEMP1;
                }
                s2 = TAG_RIB(alloc_rib(rest, s2, PAIR_TAG));
            }
            // )@@

            for (int i = 0; i < nparams; ++i) {
              s2 = TAG_RIB(alloc_rib(pop(), s2, PAIR_TAG));
            }

            nparams = nparams + vari; // @@(feature arity-check)@@

            obj c2 = TAG_RIB(list_tail(RIB(s2), nparams));

            if (jump) {
              obj k = get_cont();
              CAR(c2) = CAR(k);
              TAG(c2) = TAG(k);
            } else {
              CAR(c2) = stack;
              TAG(c2) = TAG(pc);
            }

            stack = s2;

            obj new_pc = CAR(pc);
            CAR(pc) = TAG_NUM(instr);
            pc = TAG(new_pc);
          }
          break;
      }
      break;
    }
#undef code
    case INSTR_SET: { // set
#ifdef DEBUG_I_CALL
      printf("--- set ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      obj x = CAR(stack);
      ((IS_NUM(CDR(pc))) ? list_tail(RIB(stack), NUM(CDR(pc))) : RIB(CDR(pc)))
          ->fields[0] = x;
      stack = CDR(stack);
      ADVANCE_PC();
      break;
    }
    case INSTR_GET: { // get
#ifdef DEBUG_I_CALL
      printf("--- get ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      push2(get_opnd(CDR(pc)), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_CONST: { // const
#ifdef DEBUG_I_CALL
      printf("--- const ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      push2(CDR(pc), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_IF: { // if
#ifdef DEBUG_I_CALL
      printf("--- if");
      show_stack();
      PRINTLN();
#endif

      obj p = pop();
      if (p != FALSE) {
        pc = CDR(pc);
      } else {
        pc = TAG(pc);
      }
      break;
    }
    }
  }
#undef ADVANCE_PC
}

rib *symbol_ref(num n) { return RIB(list_ref(RIB(symbol_table), n)); }

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
  rib *root = alloc_rib(TAG_RIB(sym), symbol_table, PAIR_TAG);
  return root;
}

void build_sym_table() {
  num n = get_int(0);

  while (n > 0) {
    n--;
    symbol_table = TAG_RIB(create_sym(NIL));
  }

  obj accum = NIL;

  while (1) {
    byte c = get_byte();

    if (c == 44) {
      symbol_table = TAG_RIB(create_sym(accum));
      accum = NIL;
      continue;
    }

    if (c == 59)
      break;

    accum = TAG_RIB(alloc_rib(TAG_NUM(c), TAG_RIB(accum), PAIR_TAG));
  }

  symbol_table = TAG_RIB(create_sym(accum));
}

void set_global(obj c) {
  CAR(CAR(symbol_table)) = c;
  symbol_table = CDR(symbol_table);
}


// @@(feature encoding/optimal
void decode() {
  int weights[] = {1,2,3}; // @@(replace "{1,2,3}" (list->host encoding/optimal/start "{" "," "}"))@@

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

    //printf("n : %d, ", n); // @@(feature debug)@@
    //fflush(stdout); // @@(feature debug)@@

    if (op < 20) {
      i = (op / 4) - 1;
      i = i < 0?0:i;
      n = !(op & 0b10)  ? TAG_NUM(n) : TAG_RIB(symbol_ref(n));
    }
    else if (op < 22) {
      n = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib2(TAG_NUM(n), NUM_0, pop())), NIL, CLOSURE_TAG));
      i = 3;
      if (stack == NUM_0) break;
    }
    else if (op < 24){
      push2(TAG_RIB(inst_tail(RIB(TOS), n)), NUM_0);
      continue;
    }
    else if (op < 25){
      n = pop();
      i=4;
    }

    //printf("i : %d\n", i); // @@(feature debug)@@
    //fflush(stdout); // @@(feature debug)@@

    rib *c = alloc_rib(TAG_NUM(i), n, NUM_0);
    c->fields[2] = TOS;
    TOS = TAG_RIB(c);
  }

  pc = TAG(CAR(n));
}
// )@@

// @@(feature encoding/original
void decode() {
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

void setup_stack() {
  push2(NUM_0, PAIR_TAG);
  push2(NUM_0, PAIR_TAG);

  obj first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;

  CAR(first) = TAG_NUM(INSTR_HALT);
  CDR(first) = NUM_0;
  TAG(first) = PAIR_TAG;
}

#ifdef NOSTART
void _start() {
#else

void init() {
#endif
  decompress(); // @@(feature compression/lzss/2b)@@
  init_heap();

  FALSE = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                            TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                            SINGLETON_TAG));

  build_sym_table();
  decode();

  set_global(
      TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG))); /* primitive 0 */
  set_global(FALSE);
  set_global(TRUE);
  set_global(NIL);

  setup_stack();

  run();
}

#ifdef ARG_V

int main(int _argc, char* _argv[]) { 
    argc=_argc;
    argv=_argv;
    init(); 
}

#else

int main() { init(); }

#endif

#pragma clang diagnostic pop
