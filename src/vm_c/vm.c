// debug instruction calls?

#ifdef DEBUG_I_CALL
#define DEBUG
#endif


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

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-pragmas"
#pragma ide diagnostic ignored "ConstantFunctionResult"

int input_len = 1980;
char *input = "Bfer-gnirts,htgnel-gnirts,fi,!rdc-tes,!tes-gnirts,esrever,enifed,!tes-rotcev,=,cc/llac,!tes,adbmal,rddc,fer-rotcev,htgnel-rotcev,etouq,lobmys>-gnirts,cossa,gnirts>-lobmys,gnirts>-tsil,?erudecorp,?rotcev,!rac-tes,qssa,tneitouq,?gnirts,enilwen,ton,rebmun>-gnirts,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,rahc-keep,?llun,liat-tsil,tsil>-gnirts,daer,+,etirw,,,,?lauqe,htgnel,,,-,,rddac,rdac,<,,,,,rahc-daer,?riap,rac,,rdc,snoc,,?vqe,,,,,,;9I]I9I?YP?YAYM^'i$~YI^YC?OvCvR3y!M7#YS&^z!S&9Ai&:IiS,ai&kkz!S,:kw'k]@'_+Z@aC_E^~F^{!>'^8>YBlbC`^'`~@_E_~F_|]C9B`^Uka_CaE`2ZCdCbBai$E`^~F_|!S(#`kn4^~i$#`kn4^~i$#`kn4^~i$#`kn4^~QK^~@w(D^~@kJ^~Q^z]G#YS(a_l{]B#a_k#k_k~@iS,_{!2#b`n9CBd`Ca_#ZBex>#d~YFbZAi&:IiS,NeZ@BBfi$i$akS_nM`~@x1^2:IgYKecIfNdboMa_~@x:^2ZGdUlbMbNa_~@x6^9CBd`Ca_#ZBex>#d~YFbZAi&:IiS,NeZ@BBfi$i$akS_nM`~@x1^2:IgYKecIfNdboMa_~@x:^2ZGdUlbMbNa_~i%~@x2^#cMan~@x-^E_~F_#bUk``m~YH_|!81_?H^{!P1uy]H1^1^?ZH_~L`kYBPYJu``vR%Z$u^z]?'i$9?C^?HE^~F^z]E'i$'i$9EC^?YAE^~F^?HvC~F^z!A9H^1vS*?OvS6vS9?OvS7vF~Z(^8AYS'^?HvF~Z'^9?YDZ*^~YH^1vE?Z?YD^?HvE~Z#^1vL?ZEC^?YAE^?HvK~F^88vLvK~YF^88vS;vF~@i%^88vS-vF~YO^z]L9L8@~@u^'^~Lk^Gy!@8@?G'^9L~@vR0^~L_vC89lk~YI^YGy!?+V^?G'i&~LvD^+V^?G'i&~i%~@vL^+V^?G'i&~i%~@vK^YGy]J+ZJ^YC'i&?G~@vL^Wy!C9,_'^~^YN^Z)BV^?G+Bi&YCx-?G~@vJ^9PYC'i%?G~@vS;^'i$?G~@vS-^YG?G~@vF^9J?G~@vK^'^~Lk^Wy!G'^?Z=_iS+Gy]F'^?Z=iS*iS+'^~@iS-^?Z=_iS+z!09F^9FZO~@iS*^'^~@iS-^JiS+y!S+:kkiS*!S*Pmk!I)iS-^z!S-Plk]37%Z>'_?YS$Kc^?Z=Jc^DDZ>i$zDDZ>i$z]M'_9MBaE_C^~F^{]79Mi&^z]+'i$9+Ca_'^~TE__E_~F_{]%'i$9%Ca_'^~@E__E_~F_{!<'k8BSC_l~F^z!E'^8EPl`C^~L`k{]K9&aYE`^|!L.YE`^{!='i$'i$'i$'i$8=K`K^~TD`D^~TJ`J^~Q_~Q^'i%~@`^{?YS%ki#!S)Di#]N'^!S)BiS)^YS#^9NDa_'^~T`D^J_~F_{],9NiS)^z]D'_'i$'i$9DYBPvR%`YJbuC_~LvR/^~L_vR$E^~F^{!N9DkYD^z]4i)!F)i&^z]*i-!S##m_i$z!HRm]89Kb`J^|]<8L`J^{];i-!Di3])#nS_^z]#Rn]59Kb`J^|]/8L`J^{].i-!S'i3]P#oS_^z]'Ro]A#l`^{](Rl!O)i$^z!K87D^z!76D^z]0-D^z!63D^z]9iS%]&j=!,i-!.i3!+#k`^{!/Rk!;9>'i$)bK^~Q^zz!1Av6]OAv5]$Av4!JAv3!9Av2!BAv1!5Av0!)Av/!S$Av.!S%Au]=At!4As!-Ar!3Aq!:Ap]>Ao!(Am!'Al!*#lk^zy";

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

// a rib obj
#define RIB_NB_FIELDS 3
typedef struct {
  obj fields[RIB_NB_FIELDS];
} rib;

#define EXIT_HEAP_OVERFLOW 5
#define EXIT_ILLEGAL_INSTR 6
#define EXIT_NO_MEMORY 7

#define UNTAG(x) ((x) >> 1)
#define RIB(x) ((rib *)(x))
#define NUM(x) ((num)(UNTAG((num)(x))))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x))
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))
#define TAG_NUM(num) ((((obj)(num)) << 1) | 1)

#define PRIM1() obj x = pop()
#define PRIM2()                                                                \
  obj y = pop();                                                               \
  PRIM1()
#define PRIM3()                                                                \
  obj z = pop();                                                               \
  PRIM2()

#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]
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

// the only three roots allowed
obj stack = NUM_0;
obj pc = NUM_0;
obj FALSE = NUM_0;

// global, but not a root, referenced
obj symbol_table = NUM_0;

size_t pos = 0;

rib *heap_start;

// GC
#define MAX_NB_OBJS 30000
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_mid (heap_bot + (SPACE_SZ))
#define heap_top (heap_bot + (SPACE_SZ << 1))

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

#ifdef NO_STD
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
  heap_start = malloc(sizeof(obj) * (SPACE_SZ << 1));

  if (!heap_start) {
    vm_exit(EXIT_NO_MEMORY);
  }
#endif

  alloc = heap_bot;
  alloc_limit = heap_mid;
  stack = NUM_0;
}

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
#ifdef DEBUG

  obj *from_space = (alloc_limit == heap_mid) ? heap_bot : heap_mid;

  size_t objc = alloc - from_space;
  printf("\t--GC %d -> ", objc);

#endif

  // swap
  obj *to_space = (alloc_limit == heap_mid) ? heap_mid : heap_bot;
  alloc_limit = to_space + SPACE_SZ;

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

#ifdef DEBUG

  objc = alloc - to_space;
  printf("%d\n", objc);

#endif
}

obj pop() {
  obj x = CAR(stack);
  stack = CDR(stack);
  return x;
}

void push2(obj car, obj tag) {
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;
  *alloc++ = stack;
  *alloc++ = tag;

  stack = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));

  if (alloc == alloc_limit) {
    gc();
  }
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

char get_byte() { return input[pos++]; }

num get_code() {
  num x = get_byte() - 35;
  return x < 0 ? 57 : x;
}

num get_int(num n) {
  num x = get_code();
  n *= 46;
  return x < 46 ? n + x : get_int(n + x - 46);
}

rib *list_tail(rib *lst, num i) {
  return (i == 0) ? lst : list_tail(RIB(lst->fields[1]), i - 1);
}

obj list_ref(rib *lst, num i) { return list_tail(lst, i)->fields[0]; }

obj get_opnd(obj o) {
  return (IS_NUM(o) ? list_tail(RIB(stack), NUM(o)) : RIB(o))->fields[0];
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

#ifdef DEBUG

void chars2str(obj o) {
  if (o != TAG_RIB(&NIL)) {
    printf("%c", (char)(NUM(RIB(o)->fields[0]) % 256));
    chars2str(RIB(o)->fields[1]);
  }
}

void sym2str(rib *c) { chars2str(RIB(c->fields[1])->fields[0]); }

void show_operand(obj o) {
  if (IS_NUM(o)) {
    printf("int %ld", NUM(o));
  } else {
    printf("sym ");
    sym2str(RIB(o));
  }
}

#endif

obj boolean(bool x) { return x ? CAR(FALSE) : FALSE; }

void prim(int no) {
  switch (no) {
  case 0: { // rib
    obj clmp = TAG_RIB(alloc_rib(0, 0, 0));
    PRIM3();
    CAR(clmp) = x;
    CDR(clmp) = y;
    TAG(clmp) = z;
    push2(clmp, PAIR_TAG);
    break;
  }
  case 1: { // id
    PRIM1();
    push2(x, PAIR_TAG);
    break;
  }
  case 2: { // pop
    pop();
    true;
    break;
  }
  case 3: { // skip
    obj x = pop();
    pop();
    push2(x, PAIR_TAG);
    break;
  }
  case 4: { // close
    obj x = CAR(TOS);
    obj y = CDR(stack);
    TOS = TAG_RIB(alloc_rib(x, y, CLOSURE_TAG));
    break;
  }
  case 5: { // is rib?
    PRIM1();
    push2(boolean(IS_RIB(x)), PAIR_TAG);
    break;
  }
  case 6: { // field0
    PRIM1();
    push2(CAR(x), PAIR_TAG);
    break;
  }
  case 7: { // field1
    PRIM1();
    push2(CDR(x), PAIR_TAG);
    break;
  }
  case 8: { // field2
    PRIM1();
    push2(TAG(x), PAIR_TAG);
    break;
  }
  case 9: { // set field0
    PRIM2();
    push2(CAR(x) = y, PAIR_TAG);
    break;
  }
  case 10: { // set field1
    PRIM2();
    push2(CDR(x) = y, PAIR_TAG);
    break;
  }
  case 11: { // set field2
    PRIM2();
    push2(TAG(x) = y, PAIR_TAG);
    break;
  }
  case 12: { // eq
    PRIM2();
    push2(boolean(x == y), PAIR_TAG);
    break;
  }
  case 13: { // lt
    PRIM2();
    push2(boolean(x < y), PAIR_TAG);
    break;
  }
  case 14: { // add
    PRIM2();
    push2(x + y - 1, PAIR_TAG);
    break;
  }
  case 15: { // sub
    PRIM2();
    push2(x - y + 1, PAIR_TAG);
    break;
  }
  case 16: { // mul
    PRIM2();
    push2(TAG_NUM((NUM(x) * NUM(y))), PAIR_TAG);
    break;
  }
  case 17: { // div
    PRIM2();
    push2(TAG_NUM((NUM(x) / NUM(y))), PAIR_TAG);
    break;
  }
  case 18: { // getc
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
#endif
    push2(TAG_NUM(read), PAIR_TAG);
    break;
  }
  case 19: { // putc
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
  }
  default: {
    vm_exit(EXIT_ILLEGAL_INSTR);
    break;
  }
  }
}

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
      return;
    }
    case INSTR_HALT: { // halt
      vm_exit(0);
      return;
    }
    case INSTR_AP: // call or jump
    {
        bool jump = IS_NUM(TAG(pc)) && NUM(TAG(pc)) == 0;
#ifdef DEBUG_I_CALL
      printf(jump ? "--- jump " : "--- call ");
      PRINTLN();
#endif
#define proc (get_opnd(CDR(pc)))
#define code CAR(proc)
      if (IS_NUM(code)) {
        prim(NUM(code));

        if (jump) {
          // jump
          pc = get_cont();
          CDR(stack) = CAR(pc);
        }
        pc = TAG(pc);
      } else {
        num argc = NUM(CAR(code));
        // Use the car of the PC to save the new PC
        CAR(pc) = CAR(get_opnd(CDR(pc)));

//        pop();

        obj s2 = TAG_RIB(alloc_rib(NUM_0, proc, PAIR_TAG));

        for (int i = 0; i < argc; ++i) {
          s2 = TAG_RIB(alloc_rib(pop(), s2, PAIR_TAG));
        }

        obj c2 = TAG_RIB(list_tail(RIB(s2), argc));

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
#undef code
#undef proc
    case INSTR_SET: { // set
#ifdef DEBUG_I_CALL
      printf("--- set ");
      PRINTLN();
#endif
      obj x = pop();
      ((IS_NUM(CDR(pc))) ? list_tail(RIB(stack), NUM(CDR(pc))) : RIB(CDR(pc)))
          ->fields[0] = x;
      ADVANCE_PC();
      break;
    }
    case INSTR_GET: { // get
#ifdef DEBUG_I_CALL
      printf("--- get ");
      PRINTLN();
#endif
      push2(get_opnd(CDR(pc)), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_CONST: { // const
#ifdef DEBUG_I_CALL
      printf("--- const ");
      PRINTLN();
#endif
      push2(CDR(pc), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_IF: { // if
#ifdef DEBUG_I_CALL
      printf("--- if");
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

void decode() {
  int weights[6] = {20, 30, 0, 10, 11, 4};

  obj n;
  int d;
  int op;

  while (1) {
    num x = get_code();
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

      // not very readable, see generic.vm.js

      if (n >= d) {
        n = (n == d) ? TAG_NUM(get_int(0))
                     : TAG_RIB(symbol_ref(get_int(n - d - 1)));
      } else {
        n = (op < 3) ? TAG_RIB(symbol_ref(n)) : TAG_NUM(n);
      }

      if (op > 4) {
        n = TAG_RIB(
            alloc_rib(TAG_RIB(alloc_rib2(n, NUM_0, pop())), NIL, CLOSURE_TAG));
        if (stack == NUM_0 || stack == NULL) {
          break;
        }
        op = INSTR_CONST;
      } else if(op > 0) {
          op --;
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
  init_heap();

  FALSE =
      TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                        TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                        SINGLETON_TAG));

  build_sym_table();
  decode();

  set_global(TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG))); /* primitive 0 */
  set_global(FALSE);
  set_global(TRUE);
  set_global(NIL);


  setup_stack();

  run();
}

#ifndef NOSTART

int main() { init(); }

#endif

#pragma clang diagnostic pop
