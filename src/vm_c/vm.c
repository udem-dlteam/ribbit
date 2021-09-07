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

int input_len = 2941;
char *input =
    "#?vqe,fer-gnirts,enifed,htgnel-gnirts,!tes-rav-labolg,!tes-rotcev,esrever,"
    "etouq,rddc,*,fer-rav-labolg,+,2gra_,vne-erudecorp,!tes-gnirts,reffub,cc/"
    "llac,<,edoc-erudecorp,!rdc-tes,tneitouq,-,foe,htgnel-rotcev,ytpme,fi,"
    "adbmal,fer-rotcev,liat,!tes,lobmys-denretninu>-gnirts,tsil>-rotcev,"
    "rahcteg_,?gnirts,!tes-2dleif_,enilwen,?rotcev,!rac-tes,?erudecorp,ton,"
    "qssa,lave,rebmun>-gnirts,lobmys>-gnirts,!tes-1dleif_,elipmoc,rotcev>-tsil,"
    "poon-neg,cossa,gnirts>-tsil,ngissa-neg,xua-rebmun>-gnirts,lper,xua-rahc-"
    "daer,tsil-daer,xua-esrever,tneitouq_,rdddac,tnemmoc-piks,?tcejbo-foe,"
    "gnirts>-lobmys,tsil-etirw,rebmun-etirw,fer-tsil,xua-lobmys>-gnirts,!tes-"
    "tsil,llac-pmoc,srahc-etirw,rahc-keep,?lobmys,liat-tsil,dnetxe,*_,"
    "erudecorp-ekam,?llun,lobmys-daer,?pmulc_,ecapsetihw-non-rahc-keep,tsil>-"
    "gnirts,esolc_,daer,etirw,?ecnatsni,pukool,?lauqe,+_,llac-neg,!tes-0dleif_,"
    "2rahctup,htgnel,2dleif_,rddac,-_,rdac,<_,pmoc,rahctup_,rahc-daer,?riap,=,"
    "1dleif_,0dleif_,rac,snoc,evitimirp,?vqe_,rdc,1gra_,ytitnedi_,pmulc_,lin_,"
    "eurt_,eslaf_,lbtmys_;92k]292k@ZCk@YClZ=l^(i$~Z+l^YDk@SmvCvR3y]=7$kZ9l^z]"
    "98Kmi&>LnjJai&kkz]J>kw(k!M(_-mYMmaAl_El^~Il^{!A(^8AnVlbAl`^(`~B_El_~Il_|]$"
    "'Umc``o8>ma^~i$'Umc``o8>ma^~HmElYFlZ*l_vS&~YOl^YAnkcaAl_El^AlaEl`5nZ$"
    "ndAlbDmai$El`^~Il_|]7'`ko8:^~i$'`ko8:^~i$'`ko8:^~Bw)G^~BlF^~YH^z]4'Z7la_m{"
    "!>'a_l'k_k~BjJ_{!5'b`o9$nDmDmRlbea_`Al`'UmewE'd~YJlbYKmi&>"
    "LnjJPleYMmDmDmfi$i$akRl_oNl`~BxL^5n>LngZ-lecLnfPldbpNla_~BxM^"
    "5nZ4mdYAnlbNlbPla_~BwS6^9$nDmDmRlbea_`Al`'UmewE'd~YJlbYKmi&>"
    "LnjJPleYMmDmDmfi$i$akRl_oNl`~BxL^5n>LngZ-lecLnfPldbpNla_~BxM^"
    "5nZ4mdYAnlbNlbPla_~i%~BxI^'cNlao~BwS1^El_~Il_'bYAnk``n~YOl_|!<4_@K^{]C4uy]"
    "(4^4^@Z(l_~M`kVOYLu``vR%Z.u^z]#(i$9#lAl^@KEl^~Il^z])(i$(i$9)lAl^@YClEl^~"
    "Il^@KvC~Il^z!C9(l^4vS*@SmvS6vS9@SmvS7vF~Z@l^8ClZGl^@KvF~ZBl^9#lYFlZ*l^~"
    "YOl^4vE@Z#lYFl^@KvE~ZEl^4vL@Z)lAl^@YClEl^@KvK~Il^8<mvLvK~YJl^8<mvS;vF~Bi%^"
    "8<mvS-vF~Z?l^z],9,k8Gk~Hmu^(^~Mk^Jky!G8Gk@Jk(^9,k~HmvR0^~M_vC88lk~Z+l^"
    "YPky!I-mYIk^@Jk(i&~MvD^-mYIk^@Jk(i&~i%~HmvL^-mYIk^@Jk(i&~i%~HmvK^YPky]0-"
    "mZ0k^YDk(i&@Jk~HmvL^YGky!D9;l_(^~^Z<l^Z5lDmYIk^@Jk-mDmi&YDkwS1@Jk~HmvJ^"
    "98lYDk(i%@Jk~HmvS;^(i$@Jk~HmvS-^YPk@Jk~HmvF^90k@Jk~HmvK^(^~Mk^YGky!P(^@T_"
    "iS)Jky]1(^@TjNiS)(^~HmjP^@T_iS)z!391l^91lZF~HmjN^(^~HmjP^FiS)y!S)>kkjN]"
    "NOmk]++jP^z]POlk!S(7&lYE(_@ZDQc^@TFc^GGYEi$zGGYEi$z]/(_9/"
    "mDmaEl_Al^~Il^{!S29/"
    "mi&^z]6(i$96mAla_(^~WmEl__El_~Il_{]>(i$9>mAla_(^~BEl__El_~Il_{!;(k8?RlAl_"
    "l~Il^z!N(^8NmOl`Al^~M`k{]%9AmaYNm`^|]'.lYNm`^{!@(i$(i$(i$(i$8@mQ`Q^~WmG`G^"
    "~WmF`F^~YH_~YH^(i%~B`^{]&(^!#>ki#^ZHl^9&mGa_(^~Wm`G^F_~Il_{];9&mi#^z]3(_("
    "i$(i$93mVOvR%`YLbuAl_~MvR/^~M_vR$El^~Il^{]<93mkYFl^z!S$9.`^{!S/"
    "8L`^{!S#88`^{!S-8?`^{!S'6`^{!1+`^{!S8+`^{!J+i&^z!S48=`^{!S./"
    "^z]*0^z]H'o_i$z!OYBlo!S*9%nb`F^|!S79'm`F^{!S50^z!F/"
    "^z]5'nRl_^z]EYBln!S39%nb`F^|]K9'm`F^{]O0^z]G/^z]8'mRl_^z]BYBlm!S+0^z!S&/"
    "^z!K'l`^{]@YBll]?+i$^z]-89lAl^z!987lAl^z!S0*lAl^z!7.lAl^z!S%9:`^{]A8=`^{!*"
    "0^z!./"
    "^z!-'k`^{!2YBlk!B8E(i$+bQ^~YH^zz!4Clv6]FClv5].Clv4!LClv3!8Clv2!?Clv1!"
    "6Clv0!+Clv/]DClv.]:Clu!=Clt!:Cls!0Clr!/"
    "Clq!HClp!EClo!S,Cln!)Clm!(Cll!,'lk^zy";

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

#define VM_HALT 6

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

// the only three roots allowed
obj stack = NUM_0;
obj pc = NUM_0;
obj FALSE = NUM_0;

// global, but not a root, referenced
obj symbol_table = NUM_0;

size_t pos = 0;

rib *heap_start;

// GC
#define MAX_NB_OBJS 15000
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

register obj *alloc asm("edi");
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
    push2(clmp, NUM_0);
    break;
  }
  case 1: { // id
    PRIM1();
    push2(x, NUM_0);
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
    push2(x, NUM_0);
    break;
  }
  case 4: { // unk
    obj x = CAR(TOS);
    obj y = CDR(stack);
    TOS = TAG_RIB(alloc_rib(x, y, TAG_NUM(1)));
    break;
  }
  case 5: { // is rib?
    PRIM1();
    push2(boolean(IS_RIB(x)), NUM_0);
    break;
  }
  case 6: { // field0
    PRIM1();
    push2(CAR(x), NUM_0);
    break;
  }
  case 7: { // field1
    PRIM1();
    push2(CDR(x), NUM_0);
    break;
  }
  case 8: { // field2
    PRIM1();
    push2(TAG(x), NUM_0);
    break;
  }
  case 9: { // set field0
    PRIM2();
    push2(CAR(x) = y, NUM_0);
    break;
  }
  case 10: { // set field1
    PRIM2();
    push2(CDR(x) = y, NUM_0);
    break;
  }
  case 11: { // set field2
    PRIM2();
    push2(TAG(x) = y, NUM_0);
    break;
  }
  case 12: { // eq
    PRIM2();
    push2(boolean(x == y), NUM_0);
    break;
  }
  case 13: { // lt
    PRIM2();
    push2(boolean(x < y), NUM_0);
    break;
  }
  case 14: { // add
    PRIM2();
    push2(x + y - 1, NUM_0);
    break;
  }
  case 15: { // sub
    PRIM2();
    push2(x - y + 1, NUM_0);
    break;
  }
  case 16: { // mul
    PRIM2();
    push2(TAG_NUM((NUM(x) * NUM(y))), NUM_0);
    break;
  }
  case 17: { // div
    PRIM2();
    push2(TAG_NUM((NUM(x) / NUM(y))), NUM_0);
    break;
  }
  case 18: { // getc
    int read;
#ifdef NO_STD
    if (pos < input_len) {
      read = (int)get_byte();
    } else {
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
    }
#else
    read = pos < input_len ? get_byte() : getchar();
#endif
    push2(TAG_NUM(read), NUM_0);
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
    push2(x, NUM_0);
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
    case VM_HALT: { // halt
      vm_exit(0);
      return;
    }
    case 0: // jump
    case 1: // call
    {
#ifdef DEBUG_I_CALL
      printf(instr ? "--- jump " : "--- call ");
      show_operand(o);
      PRINTLN();
#endif
#define jump_target CAR(get_opnd(CDR(pc)))
      bool call = instr;
      if (IS_NUM(jump_target)) {
        prim(NUM(jump_target));

        if (!instr) {
          // jump
          pc = get_cont();
          CDR(stack) = CAR(pc);
        }
        pc = TAG(pc);
      } else {
        num argc = NUM(TOS);
        // Use the car of the PC to save the new PC
        CAR(pc) = CAR(get_opnd(CDR(pc)));

        pop();

        obj s2 = TAG_RIB(alloc_rib(NUM_0, get_opnd(CDR(pc)), NUM_0));

        for (int i = 0; i < argc; ++i) {
          s2 = TAG_RIB(alloc_rib(pop(), s2, NUM_0));
        }

        obj c2 = TAG_RIB(list_tail(RIB(s2), argc));
        if (call) {
          CAR(c2) = stack;
          TAG(c2) = TAG(pc);
        } else {
          obj k = get_cont();
          CAR(c2) = CAR(k);
          TAG(c2) = TAG(k);
        }

        stack = s2;

        obj new_pc = CAR(pc);
        CAR(pc) = TAG_NUM(instr);
        pc = TAG(new_pc);
      }
      break;
    }
#undef jump_target
    case 2: { // set
#ifdef DEBUG_I_CALL
      printf("--- set ");
      show_operand(o);
      PRINTLN();
#endif
      obj x = pop();
      ((IS_NUM(CDR(pc))) ? list_tail(RIB(stack), NUM(CDR(pc))) : RIB(CDR(pc)))
          ->fields[0] = x;
      ADVANCE_PC();
      break;
    }
    case 3: { // get
#ifdef DEBUG_I_CALL
      printf("--- get ");
      show_operand(o);
      PRINTLN();
#endif
      push2(get_opnd(CDR(pc)), NUM_0);
      ADVANCE_PC();
      break;
    }
    case 4: { // const
#ifdef DEBUG_I_CALL
      printf("--- const ");
      PRINTLN();
#endif
      push2(CDR(pc), NUM_0);
      ADVANCE_PC();
      break;
    }
    case 5: { // if
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
  rib *list = alloc_rib(name, lst_length(name), TAG_NUM(3));
  rib *sym = alloc_rib(FALSE, TAG_RIB(list), TAG_NUM(4));
  rib *root = alloc_rib(TAG_RIB(sym), symbol_table, NUM_0);
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

    accum = TAG_RIB(alloc_rib(TAG_NUM(c), TAG_RIB(accum), NUM_0));
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
            alloc_rib(TAG_RIB(alloc_rib2(n, NUM_0, pop())), NIL, TAG_NUM(1)));
        if (stack == NUM_0 || stack == NULL) {
          break;
        }
        op = 4;
      }
    }

    rib *c = alloc_rib(TAG_NUM(op), n, 0);
    c->fields[2] = TOS;
    TOS = TAG_RIB(c);
  }

  pc = TAG(CAR(n));
}

void setup_stack() {
  push2(NUM_0, NUM_0);
  push2(NUM_0, NUM_0);

  obj first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;

  CAR(first) = TAG_NUM(VM_HALT);
  CDR(first) = NUM_0;
  TAG(first) = NUM_0;
}

#ifdef NOSTART
void _start() {
#else

void init() {
#endif
  init_heap();

  FALSE = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, TAG_NUM(5))),
                            TAG_RIB(alloc_rib(NUM_0, NUM_0, TAG_NUM(6))),
                            TAG_NUM(4)));

  build_sym_table();
  decode();

  set_global(symbol_table);
  set_global(FALSE);
  set_global(TRUE);
  set_global(NIL);

  set_global(TAG_RIB(alloc_rib(NUM_0, NUM_0, TAG_NUM(1)))); /* primitive 0 */

  setup_stack();

  run();
}

#ifndef NOSTART

int main() { init(); }

#endif

#pragma clang diagnostic pop
