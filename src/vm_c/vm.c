// debug instruction calls?
#ifdef DEBUG_I_CALL
#define DEBUG
#endif


#ifdef DEBUG

#include <stdio.h>

#define PRINTLN() do { printf("\n"); } while(0)

#endif

#ifndef NO_STD

#include <stdio.h>
#include <stdlib.h>

#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-pragmas"
#pragma ide diagnostic ignored "ConstantFunctionResult"

int input_len = 1728;
char *input = "#etouq,,fi,,!rdc-tes,,esrever,?erudecorp,enifed,,,!rac-tes,,,!tes,,,,adbmal,rddc,lobmys>-gnirts,cossa,fer-tsil,gnirts>-lobmys,,gnirts>-tsil,,tneitouq,qssa,?gnirts,,enilwen,ton,rebmun>-gnirts,lobmys-denretninu>-gnirts,lave,,rdddac,,,*,,?lobmys,,,,,,,,daer,,etirw,,,rahc-keep,,,?llun,htgnel,tsil>-gnirts,,+,,-,,,,,,?lauqe,,rddac,rdac,,<,,rahctup,=,rahc-daer,,?riap,snoc,rac,?qe,,rdc,,,,,,,;9']'9'@Z3@YLZ/YN@TvCvR3y]/94]4Z8^z]88Gi&>GjOai&kkz]O>kw(k!C(_.YCaA_D^~F^{!;(^8;YBlbA`^(`~C_D_~F_|!J8HAaRk_D`0YJdAbEai$D`^~F_|]:'`ko89^~i$'`ko89^~i$'`ko89^~Cw)M^~ClK^~YA^z]$'Z:a_m{!H'a_l'k_k~CjO_{!0'b`o8JEdDbAa_'YHewS#'d~YFbYGi&>GjOOeYCEEfi$i$akYE_oN`~Cx@^0>GgZ-ecGfOdbpNa_~CxP^0Z$dRlbNbOa_~CxJ^8JEdDbAa_'YHewS#'d~YFbYGi&>GjOOeYCEEfi$i$akYE_oN`~Cx@^0>GgZ-ecGfOdbpNa_~CxP^0Z$dRlbNbOa_~i%~CxD^'cNao~CwS$^D_~F_'bRk``n~Z(_|!=3_@J^{]33uy]%3^3^@Z%_~L`kYBWZ*u``vR%Z7u^z!M(i$8MA^@JD^~F^z]#(i$(i$9#A^@YLD^~F^@JvC~F^z!L9%^8MYDZ;^~Z(^3vE@YMYD^@JvE~Z5^3vL@Z#A^@YLD^@JvK~F^8=vLvK~YF^8=vS;vF~Ci%^8=vS-vF~Z2^z])9)8>~Iu^(^~Lk^Hy!>8>@H(^9)~IvR0^~L_vC(^~Lk^YIy!<.S^@H(i&~LvD^.S^@H(i&~i%~IvL^.S^@H(i&~i%~IvK^YIy]&.Z&^YN(i&@H~IvL^Uy!N9>_(^~^Z1^Z9ES^@H.Ei&YNwS$@H~IvJ^(i%@H(i$@H~IvS-^YI@H~IvF^9&@H~IvK^(^~Lk^Uy!I(^(^@YK_jFYO~IjA^KjFy!1(^@YKjAjF8O~IjA^KjFy]F>kkjA]AWmk]+(_9+EaD_A^~F^{]L9+i&^z]=(i$9=Aa_(^~QD__D_~F_{]6(i$96Aa_(^~CD__D_~F_{!E(k8BYEA_l~F^z]<-^9<Wl`A^~L`k{!:(i$(i$(i$(i$8:P`P^~QM`M^~QK`K^~YA_~YA^(i%~C`^{].(^!#>ki#^Z0^9.Ma_(^~Q`M^K_~F_{]>9.i#^z!P(_(i$(i$8PYBWvR%`Z*buA_~LvR/^~L_vR$D^~F^{]18PkYD^z!2,`^{!F,i&^z]I9,`^{]B4^z];6^z]0'n_kz](8?n^z!D4^z]9'mk^z]58?m^z]C6^z]M4^z!G'l`^{]K8?l^z]2,i$^z]-88A^z!887A^z]?*A^z!7-A^z]N9,`^{]G8K`^{!*6^z!-4^z!.'k`^{!/8?k^z!?(i$,`P^~YA^{!3Bv6!OBv5]7Bv4]*Bv3!@Bv2!BBv1!5Bv0!,Bv/]EBv.],Bu!KBt!9Bs!6Br!4Bq!ABp!S#Bo]HBn!)Bm!(Bl!+'lk^zy";


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


// a clump obj
#define CLUMP_NB_FIELDS 3
typedef struct {
    obj fields[CLUMP_NB_FIELDS];
} clump;

#define EXIT_HEAP_OVERFLOW 5
#define EXIT_ILLEGAL_INSTR 6
#define EXIT_NO_MEMORY 7

#define VM_HALT 6

#define UNTAG(x) ((x) >> 1)
#define CLUMP(x) ((clump*)(UNTAG(x)))
#define NUM(x) ((num)(UNTAG((num)(x))))
#define IS_NUM(x) ((x) & 1)
#define IS_CLUMP(x) (!IS_NUM(x))
#define TAG_CLUMP(c_ptr) (((obj)(c_ptr)) << 1)
#define TAG_NUM(num) ((((obj)(num)) << 1) | 1)

#define PRIM1() obj x = pop()
#define PRIM2() obj y = pop(); PRIM1()
#define PRIM3() obj z = pop(); PRIM2()

#define CAR(x) CLUMP(x)->fields[0]
#define CDR(x) CLUMP(x)->fields[1]
#define TAG(x) CLUMP(x)->fields[2]
#define TOS CAR(stack)

#define nil (TAG_NUM(0))

// the only two roots allowed
obj stack = nil;
obj pc = nil;

// global, but not a root, referenced
obj symbol_table = nil;

size_t pos = 0;

clump *heap_start;

// GC
#define MAX_NB_OBJS 5000
#define SPACE_SZ (MAX_NB_OBJS * CLUMP_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_mid (heap_bot + (SPACE_SZ))
#define heap_top (heap_bot + (SPACE_SZ << 1))
#define IN_HEAP(ptr) ({ unsigned long __ptr = (unsigned long)(ptr);__ptr >= ((unsigned long)heap_bot) && __ptr < ((unsigned long)heap_top); })

#ifdef NO_STD
#define vm_exit(code) do{asm volatile ( "mov $0x01, %%eax\nmov %0, %%ebx\nint $0x80" : : "i"(code)); } while(0)
#else
#define vm_exit(code) do { exit((code)) } while(0)
#endif

obj *alloc;
obj *alloc_limit;
obj *scan;


#ifdef NO_STD

void *sys_brk(void *addr) {
    long ptr;

    asm volatile (
    "mov %0, %%ebx\n"
    "mov $0x2d, %%eax\n"
    "int $0x80\n"
    : "=a" (ptr)
    : "g"((long) addr)
    : "ebx" );

    return (void *) ptr;
}

#endif

void init_heap() {
#ifdef NO_STD
    heap_start = sys_brk((void *) NULL);
    void *new_brk = sys_brk((void *) heap_top);

    if (new_brk == heap_start) {
        vm_exit(EXIT_NO_MEMORY);
    }

#else
    heap_start = malloc(sizeof(obj) * (SPACE_SZ << 1));

    if(!heap_start) {
        vm_exit(EXIT_NO_MEMORY);
    }
#endif

    alloc = heap_bot;
    alloc_limit = heap_mid;
    stack = nil;
}

// NULL is a pointer (0) but would represent NULL
// so it is never present in an obj field, and
// cannot be a number because it is even. This
// saves a couple of bytes v.s having STACK
// as the broken heart value
#define GC_COPIED_OBJ ((obj)NULL)

void copy() {
    obj o = *scan;
    // we sometime reference clump that are allocated in BSS,
    // we do not want to copy those
    if (IS_CLUMP(o) && IN_HEAP((void *) CLUMP(o))) {
        obj *ptr = CLUMP(o)->fields;
        obj field0 = ptr[0];
        obj copy;

        if (field0 == GC_COPIED_OBJ) {
            copy = ptr[1]; // copied, get new address
        } else {
            copy = TAG_CLUMP(alloc);
            *ptr++ = GC_COPIED_OBJ; // ptr points to CDR
            *alloc++ = field0;
            *alloc++ = *ptr++; // ptr points to TAG
            *alloc++ = *ptr;
            ptr[-1] = copy; // set forward ptr. Since it points to TAG, ptr[-1] rewrites the CDR
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
    if (stack != nil) {
        scan = &stack;
        copy();
    }

    // root: pc
    if (pc != nil) {
        scan = &pc;
        copy();
    }

    // scan the to_space to pull all live references
    scan = to_space;
    while (scan != alloc) {
        copy();
        if (alloc >= alloc_limit) {
            vm_exit(EXIT_HEAP_OVERFLOW);
        }
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

void push(obj val) {
    // default stack frame is (value, ->, nil)
    *alloc++ = val;
    *alloc++ = stack;
    *alloc++ = nil;

    stack = TAG_CLUMP((clump *) (alloc - CLUMP_NB_FIELDS));

    if (alloc == alloc_limit) {
        gc();
    }
}

clump *alloc_clump(obj car, obj cdr, obj tag) {
    push(car);
    obj allocated = stack;

    obj old_stack = CDR(allocated);
    stack = old_stack;

    CDR(allocated) = cdr;
    TAG(allocated) = tag;

    return CLUMP(allocated);
}

char get_byte() {
    return input[pos++];
}

num get_code() {
    num x = get_byte() - 35;
    return x < 0 ? 57 : x;
}

num get_int(num n) {
    num x = get_code();
    n *= 46;
    return x < 46 ? n + x : get_int(n + x - 46);
}

clump *list_tail(clump *lst, num i) {
    return (i == 0) ? lst : list_tail(CLUMP(lst->fields[1]), i - 1);
}

obj list_ref(clump *lst, num i) {
    return list_tail(lst, i)->fields[0];
}

obj get_opnd(obj o) {
    return (IS_NUM(o) ? list_tail(CLUMP(stack), NUM(o)) : CLUMP(o))->fields[0];
}

obj get_cont() {
    obj s = stack;

    while (!NUM(TAG(s))) {
        s = CDR(s);
    }

    return s;
}

clump FALSE = {nil, nil, TAG_NUM(4)};
clump TRUE = {nil, nil, TAG_NUM(5)};
clump NIL = {nil, nil, TAG_NUM(6)};

#ifdef DEBUG

void chars2str(obj o) {
    if (o != TAG_CLUMP(&NIL)) {
        printf("%c", (char) (NUM(CLUMP(o)->fields[0]) % 256));
        chars2str(CLUMP(o)->fields[1]);
    }
}

void sym2str(clump *c) {
    chars2str(CLUMP(c->fields[1])->fields[0]);
}

void show_operand(obj o) {
    if (IS_NUM(o)) {
        printf("int %ld", NUM(o));
    } else {
        printf("sym ");
        sym2str(CLUMP(o));
    }
}

#endif

obj boolean(bool x) {
    return TAG_CLUMP(x ? &TRUE : &FALSE);
}

void run() {
#define ADVANCE_PC() do {pc = TAG(pc); } while(0)
    while (1) {
        num instr = NUM(CAR(pc));
        obj o = CDR(pc);

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
                obj new_pc;
                if (IS_NUM(jump_target)) {
                    switch (NUM(jump_target)) {
                        case 0: { // clump
                            obj clmp = TAG_CLUMP(alloc_clump(0, 0, 0));
                            PRIM3();
                            CAR(clmp) = x;
                            CDR(clmp) = y;
                            TAG(clmp) = z;
                            push(clmp);
                            break;
                        }
                        case 1: { // id
                            PRIM1();
                            push(x);
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
                            push(x);
                            break;
                        }
                        case 4: { // unk
                            obj x = CAR(TOS);
                            obj y = CDR(stack);
                            obj z = TAG_NUM(1);
                            TOS = TAG_CLUMP(alloc_clump(x, y, z));
                            break;
                        }
                        case 5: { // is clump?
                            PRIM1();
                            push(boolean(IS_CLUMP(x)));
                            break;
                        }
                        case 6: { // field0
                            PRIM1();
                            push(CAR(x));
                            break;
                        }
                        case 7: { // field1
                            PRIM1();
                            push(CDR(x));
                            break;
                        }
                        case 8: { // field2
                            PRIM1();
                            push(TAG(x));
                            break;
                        }
                        case 9: { // set field0
                            PRIM2();
                            push(CAR(x) = y);
                            break;
                        }
                        case 10: { // set field1
                            PRIM2();
                            push(CDR(x) = y);
                            break;
                        }
                        case 11: { // set field2
                            PRIM2();
                            push(TAG(x) = y);
                            break;
                        }
                        case 12 : { // eq
                            PRIM2();
                            push(boolean(x == y));
                            break;
                        }
                        case 13: { // lt
                            PRIM2();
                            push(boolean(x < y));
                            break;
                        }
                        case 14: { // add
                            PRIM2();
                            push(x + y - 1);
                            break;
                        }
                        case 15 : { // sub
                            PRIM2();
                            push(x - y + 1);
                            break;
                        }
                        case 16: { // mul
                            PRIM2();
                            push(TAG_NUM((NUM(x) * NUM(y))));
                            break;
                        }
                        case 17: { // div
                            PRIM2();
                            push(TAG_NUM((NUM(x) / NUM(y))));
                            break;
                        }
                        case 18: { // getc
                            int read;
#ifdef NO_STD
                            if (pos < input_len) {
                                read = (int) get_byte();
                            } else {
                                asm volatile ("push %%eax\n"
                                              "mov $0x03, %%eax\n" // sys_call
                                              "mov $0, %%ebx\n" // fd
                                              "lea 0(%%esp), %%ecx\n" // ptr
                                              "mov $1, %%edx\n" // count
                                              "int $0x80\n"
                                              "pop %%eax\n"
                                : "=a"(read)
                                :
                                : "ebx", "ecx", "edx", "esi", "edi");
                                read &= 0xFF;
                            }
#else
                            c = pos < input_len ? get_byte() : getchar();
#endif
                            push(TAG_NUM(read));
                            break;

                        }
                        case 19: { // putc
                            PRIM1();
#ifdef NO_STD
                            {
                                asm volatile ("mov %0, %%eax\n"
                                              "push %%eax\n"
                                              "mov $1, %%ebx\n" // fd
                                              "lea 0(%%esp), %%ecx\n" // ptr
                                              "mov $1, %%edx\n" // count
                                              "mov $0x04, %%eax\n" // sys_call
                                              "int $0x80\n"
                                              "pop %%eax\n"
                                :
                                : "g"((int) (NUM(x) & 0xFF))
                                : "eax", "ebx", "ecx", "edx", "esi", "edi");
                            }
#else
                            putchar((char) NUM(x));
                            fflush(stdout);
#endif
                            push(x);
                            break;
                        }
                    }

                    if (instr) {
                        // call
                        new_pc = pc;
                    } else {
                        // jump
                        new_pc = get_cont();
                        CDR(stack) = CAR(new_pc);
                    }
                } else {
                    clump *c2 = alloc_clump(nil, o, nil);
                    clump *s2 = c2;

                    num nargs = NUM(CAR(jump_target));

                    while (nargs--) {
                        s2 = alloc_clump(pop(), TAG_CLUMP(s2), nil);
                    }

                    if (IS_NUM(CAR(pc)) && NUM(CAR(pc))) {
                        c2->fields[0] = stack;
                        c2->fields[2] = TAG(pc);
                    } else {
                        obj k = get_cont();
                        c2->fields[0] = CAR(k);
                        c2->fields[2] = TAG(k);
                    }

                    stack = TAG_CLUMP(s2);
                    new_pc = jump_target;
                }
                pc = TAG(new_pc);
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
                ((IS_NUM(o)) ? list_tail(CLUMP(stack), NUM(o)) : CLUMP(o))->fields[0] = x;
                ADVANCE_PC();
                break;
            }
            case 3: { // get
#ifdef DEBUG_I_CALL
                printf("--- get ");
                show_operand(o);
                PRINTLN();
#endif
                push(get_opnd(o));
                ADVANCE_PC();
                break;
            }
            case 4: { // const
#ifdef DEBUG_I_CALL
                printf("--- const ");
                PRINTLN();
#endif
                push(o);
                ADVANCE_PC();
                break;
            }
            case 5: { // if
#ifdef DEBUG_I_CALL
                printf("--- if");
                PRINTLN();
#endif

                obj p = pop();
                if (p != TAG_CLUMP(&FALSE)) {
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


clump *symbol_ref(num n) {
    return CLUMP(list_ref(CLUMP(symbol_table), n));
}

clump *create_sym(clump *name) {
    clump *inner = alloc_clump(TAG_CLUMP(name), nil, TAG_NUM(2));
    clump *outer = alloc_clump(nil, TAG_CLUMP(inner), TAG_NUM(3));
    clump *root = alloc_clump(TAG_CLUMP(outer), symbol_table, nil);
    return root;
}

void build_sym_table() {
    num n = get_int(0);

    while (n > 0) {
        n--;
        symbol_table = TAG_CLUMP(create_sym(&NIL));
    }

    clump *accum = &NIL;

    while (1) {
        byte c = get_byte();

        if (c == 44) {
            symbol_table = TAG_CLUMP(create_sym(accum));
            accum = &NIL;
            continue;
        }

        if (c == 59) break;

        accum = alloc_clump(TAG_NUM(c), TAG_CLUMP(accum), nil);
    }

    symbol_table = TAG_CLUMP(create_sym(accum));
}

void set_global(clump *c) {
    CAR(CAR(symbol_table)) = TAG_CLUMP(c);
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
                stack = TAG_CLUMP(alloc_clump(nil, stack, nil));
            }

            // not very readable, see generic.vm.js

            if (n >= d) {
                n = (n == d) ? TAG_NUM(get_int(0)) : TAG_CLUMP(symbol_ref(get_int(n - d - 1)));
            } else {
                n = (op < 3) ? TAG_CLUMP(symbol_ref(n)) : TAG_NUM(n);
            }

            if (op > 4) {
                clump *inner = alloc_clump(n, nil, pop());
                n = TAG_CLUMP(alloc_clump(TAG_CLUMP(inner), TAG_CLUMP(&NIL), TAG_NUM(1)));
                if (stack == nil || stack == NULL) {
                    break;
                }
                op = 4;
            }
        }

        TOS = TAG_CLUMP(alloc_clump(TAG_NUM(op), n, TOS));
    }

    pc = TAG(CAR(n));
}

void setup_stack() {
    stack = TAG_CLUMP(alloc_clump(nil,
                                  nil,
                                  TAG_CLUMP(alloc_clump(
                                          TAG_NUM(VM_HALT),
                                          nil,
                                          nil))));
}

#ifdef NOSTART
void _start() {
#else

void init() {
#endif
    init_heap();

    build_sym_table();
    decode();

    set_global(CLUMP(symbol_table));
    set_global(&FALSE);
    set_global(&TRUE);
    set_global(&NIL);
    set_global(alloc_clump(nil, TAG_CLUMP(&NIL), TAG_NUM(1))); /* primitive 0 */

    setup_stack();

    run();
}

#ifndef NOSTART

int main() {
    init();
}

#endif

#pragma clang diagnostic pop