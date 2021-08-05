#define NO_STD
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

#define nil (TAG_NUM(0))

obj stack = nil;
obj pc = nil;
size_t pos = 0;

clump *heap_start;
clump *heap_end;

// GC
#define MAX_NB_OBJS 100000
#define TOTAL_HEAP_SZ (MAX_NB_OBJS * CLUMP_NB_FIELDS * 2)
#define heap_bot ((obj *)(heap_start))
#define heap_mid (heap_bot + (TOTAL_HEAP_SZ>>1))
#define heap_top (heap_bot + TOTAL_HEAP_SZ)

obj *alloc;
obj *alloc_limit;
obj *scan;
obj broken_heart;


#ifdef NO_STD

// implementation modified from `my_syscall` in nolibc.h (linux kernel), x86-32
void *sys_brk(void *addr) {
    long ptr;

    register long _addr asm("ebx") = (long) (addr);

    asm volatile (
    "mov $0x2d, %%eax\n"
    "int $0x80\n"
    : "=a" (ptr)
    : "r"(_addr));

    return (void *) ptr;
}

#endif


void init_heap() {
#ifdef NO_STD
    heap_start = sys_brk((void *) NULL);
    void *new_brk = sys_brk(heap_top);
#else
    heap_start = malloc(TOTAL_HEAP_SZ);
#endif

    alloc = heap_bot;
    alloc_limit = heap_mid;
    stack = nil;
}

void copy() {
    obj o = *scan;
    if (IS_CLUMP(o)) {
        obj *ptr = CLUMP(o)->fields;
        obj field0 = ptr[0];
        obj copy;

        if (field0 == broken_heart) {
            copy = ptr[1];
        } else {
            copy = TAG_CLUMP(alloc);
            *ptr++ = broken_heart;
            *alloc++ = field0;
            *alloc++ = *ptr++;
            *alloc++ = *ptr;
            ptr[-1] = copy;
        }
        *scan = copy;
    }
    scan++;
}

void gc() {
    obj *start = (alloc_limit == heap_mid) ? heap_mid : heap_bot;
    alloc_limit = start + MAX_NB_OBJS * CLUMP_NB_FIELDS;
    alloc = start;

    broken_heart = stack;
    scan = &stack;
    copy();
    scan = start;

    while (scan != alloc) {
        copy();
    }
}


obj pop() {
    obj x = CLUMP(stack)->fields[0];
    stack = CLUMP(stack)->fields[1];
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
    clump *allocated = CLUMP(stack);

    obj old_stack = allocated->fields[1];
    stack = old_stack;

    allocated->fields[1] = cdr;
    allocated->fields[2] = tag;

    return allocated;
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

clump *get_cont() {
    clump *s = CLUMP(stack);

    while (!NUM(s->fields[2])) {
        s = CLUMP(s->fields[1]);
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
#define ADVANCE_PC() do {pc = CLUMP(pc)->fields[2]; } while(0)
    while (1) {
        obj o = CLUMP(pc)->fields[1];
        num instr = NUM(CLUMP(pc)->fields[0]);

        switch (instr) {
            default:
            case VM_HALT: { // halt
                return;
            }
            case 0: // jump
            case 1: // call
            {
#ifdef DEBUG
                printf(instr ? "--- jump " : "--- call ");
                show_operand(o);
                PRINTLN();
#endif
                o = get_opnd(o);
                obj c = (CLUMP(o))->fields[0];

                if (IS_NUM(c)) {
                    switch (NUM(c)) {
                        case 0: { // clump
                            clump *clmp = alloc_clump(0, 0, 0);
                            PRIM3();
                            clmp->fields[0] = x;
                            clmp->fields[1] = y;
                            clmp->fields[2] = z;
                            push(TAG_CLUMP(clmp));
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
                            obj x = CLUMP(CLUMP(stack)->fields[0])->fields[0];
                            obj y = CLUMP(stack)->fields[1];
                            obj z = TAG_NUM(1);
                            CLUMP(stack)->fields[0] = TAG_CLUMP(alloc_clump(x, y, z));
                            break;
                        }
                        case 5: { // is clump?
                            PRIM1();
                            push(boolean(IS_CLUMP(x)));
                            break;
                        }
                        case 6: { // field0
                            PRIM1();
                            push(CLUMP(x)->fields[0]);
                            break;
                        }
                        case 7: { // field1
                            PRIM1();
                            push(CLUMP(x)->fields[1]);
                            break;
                        }
                        case 8: { // field2
                            PRIM1();
                            push(CLUMP(x)->fields[2]);
                            break;
                        }
                        case 9: { // set field0
                            PRIM2();
                            push(CLUMP(x)->fields[0] = y);
                            break;
                        }
                        case 10: { // set field1
                            PRIM2();
                            push(CLUMP(x)->fields[1] = y);
                            break;
                        }
                        case 11: { // set field2
                            PRIM2();
                            push(CLUMP(x)->fields[2] = y);
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
                        c = pc;
                    } else {
                        // jump
                        clump *cont = get_cont();
                        c = TAG_CLUMP(cont);
                        CLUMP(stack)->fields[1] = get_cont()->fields[0];
                    }
                } else {
                    clump *c2 = alloc_clump(nil, o, nil);
                    clump *s2 = c2;
                    num nargs = NUM(CLUMP(c)->fields[0]);

                    while (nargs--) {
                        s2 = alloc_clump(pop(), TAG_CLUMP(s2), nil);
                    }

                    if (IS_NUM(CLUMP(pc)->fields[0]) && NUM(CLUMP(pc)->fields[0])) {
                        c2->fields[0] = stack;
                        c2->fields[2] = CLUMP(pc)->fields[2];
                    } else {
                        clump *k = get_cont();
                        c2->fields[0] = k->fields[0];
                        c2->fields[2] = k->fields[2];
                    }

                    stack = TAG_CLUMP(s2);
                }
                pc = CLUMP(c)->fields[2];
                break;
            }
            case 2: { // set
#ifdef DEBUG
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
#ifdef DEBUG
                printf("--- get ");
                show_operand(o);
                PRINTLN();
#endif
                push(get_opnd(o));
                ADVANCE_PC();
                break;
            }
            case 4: { // const
#ifdef DEBUG
                printf("--- const ");
                PRINTLN();
#endif
                push(o);
                ADVANCE_PC();
                break;
            }
            case 5: { // if
#ifdef DEBUG
                printf("--- if");
                PRINTLN();
#endif

                obj p = pop();
                if (p != TAG_CLUMP(&FALSE)) {
                    pc = CLUMP(pc)->fields[1];
                } else {
                    pc = CLUMP(pc)->fields[2];
                }
                break;
            }
        }
    }
#undef ADVANCE_PC
}


clump *symbol_table = &NIL;

clump *symbol_ref(num n) {
    return CLUMP(list_ref(symbol_table, n));
}

clump *create_sym(clump *name) {
    clump *inner = alloc_clump(TAG_CLUMP(name), nil, TAG_NUM(2));
    clump *outer = alloc_clump(nil, TAG_CLUMP(inner), TAG_NUM(3));
    clump *root = alloc_clump(TAG_CLUMP(outer), TAG_CLUMP(symbol_table), nil);
    return root;
}

void build_sym_table() {
    num n = get_int(0);

    while (n > 0) {
        n--;
        symbol_table = create_sym(&NIL);
    }

    clump *accum = &NIL;

    while (1) {
        byte c = get_byte();

        if (c == 44) {
            symbol_table = create_sym(accum);
            accum = &NIL;
            continue;
        }

        if (c == 59) break;

        accum = alloc_clump(TAG_NUM(c), TAG_CLUMP(accum), nil);
    }

    symbol_table = create_sym(accum);
}

void set_global(clump *c) {
    CLUMP(symbol_table->fields[0])->fields[0] = TAG_CLUMP(c);
    symbol_table = CLUMP(symbol_table->fields[1]);
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

        CLUMP(stack)->fields[0] = TAG_CLUMP(alloc_clump(TAG_NUM(op), n, CLUMP(stack)->fields[0]));
    }

    pc = CLUMP(CLUMP(n)->fields[0])->fields[2];
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

    set_global(symbol_table);
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