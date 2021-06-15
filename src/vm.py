# uVM implementation in Python
# Clumps are implemented as 3-element list so we can mutate them
# This file should read as a big class without the nesting; state
# is global.
from sys import stdin
from operator import add as math_add, \
    mul as math_mul, \
    ifloordiv as math_div, \
    sub as math_sub, \
    lt as math_lt, \
    eq as math_eq
from functools import reduce


def to_fixnum(x):
    if isinstance(x, bool):
        x = 1 if x else 0

    return (x << 1) | 1


def from_fixnum(x):
    return x >> 1


TAG_PAIR = to_fixnum(0)
TAG_PROC = to_fixnum(1)
TAG_STR = to_fixnum(2)
TAG_SYM = to_fixnum(3)
TAG_TRUE = to_fixnum(4)
TAG_FALSE = to_fixnum(5)
TAG_NUL = to_fixnum(6)

NIL = to_fixnum(0)

CAR_I = 0
CDR_I = 1
TAG_I = 2
CLUMP_SIZE = 3

# Top of the stack
stack = NIL

# Program Counter
pc = stack


def __obj_to_str(obj):
    if isinstance(obj, list):
        return "CLMP"
    elif isinstance(obj, int):
        return str(from_fixnum(int(obj)))
    else:
        raise Exception("unknown clump value: " + str(type(obj)))


def __print_stack():
    global stack
    explorer = stack
    while explorer != NIL:
        print(f"-->({__obj_to_str(explorer[0])}, {__obj_to_str(explorer[1])}, {__obj_to_str(explorer[2])})")
        explorer = explorer[CDR_I]


def __push_and_set(x):
    push_clump()
    stack[CAR_I] = to_fixnum(x)


def push_clump():
    """
    Push a new clump on top of the stack
    The CDR field will be the previous stack
    The TAG field will be 0
    :return:
    """
    global stack
    new_clump = [NIL] * CLUMP_SIZE
    new_clump[CDR_I] = stack
    new_clump[TAG_I] = TAG_PAIR

    stack = new_clump


def pop_clump():
    """
    Remove the clump currently on top of the stack
    :return:
    """
    global stack
    stack = stack[CDR_I]


def __field_x(x):
    global stack
    field = stack[CAR_I][x]
    push_clump()
    stack[0] = field


def field_0():
    __field_x(0)


def field_1():
    __field_x(1)


def field_2():
    __field_x(2)


def __field_x_set(x):
    global stack
    v = stack[CAR_I]
    pop_clump()
    stack[CAR_I][x] = v


def field_0_set():
    __field_x_set(0)


def field_1_set():
    __field_x_set(1)


def field_2_set():
    __field_x_set(2)


def __pop(n):
    """
    Retrieve n elements from the stack (0th field), popping n-1 elements (leaving the last
    clump so it can be overwritten)
    :param n:
    :return:
    """
    assert n >= 1, "Cannot pop 0 elements"

    result = []
    while n != 1:
        result.append(stack[CAR_I])
        pop_clump()
        n -= 1
    result.append(stack[CAR_I])

    return result


def clump():
    stack = __pop(CLUMP_SIZE)


def __binop(op):
    global stack
    stack[CAR_I] = to_fixnum(reduce(op, map(from_fixnum, __pop(2))))


def lt(): __binop(math_lt)
def eq(): __binop(math_eq)
def add(): __binop(math_add)
def sub(): __binop(math_sub)
def mul(): __binop(math_mul)
def div(): __binop(math_div)

def putchar():
    char = from_fixnum(stack[CAR_I])
    ascii_char = chr(char)
    print(ascii_char, sep='')


def getchar():
    push_clump()
    # TODO: no enter? is it my env?
    c = to_fixnum(ord(stdin.read(1)))
    stack[CAR_I] = c


def __env():
    """
    Get the clump of the environment
    :return:
    """
    slow_scout = None
    scout = stack

    # slow_scout    ... scout
    # (?, ->, ?) ... (?, ->, 1)
    while scout[TAG_I] != TAG_PROC:
        slow_scout = scout
        scout = scout[CDR_I]

    assert slow_scout is not None, "There must be an env. clump"
    return slow_scout


def __skip_n(n):
    """
    Skip n clumps
    :param n:
    :return:
    """

    scout = stack
    while n != 0:
        scout = scout[CDR_I]
        n -= 1

    return scout


def call(proc_clump):
    return __call_or_jump(True, proc_clump)


def jump(proc_clump):
    return __call_or_jump(False, proc_clump)


# map an integer to a primitive operation
int_to_prim = [
    add,
    sub,
    mul,
    div,
    lt,
    eq,
    clump,
    push_clump,
    pop_clump,
    putchar,
    getchar,
    field_0,
    field_1,
    field_2,
    field_0_set,
    field_1_set,
    field_2_set,
    call,
    jump
]


def __call_or_jump(is_call: bool, proc_clump):
    """
    Call a procedure
    Assumes:
    :return:
    """
    global stack, pc

    proc_code = proc_clump[CAR_I]
    args = proc_code[CAR_I]
    code = proc_code[CDR_I]

    is_primitive = isinstance(code, int)  # if the procedure is a primitive, we can exec. it

    [curr_env, _, curr_code] = __env()
    old_env = __skip_n(args)

    if is_call:
        if is_primitive:
            primitive_code = from_fixnum(code)
            prim = int_to_prim[primitive_code]
            prim()
        else:
            push_clump()

            stack[CAR_I] = old_env
            stack[CDR_I] = proc_clump
            stack[TAG_I] = pc

            pc = code
    else:  # is a jump
        if is_primitive:

            primitive_code = from_fixnum(code)
            prim = int_to_prim[primitive_code]
            prim()

            stack[CDR_I] = curr_env
            pc = curr_code
        else:
            push_clump()

            stack[CAR_I] = curr_env
            stack[CDR_I] = proc_clump
            stack[TAG_I] = curr_code

            pc = code


def test_jump_prim():
    # we want to jump to add, to add 5,6
    # nothing in env
    global stack, pc
    push_clump()
    stack[TAG_I] = TAG_PROC

    push_clump()

    __push_and_set(5)
    __push_and_set(6)

    __print_stack()  # expecting 3, 2, 0, 0 where the last clump is 1-tagged

    frozen = stack  # save the current stack

    push_clump()  # push a function clump for BP

    add_idx = int_to_prim.index(add)

    # cheat to rewrite the stack correctly
    prim_clump = [2, to_fixnum(add_idx), 0]
    stack[0] = prim_clump

    proc_clump = stack
    stack = frozen

    jump(proc_clump)
    print("--After jump--")

    __print_stack()  # expecting to see 11 on TOS with 0,0 following


def main():
    test_jump_prim()


if __name__ == '__main__':
    main()
