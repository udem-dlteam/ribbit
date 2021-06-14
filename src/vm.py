# uVM implementation in Python
# Clumps are implemented as 3-element list so we can mutate them
from sys import stdin
from operator import add as math_add, mul as math_mul, ifloordiv as math_div, sub as math_sub
from functools import reduce

CAR_I = 0
CDR_I = 1
TYPE_I = 2
CLUMP_SIZE = 3


def to_fixnum(x):
    return (x << 1) | 1


def from_fixnum(x):
    return x >> 1


nil = to_fixnum(0)
stack = [nil] * CLUMP_SIZE


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
    while explorer != nil:
        print(f"-->({__obj_to_str(explorer[0])}, {__obj_to_str(explorer[1])}, {__obj_to_str(explorer[2])})")
        explorer = explorer[CDR_I]


def push_clump():
    global stack
    new_clump = [nil] * CLUMP_SIZE
    new_clump[CDR_I] = stack
    stack = new_clump


def pop_clump():
    global stack
    stack = stack[CDR_I]


def field_x(x):
    clump = stack[CAR_I]
    return clump[x]


def field_0():
    return field_x(0)


def field_1():
    return field_x(1)


def field_2():
    return field_x(2)


def field_x_set(x, v):
    clump = stack[CAR_I]
    clump[x] = v


def field_0_set(v):
    field_x_set(0, v)


def field_1_set(v):
    field_x_set(1, v)


def field_2_set(v):
    field_x_set(2, v)


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


def lt():
    [left, right] = __pop(2)
    stack[CAR_I] = to_fixnum(1 if left < right else 0)


def eq():
    [left, right] = __pop(2)
    stack[CAR_I] = to_fixnum(1 if left == right else 0)


def clump():
    new_clump = __pop(3)
    stack[CAR_I] = new_clump


def binop(op):
    stack[CAR_I] = to_fixnum(reduce(op, map(from_fixnum, __pop(2))))


def add():
    binop(math_add)


def sub():
    binop(math_sub)


def mul():
    binop(math_mul)


def div():
    binop(math_div)


def putchar():
    char = from_fixnum(stack[CAR_I])
    ascii_char = chr(char)
    print(ascii_char, sep='')


def getchar():
    push_clump()
    # TODO: no enter? is it my env?
    c = to_fixnum(ord(stdin.read(1)))
    stack[CAR_I] = c


def vm():
    pass


def main():
    push_clump()
    stack[CAR_I] = to_fixnum(5)
    push_clump()

    stack[CAR_I] = to_fixnum(5)
    __print_stack()

    print()
    print()

    add()
    __print_stack()


if __name__ == '__main__':
    main()
