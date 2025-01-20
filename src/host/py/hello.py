#
# Hello World RVM in Python
#
# This file is a simple implementation of a Hello World RVM
# as described in the CONTRIBUTION.md file. This file
# showcases a standard implementation of a simple RVM
# in Python and can serve as a reference for other impementation.
#
# This RVM only implements the core features of Ribbit.
#

#
# To run this RVM, and get 'HELLO!' back, simply do:
#   python3 hello.py
#
# To run this RVM with the rsc compiler, give the option `--rvm host/py/hello.py`
# to the compiler
#

# ======================
# == (0) Input String ==
# ======================

# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
# RVM code that prints HELLO!
input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"
# )@@

# ========================
# == (1) VM definitions ==
# ========================
import sys
stdout=sys.stdout

# Functions to retrive bytes from the input string
pos=-1
def get_byte():
    """
    Get the next byte in input string.

    return: integer represeting the byte value (number between 0 and 255)
    """
    global pos
    pos += 1
    return ord(input[pos])

def get_code():
    """
    In Ribbit, the input string is optimized for space, meaning that only
    the subset of all writable characters (without escaping) is used. This
    character set is made of 92 characters:

      !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNO
      PQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~

    This function converts a byte into a number between 0 and 91 that represent
    it's position in this character-set.

    return: integer between 0 and 91 representing the position in the writable, non-escapable character-set
    """

    # Most writable characters are continuous, except for space (ascii 34) and `\` (ascii 92).
    # This trick allows us to write this function efficiently. The table below should help understand
    # the relationship between ascii codes, characters and the output.
    #
    # Ascii code | character | Writable Character Map (output)
    # 33         | !         | 57
    # 35-91      | # to [    | 0-56
    # 93-126     | ] to ~    | 58-91

    byte_value = get_byte() - 35
    if byte_value < 0:
        # for the ! character (ascii code 33)
        return 57
    else:
        return byte_value

def get_int(accumulator):
    """
    Recursively decodes a variable-length integer from the input string. The integer is encoded
    using a variable-length encoding format on 92 codes,where each byte contributes a portion
    of the total value.

    At each code checked, this function either stops and returns the integer or consumes
    the next byte to construct a bigger number. Codes between 0-45 tell the algorithm
    to stop and return the integer, while codes between 46-91 indicate that the function
    should continue reading more bytes to build a larger number.

    This algorithm is analog to a base 46 number representation. See the examples to understand
    the relationship:

     Codes in input |  Base 46 encoding                       | Decoded number (output)
     42             |  42 * (46^0)                            | 42
     47 17          |  1  * (46^1) + 17 * (46^0)              | 63
     56 61 03       |  10 * (46^2) + 15 * (46^1) + 3 * (46^0) | 21853


    Args:
      - accumulator: The accumulated value.

    Returns: The next integer encoded in the stack.

    See also:
     - https://en.wikipedia.org/wiki/Variable-length_quantity
     - https://simple.wikipedia.org/wiki/Base_(mathematics)
    """
    next_byte = get_code()
    accumulator *= 46
    if next_byte < 46:
        return accumulator + next_byte
    else:
        return get_int(accumulator + next_byte - 46)

# Representation of scheme constants as Ribs.
# Special values are tagged with a 5 at the end.
FALSE=[0,0,5] #  #f
TRUE=[0,0,5]  #  #t
NIL=[0,0,5]   # '()


stack=0 # The stack is encoded using a linked list of ribs.
def push(value):
    """
    Adds a new value on top of the stack.

    In Ribbit, Scheme lists are encoded using ribs, where the third field is a 0. For example,

      '()         => NIL
      '(42)       => [42, NIL, 0]
      '(42 43 44) => [42, [43, [44, NIL, 0], 0], 0]

    As the stack is encoded as a scheme list, we need to allocate a new rib to 'contain' the new
    value added to the stack.

    Args:
      - value: the value to push to stack
    """
    global stack

    stack = [value,stack,0]

def pop():
    """
    Pops the top value on the stack

    As the stack is encoded as a scheme list, we need to retrive its value in the first field.
    The stack is then mutated to the next value in the list, which is the second field of the rib.

    return: The value on top of the stack
    """
    global stack

    x = stack[0]
    stack = stack[1]

    return x

# =======================
# == (3) VM primitives ==
# =======================

def putchar(c):
    """
    Prints a character to the standard output.

    Args:
      - c: The character to print, as a integer
    """
    stdout.write(chr(c))
    stdout.flush()
    return c

def getchar():
    """
    Gets the next character from the standard input or -1 when
    no characters are available, for example, when end-of-file (eof)
    is reached.

    Return: The character in the standard input or -1.
    """
    c=sys.stdin.read(1)
    if len(c) == 1:
        push(ord(c))
    else:
        push(-1) #eof

# Utility functions to create primitives
# with a specific number of arguments. It :
#  1. Pops the specified of argument from the stack
#  2. Calls the function passed as argument with the
#     popped arguments
#  3. Pushes the result back to the stack
# This simplifies the creation of primitives as it can wrap
# a python function and transform it into a valid Ribbit
# primitive.
def prim1(f):
    return lambda: push(f(pop()))

def prim2(f):
    return lambda: push(f(pop(),pop()))

def prim3(f):
    return lambda: push(f(pop(),pop(),pop()))

def bool2scm(x):
    """
    Utilitary function to transform a Python boolean into a
    Scheme one.

    Args:
     - x: a python boolean

    Returns: The corresponding Scheme boolean
    """
    return TRUE if x else FALSE

def is_rib(x):
    """
    Utility function to check if a python object is a rib.

    Args:
     - x: A python object

    Returns: true if x is a rib, false otherwise.
    """
    return isinstance(x, list)

def field0set(y,x):
    x[0] = y
    return y

def field1set(y,x):
    x[1] = y
    return y

def field2set(y,x):
    x[2] = y
    return y


# Primitive table as defined in the paper. You can look at the CONTRIBUTING document for
# some usefull notes about each primitive.
primitives = [
 # @@(primitives (gen body)
 prim3(lambda z,y,x: [x,y,z]),                                           # @@(primitive (##rib a b c))@@
 prim1(lambda x:x),                                                      # @@(primitive (##id x))@@
 lambda:(pop(),0)[1],                                                    # @@(primitive (##arg1 x y))@@
 lambda:push([pop(),pop()][0]),                                          # @@(primitive (##arg2 x y))@@
 lambda:push([pop()[0],stack,1]),                                        # @@(primitive (##close rib))@@
 prim1(lambda x:bool2scm(is_rib(x))),                                    # @@(primitive (##rib? rib))@@
 prim1(lambda x:x[0]),                                                   # @@(primitive (##field0 rib))@@
 prim1(lambda x:x[1]),                                                   # @@(primitive (##field1 rib))@@
 prim1(lambda x:x[2]),                                                   # @@(primitive (##field2 rib))@@
 prim2(field0set),                                                       # @@(primitive (##field0-set! rib x))@@
 prim2(field1set),                                                       # @@(primitive (##field1-set! rib x))@@
 prim2(field2set),                                                       # @@(primitive (##field2-set! rib x))@@
 prim2(lambda y,x:bool2scm(x is y if is_rib(x) or is_rib(y) else x==y)), # @@(primitive (##eqv? x y))@@
 prim2(lambda y,x:bool2scm(x<y)),                                        # @@(primitive (##< a b))@@
 prim2(lambda y,x:x+y),                                                  # @@(primitive (##+ a b))@@
 prim2(lambda y,x:x-y),                                                  # @@(primitive (##- a b))@@
 prim2(lambda y,x:x*y),                                                  # @@(primitive (##* a b))@@
 prim2(lambda y,x:int(x/y)),                                             # @@(primitive (##quotient a b))@@
 getchar,                                                                # @@(primitive (##getchar))@@
 prim1(putchar),                                                         # @@(primitive (##putchar c))@@
 prim1(exit),                                                            # @@(primitive (##exit a))@@
 # )@@
]

# ======================
# == (4) Symbol table ==
# ======================

def list_tail(lst,i):
    """
    Given a liked list and an index, gets the ith element of this list

    returns:
      - the ith element of the list
    """
    while i>0:
        lst=lst[1]
        i-=1

    return lst

# Symbol table variable
symtbl=NIL

# The symbol table is built by reading the bytecode. The bytecode contains the
# symbol table in reverse order. This simplifies the creation of linked list of
# ribs with it, as we can just append new symbols to the head of the list.
#
# Each symbol contains the global value it refers to, its name as a sting and
# the symbol tag (2) :
#
#  Symbol : [ value, name, 2 ]
#
# In the creation of the symbol table, values associated with the symbol (global variables)
# are initiated to FALSE.

# The first byte of the bytecode tells the number of "empty symbols", meaning
# symbols whose name is the empty string, or in ribs: [NIL, 0, 3]
n = get_int(0)
for n in range(n):
    # Appends an empty symbol [ FALSE, "" , 2 ] to the symbol table
    symtbl = [ [ FALSE, [ NIL, 0, 3] , 2 ], symtbl , 0]

# The bytecode contains the symbols name in reverse order, separated by a comma.
# For example, the symbols foo and bar would be encoded as "rab,oof"

symbol_name = NIL   # The current symbol name (string)
symbol_name_len = 0 # The length of the current symbol name (int)

while True:
    c = get_byte() # current byte

    if c == ord(','): # end of symbol

        # Appends the symbol with its name to the symbol table
        symbol_to_append = [FALSE, [symbol_name, symbol_name_len, 3], 2]
        symtbl=[symbol_to_append, symtbl, 0]
        symbol_name = NIL
        symbol_name_len = 0
    else:
        if c == ord(';'): # end of symbol table
            # Appends last symbol and leave
            symbol_to_append = [FALSE, [symbol_name, n, 3], 2]
            symtbl=[symbol_to_append, symtbl, 0]
            break

        # Appends the current character to the symbol name
        symbol_name=[c,symbol_name,0]

        # Increment the length of the symbol name
        symbol_name_len += 1

def symbol_ref(n):
    """
    References the nth symbol in the symbol table

    Returns:
    - The nth symbol in the symbol table.
    """
    return list_tail(symtbl,n)[0]


# ========================================
# == (5) Decoding of the RVM's bytecode ==
# ========================================

while 1:
 x=get_code()
 n=x
 d=0
 op=0
 while 1:
  d=[20,30,0,10,11,4][op]
  if n<=2+d:break
  n-=d+3;op+=1
 if x>90:
  n=pop()
 else:
  if op==0:stack=[0,stack,0];op+=1
  n = get_int(0)if n==d else symbol_ref(get_int(n-d-1))if n>=d else symbol_ref(n)if op<3 else n
  if 4<op:
   n=[[n,0,pop()],NIL,1]
   if not stack:break
   op=4
 stack[0]=[op-1,n,stack[0]]


# =======================
# == (6) RVM execution ==
# =======================

pc = n[0][2]

get_opnd=lambda o:(o if is_rib(o) else list_tail(stack,o))

def get_cont():
 s=stack
 while not s[2]:s=s[1]
 return s

def set_global(val):
 global symtbl
 symtbl[0][0]=val
 symtbl=symtbl[1]

set_global([0,symtbl,1]) # primitive 0
set_global(FALSE)
set_global(TRUE)
set_global(NIL)

stack=[0,0,[5,0,0]] # primordial continuation (executes halt instr.)

while 1:
 o=pc[1]
 i=pc[0]
 if i<1: # jump/call
  o=get_opnd(o)[0]
  while 1:
   c=o[0]
   if is_rib(c):
    c2=[0,o,0]
    s2=c2
    nparams=c[0]>>1
    while nparams:
     s2=[pop(),s2,0]
     nparams-=1

    if pc[2]: # call
     c2[0]=stack
     c2[2]=pc[2]
    else: # jump
     k=get_cont()
     c2[0]=k[0]
     c2[2]=k[2]
    stack=s2
   else:
    o=primitives[c]()
    if is_rib(o): continue
    if pc[2]: # call
     c=pc
    else: # jump
     c=get_cont()
     stack[1]=c[0]
   pc=c
   break
 elif i<2: # set
  get_opnd(o)[0]=stack[0]; stack = stack[1]
 elif i<3: # get
  push(get_opnd(o)[0])
 elif i<4: # const
  push(o)
 elif i<5: # if
  if pop() is not FALSE:
   pc=pc[1]
   continue
 else: # halt
  break
 pc = pc[2]
