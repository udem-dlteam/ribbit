# #=# VM definitions
input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" # RVM code that prints HELLO!

FALSE=[0,0,5]
TRUE=[0,0,5]
NIL=[0,0,5]

stack=0
def push(x):
 global stack
 stack=[x,stack,0]

def pop():
 global stack
 x=stack[0]
 stack=stack[1]
 return x

pos=-1
def get_byte():
 global pos
 pos+=1
 return ord(input[pos])

def get_code():
 x=get_byte()-35
 return 57 if x<0 else x

def get_int(n):
 x=get_code()
 n*=46
 return n+x if x<46 else get_int(n+x-46)


# #=# Primitives
import sys
stdo=sys.stdout

putchar=lambda c:[stdo.write(chr(c)),stdo.flush(),c][2]

def getchar():
 c=sys.stdin.read(1)
 push(ord(c) if len(c) else -1)

def prim(n, f):
    return lambda:push(f(*[pop() for _ in range(n)]))

prim1=lambda f: prim(1, f)
prim2=lambda f: prim(2, f)
prim3=lambda f: prim(3, f)

def f0s(y,x):x[0]=y;return y
def f1s(y,x):x[1]=y;return y
def f2s(y,x):x[2]=y;return y

def bool2scm(x):
    return TRUE if x else FALSE

def is_rib(x):
    return isinstance(x, list)

def scm2str(s):
 def chars2str(c):
  return (chr(c[0]) + chars2str(c[1])) if c is not NIL else "" 
 return chars2str(s[0])

primitives = [
 # @@(primitives (gen body) 
 prim(3,lambda z,y,x:[x,y,z]),                                            # @@(primitive (##rib a b c))@@           
 prim(1,lambda x:x),                                                      # @@(primitive (##id x))@@
 lambda:(pop(),None)[1],                                                  # @@(primitive (##arg1 x y))@@
 lambda:push([pop(),pop()][0]),                                           # @@(primitive (##arg2 x y))@@
 lambda:push([pop()[0],stack,1]),                                         # @@(primitive (##close rib))@@
 prim(1,lambda x:bool2scm(is_rib(x))),                                    # @@(primitive (##rib? rib))@@
 prim(1,lambda x:x[0]),                                                   # @@(primitive (##field0 rib))@@
 prim(1,lambda x:x[1]),                                                   # @@(primitive (##field1 rib))@@
 prim(1,lambda x:x[2]),                                                   # @@(primitive (##field2 rib))@@
 prim(2,f0s),                                                             # @@(primitive (##field0-set! rib x))@@
 prim(2,f1s),                                                             # @@(primitive (##field1-set! rib x))@@
 prim(2,f2s),                                                             # @@(primitive (##field2-set! rib x))@@
 prim(2,lambda y,x:bool2scm(x is y if is_rib(x) or is_rib(y) else x==y)), # @@(primitive (##eqv? x y))@@
 prim(2,lambda y,x:bool2scm(x<y)),                                        # @@(primitive (##< a b))@@
 prim(2,lambda y,x:x+y),                                                  # @@(primitive (##+ a b))@@
 prim(2,lambda y,x:x-y),                                                  # @@(primitive (##- a b))@@
 prim(2,lambda y,x:x*y),                                                  # @@(primitive (##* a b))@@
 prim(2,lambda y,x:int(x/y)),                                             # @@(primitive (##quotient a b))@@
 getchar,                                                                 # @@(primitive (##getchar))@@
 prim(1,putchar),                                                         # @@(primitive (##putchar c))@@
 prim(1,exit),                                                            # @@(primitive (##exit a))@@
# )@@ 
]

# #=# Decode Symbol Table
symtbl=NIL
n=get_int(0)
while n>0:
 n-=1
 symtbl=[[FALSE,[NIL,0,3],2],symtbl,0]

accum=NIL
n=0
while 1:
 c=get_byte()
 if c==44:
  symtbl=[[FALSE,[accum,n,3],2],symtbl,0]; accum=NIL; n=0
 else:
  if c==59: break
  accum=[c,accum,0]
  n+=1

list_tail=lambda lst,i:lst if i==0 else list_tail(lst[1],i-1)
symtbl=[[FALSE,[accum,n,3],2],symtbl,0]
symbol_ref=lambda n: list_tail(symtbl,n)[0]

# #=# Decode instruction graph 

LENGTH_ARRAY=[20, 30, 0, 10, 11, 4]
while True:
    token = get_code()
    arg = token
    op = 0
    range_val = LENGTH_ARRAY[op]
    while range_val + 2 < arg:
        arg -= range_val + 3
        op += 1
        range_val = LENGTH_ARRAY[op]
    if token > 90:
        arg = pop()
    else:
        if not op:
            stack = [0, stack, 0]
        if arg >= range_val:
            if arg == range_val:
                arg = get_int(0)
            else:
                arg = symbol_ref(get_int(arg - range_val - 1))
        else:
            if op < 3:
                arg = symbol_ref(arg)
        if op > 4:
            arg = [[arg, 0, pop()], 0, 1]
            if not stack:
                break
            op = 4
    if op != 0:
        stack[0] = [op - 1, arg, stack[0]]
    else:
        stack[0] = [0, arg, stack[0]]

def set_global(x):
    global symtbl
    symtbl[0][0] = x
    symtbl = symtbl[1]

set_global([0, symtbl, 1])
set_global(FALSE)
set_global(TRUE)
set_global(NIL)

# #=# Execute RVM instructions

# We can take the pc from the last closure created
pc = arg[0][2]
stack=[0,0,[5,0,0]] # primordial continuation (executes halt instr.)

get_opnd=lambda o:(o if is_rib(o) else list_tail(stack,o))

def get_cont():
 s=stack
 while not s[2]:s=s[1]
 return s

while 1:
 o=pc[1]
 i=pc[0]
 if i<1: # jump/call
  o=get_opnd(o)[0]
  while 1:
   c=o[0]
   if is_rib(c):
    nargs=pop(); 
    c2=[0,o,0]
    s2=c2
    nparams=c[0]>>1
    if nparams > nargs if c[0]&1 else nparams != nargs:
     print("*** Unexpected number of arguments nargs:", nargs, "nparams", nparams, "variadics:", c[0]&1);
     exit(1)
    nargs-=nparams
    if c[0]&1: 
     rest=NIL
     while nargs:
      rest=[pop(), rest, 0]
      nargs-=1
      
     s2=[rest,s2,0]
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
    pop()
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
