# @@(location import)@@

# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" # RVM code that prints HELLO!
# )@@


import numpy as np # @@(feature py/numpy)@@
import os # @@(feature py/io)@@

# @@(feature (or py/io %%getchar %%putchar)
import sys
stdo=sys.stdout
# )@@


# @@(feature %%putchar
putchar=lambda c:[stdo.write(chr(c)),stdo.flush(),c][2]
# )@@

# @@(feature %%getchar
def getchar():
 c=sys.stdin.read(1)
 push(ord(c) if len(c) else -1)
# )@@


# @@(feature debug
debug = True   
# )@@
# @@(feature (not debug)
debug = False  
# )@@


# @@(feature debug
tracing = False
step_count = 0
start_tracing = 0
next_stamp = 0
def show(obj, seen=[]):
 for x in seen:
  if obj is x: return "#<circ>"

 if not is_rib(obj): return str(obj)
 type = obj[2]
 if type == 4: return "#" + show(obj[0], seen + [obj])
 result = ""
 if type == 0:
  n = 1
  result += "(" + show(obj[0], seen+ [obj])
  obj = obj[1]
  while is_rib(obj) and obj[2] == 0:
   if n > 4:
    result += " ..."
    obj = NIL
    break
   result += " " + show(obj[0], seen+ [obj])
   obj = obj[1]
   n += 1
  if obj is not NIL:
   result += " . " + show(obj, seen+[obj])
  result += ")"
 elif type == 1:
  if is_rib(obj[0]):
   result += "#<procedure nparams=" + str(obj[0][0]) + ">"
  else:
   result += "#<primitive " + str(obj[0]) + ">"
 elif type == 2:
  obj = obj[1]
  if is_rib(obj) and obj[2] == 3 and obj[1] > 0:
   obj = obj[0]
   while is_rib(obj) and obj[2] == 0:
    result += chr(obj[0])
    obj = obj[1]
  else:
   result += "#<symbol " + show(obj, seen+[obj]) + ">"
 elif type == 3:
  result += "\""
  obj = obj[0]
  while is_rib(obj) and obj[2] == 0:
   c = chr(obj[0])
   if c == "\n": c = "n"; result += "\\"
   elif c == "\r": c = "r"; result += "\\"
   elif c == "\t": c = "t"; result += "\\"
   elif c == "\\" or c == "\"": result += "\\"
   result += c
   obj = obj[1]
  result += "\""
 elif type == 5:
  if obj is FALSE:
   result += "#f"
  elif obj is TRUE:
   result += "#t"
  elif obj is NIL:
   result += "()"
  else:
   result += "["+show(obj[0], seen+[obj])+","+show(obj[1], seen+[obj])+","+show(obj[2], seen+[obj])+"]"
 else:
  result += "["+show(obj[0], seen+[obj])+","+show(obj[1], seen+[obj])+","+show(obj[2], seen+[obj])+"]"
 return result

def start_step():
 global step_count, tracing, next_stamp
 step_count += 1
 if step_count >= start_tracing: tracing = True
 if not tracing:
  if step_count >= next_stamp:
   next_stamp = int(next_stamp * 1.01 + 1)
   print("@" + str(step_count))
  return
 s = stack
 result = "@" + str(step_count) + " STACK = ("
 sep = ""
 if s == 0:
  result += "EMPTY (probably an error)"
 else:
  while s[1]!=0: result += sep + show(s[0]); sep = " "; s=s[1]
 result += ")"
 print(result)

# )@@

# @@(location decl)@@

pos=-1
def get_byte():
 global pos
 pos+=1
 return ord(input[pos])

# VM

FALSE=[0,0,5]
TRUE=[0,0,5]
NIL=[0,0,5]

def bool2scm(x):
    return TRUE if x else FALSE

def is_rib(x):
    return isinstance(x, list)

stack=0

def push(x):
 global stack
 stack=[x,stack,0]

def pop():
 global stack
 x=stack[0]
 stack=stack[1]
 return x


# @@(feature scm2list
def scm2list(l):
 return [l[0]] + scm2list(l[1]) if l is not NIL else []
# )@@

# @@(feature list_str2scm (use str2scm)
def list_str2scm(l):
 return [str2scm(l[0]),list_str2scm(l[1:]),0] if len(l) else NIL
# )@@

# @@(feature scm2str
def scm2str(s):
 def chars2str(c):
  return (chr(c[0]) + chars2str(c[1])) if c is not NIL else "" 
 return chars2str(s[0])
# )@@

# @@(feature str2scm
def str2scm(s):
 def chars2scm(c):
  return [ord(c[0]),str2scm(c[1:]),0] if len(c) else NIL
 return [chars2scm(s),len(s),3]
# )@@

def prim1(f):
    return lambda: push(f(pop()))
def prim2(f):
    return lambda: push(f(pop(),pop()))
def prim3(f):
    return lambda: push(f(pop(),pop(),pop()))

def prim(n, f):
    return lambda:push(f(*[pop() for _ in range(n)]))

def f0s(y,x):x[0]=y;return y
def f1s(y,x):x[1]=y;return y
def f2s(y,x):x[2]=y;return y



primitives = [
 # @@(primitives (gen body) 
 prim(3,lambda z,y,x:[x,y,z]),                                            # @@(primitive (%%rib a b c))@@
 prim(1,lambda x:x),                                                      # @@(primitive (%%id x))@@
 lambda:(pop(),None)[1],                                                  # @@(primitive (%%arg1 x y))@@
 lambda:push([pop(),pop()][0]),                                           # @@(primitive (%%arg2 x y))@@
 lambda:push([pop()[0],stack,1]),                                         # @@(primitive (%%close rib))@@
 prim(1,lambda x:bool2scm(is_rib(x))),                                    # @@(primitive (%%rib? rib))@@
 prim(1,lambda x:x[0]),                                                   # @@(primitive (%%field0 rib))@@
 prim(1,lambda x:x[1]),                                                   # @@(primitive (%%field1 rib))@@
 prim(1,lambda x:x[2]),                                                   # @@(primitive (%%field2 rib))@@
 prim(2,f0s),                                                             # @@(primitive (%%field0-set! rib x))@@
 prim(2,f1s),                                                             # @@(primitive (%%field1-set! rib x))@@
 prim(2,f2s),                                                             # @@(primitive (%%field2-set! rib x))@@
 prim(2,lambda y,x:bool2scm(x is y if is_rib(x) or is_rib(y) else x==y)), # @@(primitive (%%eqv? x y))@@
 prim(2,lambda y,x:bool2scm(x<y)),                                        # @@(primitive (%%< a b))@@
 prim(2,lambda y,x:x+y),                                                  # @@(primitive (%%+ a b))@@
 prim(2,lambda y,x:x-y),                                                  # @@(primitive (%%- a b))@@
 prim(2,lambda y,x:x*y),                                                  # @@(primitive (%%* a b))@@
 prim(2,lambda y,x:int(x/y)),                                             # @@(primitive (%%quotient a b))@@
 getchar,                                                                 # @@(primitive (%%getchar))@@
 prim(1,putchar),                                                         # @@(primitive (%%putchar c))@@
 prim(1,exit),                                                            # @@(primitive (%%exit a))@@
 # )@@
]

def get_code():
 x=get_byte()-35
 return 57 if x<0 else x

def get_int(n):
 x=get_code()
 n*=46
 return n+x if x<46 else get_int(n+x-46)

list_tail=lambda lst,i:lst if i==0 else list_tail(lst[1],i-1)

# build the initial symbol table

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

symtbl=[[FALSE,[accum,n,3],2],symtbl,0]
symbol_ref=lambda n: list_tail(symtbl,n)[0]

# decode the RVM instructions

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
 if debug: start_step() # @@(feature debug)@@
 o=pc[1]
 i=pc[0]
 if i<1: # jump/call
  if tracing: print(("call " if is_rib(pc[2]) else "jump ") + show(o)) # @@(feature debug)@@
  o=get_opnd(o)[0]
  while 1:
   c=o[0]
   if is_rib(c):
    nargs=pop(); # @@(feature arity-check)@@
    c2=[0,o[1],0] # @@(feature flat-closure)@@
    c2=[0,o,0] # @@(feature (not flat-closure))@@
    s2=c2
    nparams=c[0]>>1
    # @@(feature arity-check 
    if nparams > nargs if c[0]&1 else nparams != nargs:
     print("*** Unexpected number of arguments nargs:", nargs, "nparams", nparams, "variadics:", c[0]&1);
     exit(1)
    # )@@
    # @@(feature rest-param (use arity-check)
    nargs-=nparams
    if c[0]&1: 
     rest=NIL
     while nargs:
      rest=[pop(), rest, 0]
      nargs-=1
      
     s2=[rest,s2,0]
    # )@@

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
    pop(); # @@(feature (and arity-check (not prim-no-arity)))@@
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
  if tracing: print("set " + show(o)) # @@(feature debug)@@
  get_opnd(o)[0]=stack[0]; stack = stack[1]
 elif i<3: # get
  if tracing: print("get " + show(o)) # @@(feature debug)@@
  push(get_opnd(o)[0])
 elif i<4: # const
  if tracing: print("const " + show(o)) # @@(feature debug)@@
  push(o)
 elif i<5: # if
  if tracing: print("if") # @@(feature debug)@@
  if pop() is not FALSE:
   pc=pc[1]
   continue
 else: # halt
  if tracing: print("halt") # @@(feature debug)@@
  break
 pc = pc[2]
