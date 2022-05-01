input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" # RVM code that prints HELLO!

import sys

stdo=sys.stdout

putchar=lambda c:[stdo.write(chr(c)),stdo.flush(),c][2]

def getchar():
 c=sys.stdin.read(1)
 push(ord(c) if len(c) else -1)

debug = False                                                           # DEBUG
tracing = False                                                         # DEBUG
step_count = 0                                                          # DEBUG
start_tracing = 0                                                       # DEBUG
next_stamp = 0                                                          # DEBUG
                                                                        # DEBUG
def show(obj):                                                          # DEBUG
 if not is_rib(obj): return str(obj)                                    # DEBUG
 type = obj[2]                                                          # DEBUG
 if type == 4: return "#" + show(obj[0])                                # DEBUG
 result = ""                                                            # DEBUG
 if type == 0:                                                          # DEBUG
  n = 1                                                                 # DEBUG
  result += "(" + show(obj[0])                                          # DEBUG
  obj = obj[1]                                                          # DEBUG
  while is_rib(obj) and obj[2] == 0:                                    # DEBUG
   if n > 4:                                                            # DEBUG
    result += " ..."                                                    # DEBUG
    obj = NIL                                                           # DEBUG
    break                                                               # DEBUG
   result += " " + show(obj[0])                                         # DEBUG
   obj = obj[1]                                                         # DEBUG
   n += 1                                                               # DEBUG
  if obj is not NIL:                                                    # DEBUG
   result += " . " + show(obj)                                          # DEBUG
  result += ")"                                                         # DEBUG
 elif type == 1:                                                        # DEBUG
  if is_rib(obj[0]):                                                    # DEBUG
   result += "#<procedure nparams=" + str(obj[0][0]) + ">"              # DEBUG
  else:                                                                 # DEBUG
   result += "#<primitive " + str(obj[0]) + ">"                         # DEBUG
 elif type == 2:                                                        # DEBUG
  obj = obj[1]                                                          # DEBUG
  if is_rib(obj) and obj[2] == 3 and obj[1] > 0:                        # DEBUG
   obj = obj[0]                                                         # DEBUG
   while is_rib(obj) and obj[2] == 0:                                   # DEBUG
    result += chr(obj[0])                                               # DEBUG
    obj = obj[1]                                                        # DEBUG
  else:                                                                 # DEBUG
   result += "#<symbol " + show(obj) + ">"                              # DEBUG
 elif type == 3:                                                        # DEBUG
  result += "\""                                                        # DEBUG
  obj = obj[0]                                                          # DEBUG
  while is_rib(obj) and obj[2] == 0:                                    # DEBUG
   c = chr(obj[0])                                                      # DEBUG
   if c == "\n": c = "n"; result += "\\"                                # DEBUG
   elif c == "\r": c = "r"; result += "\\"                              # DEBUG
   elif c == "\t": c = "t"; result += "\\"                              # DEBUG
   elif c == "\\" or c == "\"": result += "\\"                          # DEBUG
   result += c                                                          # DEBUG
   obj = obj[1]                                                         # DEBUG
  result += "\""                                                        # DEBUG
 elif type == 5:                                                        # DEBUG
  if obj is FALSE:                                                      # DEBUG
   result += "#f"                                                       # DEBUG
  elif obj is TRUE:                                                     # DEBUG
   result += "#t"                                                       # DEBUG
  elif obj is NIL:                                                      # DEBUG
   result += "()"                                                       # DEBUG
  else:                                                                 # DEBUG
   result += "["+show(obj[0])+","+show(obj[1])+","+show(obj[2])+"]"     # DEBUG
 else:                                                                  # DEBUG
  result += "["+show(obj[0])+","+show(obj[1])+","+show(obj[2])+"]"      # DEBUG
 return result                                                          # DEBUG
                                                                        # DEBUG
def start_step():                                                       # DEBUG
 global step_count, tracing, next_stamp                                 # DEBUG
 step_count += 1                                                        # DEBUG
 if step_count >= start_tracing: tracing = True                         # DEBUG
 if not tracing:                                                        # DEBUG
  if step_count >= next_stamp:                                          # DEBUG
   next_stamp = int(next_stamp * 1.01 + 1)                              # DEBUG
   print("@" + str(step_count))                                         # DEBUG
  return                                                                # DEBUG
 s = stack                                                              # DEBUG
 result = "@" + str(step_count) + " STACK = ("                          # DEBUG
 sep = ""                                                               # DEBUG
 while s[2]==0: result += sep + show(s[0]); sep = " "; s=s[1]           # DEBUG
 result += ")"                                                          # DEBUG
 print(result)                                                          # DEBUG

pos=-1
def get_byte():
 global pos
 pos+=1
 return ord(input[pos])

# VM

FALSE=[0,0,5]
TRUE=[0,0,5]
NIL=[0,0,5]

to_bool=lambda x:TRUE if x else FALSE
is_rib=lambda x:type(x) is list

stack=0

def push(x):
 global stack
 stack=[x,stack,0]

def pop():
 global stack
 x=stack[0]
 stack=stack[1]
 return x

prim1=lambda f:lambda:push(f(pop()))
prim2=lambda f:lambda:push(f(pop(),pop()))
prim3=lambda f:lambda:push(f(pop(),pop(),pop()))

def arg2():x = pop();pop();push(x)
def close():push([pop()[0],stack,1])
def f0s(y,x):x[0]=y;return y
def f1s(y,x):x[1]=y;return y
def f2s(y,x):x[2]=y;return y

primitives = [
 prim3(lambda z,y,x:[x,y,z]),
 prim1(lambda x:x),
 pop,
 arg2,
 close,
 prim1(lambda x:to_bool(is_rib(x))),
 prim1(lambda x:x[0]),
 prim1(lambda x:x[1]),
 prim1(lambda x:x[2]),
 prim2(f0s),
 prim2(f1s),
 prim2(f2s),
 prim2(lambda y,x:to_bool(x is y if is_rib(x) or is_rib(y) else x==y)),
 prim2(lambda y,x:to_bool(x<y)),
 prim2(lambda y,x:x+y),
 prim2(lambda y,x:x-y),
 prim2(lambda y,x:x*y),
 prim2(lambda y,x:int(x/y)),
 getchar,
 prim1(putchar),
 prim1(exit)
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
 if debug: start_step() # DEBUG
 o=pc[1]
 i=pc[0]
 if i<1: # jump/call
  if tracing: print(("call " if is_rib(pc[2]) else "jump ") + show(o)) # DEBUG
  o=get_opnd(o)[0]
  c=o[0]
  if is_rib(c):
   c2=[0,o,0]
   s2=c2
   nargs=c[0]
   while nargs:s2=[pop(),s2,0];nargs-=1
   if is_rib(pc[2]): # call
    c2[0]=stack
    c2[2]=pc[2]
   else: # jump
    k=get_cont()
    c2[0]=k[0]
    c2[2]=k[2]
   stack=s2
  else:
   primitives[c]()
   if is_rib(pc[2]): # call
    c=pc
   else: # jump
    c=get_cont()
    stack[1]=c[0]
  pc=c[2]
 elif i<2: # set
  if tracing: print("set " + show(o)) # DEBUG
  get_opnd(o)[0]=stack[0]; stack = stack[1]
  pc=pc[2]
 elif i<3: # get
  if tracing: print("get " + show(o)) # DEBUG
  push(get_opnd(o)[0])
  pc=pc[2]
 elif i<4: # const
  if tracing: print("const " + show(o)) # DEBUG
  push(o)
  pc=pc[2]
 elif i<5: # if
  if tracing: print("if") # DEBUG
  pc=pc[2 if pop()is FALSE else 1]
 else: # halt
  if tracing: print("halt") # DEBUG
  break
