import sys
putchar=lambda c:sys.stdout.write(chr(c))
def getchar():
 c=sys.stdin.read(1)
 push(ord(c) if len(c) else -1)

debug = False #debug#

sym2str = lambda s:chars2str(s[1][0]) #debug#
chars2str = lambda s:"" if s is NIL else chr(s[0])+chars2str(s[1]) #debug#
show_opnd = lambda o:"int "+str(o) if is_num(o) else "sym "+sym2str(o) #debug#

def show_stack(): #debug#
 s = stack #debug#
 r = [] #debug#
 while not s[2]: r.append(s[0]); s=s[1] #debug#
 print(r) #debug#

pos=-1
def get_byte():
 global pos
 pos+=1
 return ord(input[pos])

# VM

FALSE=[0,0,4]
TRUE=[0,0,5]
NIL=[0,0,6]

boolean=lambda x:TRUE if x else FALSE
is_num=lambda x:type(x) is int

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
def close():x = pop();push([x[0],stack,1])
def f0s(y,x):x[0]=y;return y
def f1s(y,x):x[1]=y;return y
def f2s(y,x):x[2]=y;return y

primitives = [
 prim3(lambda z,y,x:[x,y,z]),
 prim1(lambda x:x),
 pop,
 arg2,
 close,
 prim1(lambda x:boolean(not is_num(x))),
 prim1(lambda x:x[0]),
 prim1(lambda x:x[1]),
 prim1(lambda x:x[2]),
 prim2(f0s),
 prim2(f1s),
 prim2(f2s),
 prim2(lambda y,x:boolean(x==y if is_num(x) and is_num(y) else x is y)),
 prim2(lambda y,x:boolean(x<y)),
 prim2(lambda y,x:x+y),
 prim2(lambda y,x:x-y),
 prim2(lambda y,x:x*y),
 prim2(lambda y,x:x//y),
 getchar,
 prim1(putchar)
]

def get_code():
 x=get_byte()-35
 return 57 if x<0 else x
 
def get_int(n):
 x=get_code()
 n*=46
 return n+x if x<46 else get_int(n+x-46)

list_tail=lambda lst,i:lst if i==0 else list_tail(lst[1],i-1)
list_ref=lambda lst,i:list_tail(lst,i)[0]

# build the initial symbol table

symbol_table=NIL
n=get_int(0)
while n>0:
 n-=1
 symbol_table=[[0,[NIL,0,2],3],symbol_table,0]

accum=NIL
while 1:
 c=get_byte()
 if c==44:
  symbol_table=[[0,[accum,0,2],3],symbol_table,0]; accum=NIL
 else:
  if c==59: break
  accum=[c,accum,0]

symbol_table=[[0,[accum,0,2],3],symbol_table,0]
symbol_ref=lambda n: list_ref(symbol_table,n)

# decode the uVM instructions

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
  if op==0:stack=[0,stack,0]
  n = get_int(0)if n==d else symbol_ref(get_int(n-d-1))if n>=d else symbol_ref(n)if op<3 else n
  if op>4:
   n=[[n,0,pop()],NIL,1]
   if not stack:break
   op=4
 stack[0]=[op,n,stack[0]]

pc = n[0][2]

get_opnd=lambda o:(list_tail(stack,o)if is_num(o) else o)[0]

def get_cont():
 s=stack
 while not s[2]:s=s[1]
 return s

def set_global(val):
 global symbol_table
 symbol_table[0][0]=val
 symbol_table=symbol_table[1]

set_global(symbol_table)
set_global(FALSE)
set_global(TRUE)
set_global(NIL)
set_global([0,NIL,1]) # primitive 0

stack=[0,0,[6,0,0]] # primordial continuation (executes halt instr.)

while 1:
 o=pc[1]
 i=pc[0]
 if i<2: # jump/call
  if debug: print(("--- jump " if i==0 else "--- call ") + show_opnd(o)); show_stack() #debug#
  o=get_opnd(o)
  c=o[0]
  if is_num(c):
   primitives[c]()
   if i: # call
    c=pc
   else: # jump
    c=get_cont()
    stack[1]=c[0]
  else:
   c2=[0,o,0]
   s2=c2
   nargs=c[0]
   while nargs:s2=[pop(),s2,0];nargs-=1
   if i: # call
    c2[0]=stack
    c2[2]=pc[2]
   else: # jump
    k=get_cont()
    c2[0]=k[0]
    c2[2]=k[2]
   stack=s2
  pc=c[2]
 elif i<3: # set
  if debug: print("--- set " + show_opnd(o)); show_stack() #debug#
  x=pop()
  (list_tail(stack,o) if is_num(o) else o)[0]=x
  pc=pc[2]
 elif i<4: # get
  if debug: print("--- get " + show_opnd(o)); show_stack() #debug#
  push(get_opnd(o))
  pc=pc[2]
 elif i<5: # const
  if debug: print("--- const " + str(o)); show_stack() #debug#
  push(o)
  pc=pc[2]
 elif i<6: # if
  if debug: print("--- if"); show_stack() #debug#
  pc=pc[2 if pop()is FALSE else 1]
 else: # halt
  break
