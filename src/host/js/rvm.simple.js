// #=# VM definitions
input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y";

FALSE = [0,0,5]; TRUE = [0,0,5]; NIL = [0,0,5];

stack = 0;
push = (x) => ((stack = [x,stack,0]), true);
pop = () => { let x = stack[0]; stack = stack[1]; return x; };

pos = 0;
get_byte = () => input[pos++].charCodeAt(0);
get_code = () => { let x = get_byte()-35; return x<0 ? 57 : x; };
get_int = (n) => { let x = get_code(); n *= 46; return x<46 ? n+x : get_int(n+x-46); };

// #=# Primitives
fs = require("fs");

putchar = (c) => {
    let buffer = Buffer.alloc(1);
    buffer[0] = c;
    fs.writeSync(1, buffer, 0, 1);
    return c;
};

getchar_sync = () => {
    let buffer = Buffer.alloc(1);
    if (fs.readSync(0, buffer, 0, 1))
        return buffer[0];
    return -1;
};

getchar = () => {
    push(pos<input.length ? get_byte() : getchar_sync());
    return true; // indicate that no further waiting is necessary
};

prim1 = (f) => () => push(f(pop()));
prim2 = (f) => () => push(f(pop(),pop()));
prim3 = (f) => () => push(f(pop(),pop(),pop()));

bool2scm = (x) => x ? TRUE : FALSE;
scm2str = (r) => {
    let f = (c) => (c===NIL?"":String.fromCharCode(c[0])+f(c[1]));
    return f(r[0]);
};
is_rib = (x) => {
    if (x === undefined) console.log(stack);
    return x.length;
};

primitives = [
// @@(primitives (gen body)
  prim3((z, y, x) => [x, y, z]),                    //  @@(primitive (##rib a b c))@@
  prim1((x) => x),                                  //  @@(primitive (##id x))@@
  () => (pop(), true),                              //  @@(primitive (##arg1 x y))@@
  () => push([pop(),pop()][0]),                     //  @@(primitive (##arg2 x y))@@
  () => push([pop()[0],stack,1]),                   //  @@(primitive (##close rib))@@
  prim1((x) => bool2scm(is_rib(x))),             //  @@(primitive (##rib? rib) (use bool2scm))@@
  prim1((x) => x[0]),                               //  @@(primitive (##field0 rib))@@
  prim1((x) => x[1]),                               //  @@(primitive (##field1 rib))@@
  prim1((x) => x[2]),                               //  @@(primitive (##field2 rib))@@
  prim2((y, x) => (x[0]=y, true)),                          //  @@(primitive (##field0-set! rib))@@
  prim2((y, x) => (x[1]=y, true)),                          //  @@(primitive (##field1-set! rib))@@
  prim2((y, x) => (x[2]=y, true)),                          //  @@(primitive (##field2-set! rib))@@
  prim2((y, x) => bool2scm(x===y)),              //  @@(primitive (##eqv? x y) (use bool2scm))@@
  prim2((y, x) => bool2scm(x<y)),                //  @@(primitive (##< x y) (use bool2scm))@@
  prim2((y, x) => x+y),                             //  @@(primitive (##+ x y))@@
  prim2((y, x) => x-y),                             //  @@(primitive (##- x y))@@
  prim2((y, x) => x*y),                             //  @@(primitive (##* x y))@@
  prim2((y, x) => x/y|0),                           //  @@(primitive (##quotient x y))@@
  getchar,                                          //  @@(primitive (##getchar))@@
  prim1(putchar),                                   //  @@(primitive (##putchar c))@@
  () => pop() && halt(),//will crash with error on != 0 @@(primitive (##exit n))@@
// )@@
];
// --------------------------------------------------------------

// #=# Decode Symbol Table
symtbl = NIL;
n = get_int(0);
while (n-- > 0) symtbl=[[0,[NIL,0,3],2],symtbl,0]; // symbols with empty names

accum = NIL;
n = 0;
while (1) {
  c = get_byte();
  if (c == 44) { symtbl=[[0,[accum,n,3],2],symtbl,0]; accum = NIL; n = 0; continue; }
  if (c == 59) break;
  accum = [c,accum,0];
  n++;
}

symtbl = [[0,[accum,n,3],2],symtbl,0];

list_tail = (x,i) => i ? list_tail(x[1],i-1) : x;
symbol_ref = (n) => list_tail(symtbl,n)[0];

// #=# Decode instruction graph 
while (1) {
  x = get_code();
  n = x;
  d = 0;
  op = -1;
  while ((d=[20,30,0,10,11,4][++op])+2<n) n -= d+3;
  if (x>90)
    n = pop();
  else {
    if (!op) stack = [0,stack,0];
    n = n>=d ? (n==d ? get_int(0) : symbol_ref(get_int(n-d-1))) : op<3 ? symbol_ref(n) : n;
    if (4<op) {
      n = [[n,0,pop()],0,1];
      if (!stack) break;
      op=4;
    }
  }
  stack[0] = [op?op-1:0,n,stack[0]];
}

set_global = (x) => { symtbl[0][0] = x; symtbl = symtbl[1]; };

set_global([0,symtbl,1]); // primitive 0
set_global(FALSE);
set_global(TRUE);
set_global(NIL);

// #=# Execute RVM instructions

pc = n[0][2];

stack = [0,0,[5,0,0]]; // primordial continuation (executes halt instr.)


get_opnd = (o) => is_rib(o) ? o : list_tail(stack,o);
get_cont = () => { let s = stack; while (!s[2]) s = s[1]; return s; };


run = () => {
  while (1) {
    let o = pc[1];
    switch (pc[0]) {
    case 5: // halt
        return;
    case 0: // jump/call
        o = get_opnd(o)[0];
        while(1) {
            let c = o[0];

            if (is_rib(c)) {
                let nargs=pop();
                let c2 = [0,o,0];
                let s2 = c2;

                let nparams = c[0] >> 1;
                if (c[0] & 1 ? nparams > nargs : nparams != nargs){
                    console.log("*** Unexpected number of arguments nargs:", nargs, " nparams:", nparams, "variadics:", c[0]&1);
                    halt();
                }
                nargs-=nparams;
                if (c[0]&1) {
                    let rest=NIL;
                    while(nargs--)
                        rest=[pop(), rest, 0];
                    s2=[rest,s2,0];
                }
                while (nparams--) s2 = [pop(),s2,0];

                if (pc[2]===0) {
                    // jump
                    let k = get_cont();
                    c2[0] = k[0];
                    c2[2] = k[2];
                } else {
                    // call
                    c2[0] = stack;
                    c2[2] = pc[2];
                }
                stack = s2;
            } else {
                pop();
                o=primitives[c]();
                if (!o) return;
                if (is_rib(o)) continue;
                if (pc[2]===0) {
                    // jump
                    c = get_cont();
                    stack[1] = c[0];
                } else {
                    // call
                    c = pc;
                }
            }
            pc = c;
            break;
        }
        break;
    case 1: // set
        get_opnd(o)[0] = pop();
        break;
    case 2: // get
        push(get_opnd(o)[0]);
        break;
    case 3: // const
        push(o);
        break;
    case 4: // if
        if (pop() !== FALSE) { pc = pc[1]; continue; }
        break;
    }
      pc = pc[2];
  }
};

run(); 
