input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"; // RVM code that prints HELLO!

debug = false; //debug

lengthAttr = "length";

nodejs = ((function () { return this !== this.window; })()); //node
if (nodejs) { // in nodejs? //node

  // Implement putchar/getchar to the terminal //node

  node_fs = require("fs"); //node

  putchar = (c) => { //node
    let buffer = Buffer.alloc(1); //node
    buffer[0] = c; //node
    node_fs.writeSync(1, buffer, 0, 1); //node
    return c; //node
  }; //node

  getchar_sync = () => { //node
    let buffer = Buffer.alloc(1); //node
    if (node_fs.readSync(0, buffer, 0, 1)) //node
      return buffer[0]; //node
    return -1; //node
  }; //node

  getchar = () => { //node
    push(pos<input[lengthAttr] ? get_byte() : getchar_sync()); //node
    return true; // indicate that no further waiting is necessary //node
  }; //node

  sym2str = (s) => chars2str(s[1][0]); //node //debug
  chars2str = (s) => (s===NIL) ? "" : (String.fromCharCode(s[0])+chars2str(s[1])); //node //debug
  show_opnd = (o) => is_rib(o) ? "sym " + sym2str(o) : "int " + o; //node //debug
  show_stack = () => { //node //debug
    let s = stack; //node //debug
    let r = []; //node //debug
    while (!s[2]) { r[r[lengthAttr]]=s[0]; s=s[1]; } //node //debug
    console.log(require("util").inspect(r, {showHidden: false, depth: 2})); //node //debug
  } //node //debug

} else { // in web browser //node

  // Implement a simple console as a textarea in the web page

  domdoc = document;
  selstart = 0;
  addEventListenerAttr = "addEventListener";
  selectionStartAttr = "selectionStart";

  domdoc[addEventListenerAttr]("DOMContentLoaded", () => {
    dombody = domdoc.body;
    txtarea = dombody.appendChild(domdoc.createElement("textarea"));
    txtarea.style = "width:100%;height:50vh;";
    dombody[addEventListenerAttr]("keypress", (e) => {
      let x = txtarea[selectionStartAttr];
      if (x<selstart) selstart=x;
      if (e.keyCode==13) {
        e.preventDefault();
        input += txtarea.value.slice(selstart,x)+"\n";
        putchar(10);
        run(); // wake up VM
      }
    });
    run();
  });

  putchar = (c) => (selstart=txtarea[selectionStartAttr]=(txtarea.value += String.fromCharCode(c))[lengthAttr], c);

  getchar = () => pos<input[lengthAttr] && push(get_byte());
} //node

// VM

// build the symbol table

pos = 0;
get_byte = () => input[pos++].charCodeAt(0);
get_code = () => { let x = get_byte()-35; return x<0 ? 57 : x; };
get_int = (n) => { let x = get_code(); n *= 46; return x<46 ? n+x : get_int(n+x-46); };

pop = () => { let x = stack[0]; stack = stack[1]; return x; };

FALSE = [0,0,5]; TRUE = [0,0,5]; NIL = [0,0,5];

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

symbol_ref = (n) => list_tail(symtbl,n)[0];
list_tail = (x,i) => i ? list_tail(x[1],i-1) : x;

// decode the instruction graph

stack = 0;

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

// RVM core

pc = n[0][2];
stack = [0,0,[5,0,0]]; // primordial continuation (executes halt instr.)

push = (x) => ((stack = [x,stack,0]), true);
to_bool = (x) => x ? TRUE : FALSE;
is_rib = (x) => x[lengthAttr];

get_opnd = (o) => is_rib(o) ? o : list_tail(stack,o);
get_cont = () => { let s = stack; while (!s[2]) s = s[1]; return s; };

prim1 = (f) => () => push(f(pop()));
prim2 = (f) => () => push(f(pop(),pop()));
prim3 = (f) => () => push(f(pop(),pop(),pop()));

primitives = [
  prim3((z, y, x) => [x, y, z]),
  prim1((x) => x),
  () => (pop(), true),
  () => { let y = pop(); pop(); return push(y); },
  () => push([pop()[0],stack,1]),
  prim1((x) => to_bool(is_rib(x))),
  prim1((x) => x[0]),
  prim1((x) => x[1]),
  prim1((x) => x[2]),
  prim2((y, x) => x[0]=y),
  prim2((y, x) => x[1]=y),
  prim2((y, x) => x[2]=y),
  prim2((y, x) => to_bool(x===y)),
  prim2((y, x) => to_bool(x<y)),
  prim2((y, x) => x+y),
  prim2((y, x) => x-y),
  prim2((y, x) => x*y),
  prim2((y, x) => x/y|0),
  getchar,
  prim1(putchar),
  () => pop() && halt() // will crash with error on != 0
];

run = () => {
  while (1) {
    let o = pc[1];
    switch (pc[0]) {
    case 5: // halt
        return;
    case 0: // jump/call
        if (debug) { console.log((pc[2]===0 ? "--- jump " : "--- call ") + show_opnd(o)); show_stack(); } //debug
        o = get_opnd(o)[0];
        let c = o[0];
        if (is_rib(c)) {
            let c2 = [0,o,0];
            let s2 = c2;
            let nargs = c[0];
            while (nargs--) s2 = [pop(),s2,0];
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
            if (!primitives[c]()) return;
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
    case 1: // set
        if (debug) { console.log("--- set " + show_opnd(o)); show_stack(); } //debug
        get_opnd(o)[0] = pop();
        break;
    case 2: // get
        if (debug) { console.log("--- get " + show_opnd(o)); show_stack(); } //debug
        push(get_opnd(o)[0]);
        break;
    case 3: // const
        if (debug) { console.log("--- const " + o); show_stack(); } //debug
        push(o);
        break;
    case 4: // if
        if (debug) { console.log("--- if"); show_stack(); } //debug
        if (pop() !== FALSE) { pc = pc[1]; continue; }
        break;
    }
      pc = pc[2];
  }
};

if (nodejs) run(); //node
