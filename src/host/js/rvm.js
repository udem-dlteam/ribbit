// @@(location decl)@@
input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"; // @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" source)@@

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
// @@(feature debug
log_return = (s) => {console.log(s); return s;}
// )@@
// @@(feature bool_to_rib
bool_to_rib = (x) => x ? TRUE : FALSE;
// )@@
// @@(feature str_to_rib
str_to_rib = (s) => {
    let l = s.length
    let i = l
    let a = NIL
    while (i) a=[s.charCodeAt(--i),a,0];
    return [a,l,3]
}
// )@@

// @@(feature find_sym (use rib_to_list)
find_sym = (name, symtbl) => {
  lst = rib_to_list(symtbl)
  return list_tail(symtbl, lst.indexOf(name))[0]

}
// )@@

// @@(feature function_to_rib (use foreign call find_sym)
function_to_rib = (f) => {
  let host_call = find_sym('host-call', symtbl)
  let id = find_sym('id', symtbl)
  let arg2 = find_sym('arg2', symtbl)
  let rib = [[0, 0, 1], [NIL, 0, 3], 2]
  if (host_call == -1 || id == -1){
    console.log("ERROR : you must define host-call as a primitive to convert a function to a rib")
    return 
  }

  let code = [3, foreign(f),  // push(foreign(f))
              [2, 1, // inverse arguments
               [0, host_call,  // call host_call primitive
                [0, arg2, // discard argument on stack
                 [0, id, 0]]]]] // return 
  let i = f.length // number of args
  while(i--){
    code = [3, 0,  // push 0
             [0, rib, // call rib
              code]]
  }
  code = [f.length, 0,     // number of params
          [3, NIL, code]] // push nil

  let env = 0 // no environnement
  return [code, env, 1] // return the procedure
}
// )@@

// @@(feature any_to_rib (use list_to_rib str_to_rib bool_to_rib function_to_rib)
any_to_rib = (v) => {
  return ({"number":(x)=>x,"boolean":bool_to_rib,"string":str_to_rib,"object":list_to_rib, 'function':function_to_rib, 'undefined':()=>NIL}[typeof v](v))
}
// )@@

// @@(feature list_to_rib (use any_to_rib)
list_to_rib = (l,i=0) => (i<l.length?[any_to_rib(l[i]),list_to_rib(l,i+1),0]:NIL)
// )@@

// @@(feature rib_to_str
rib_to_str = (r) => {
    let f = (c) => (c===NIL?"":String.fromCharCode(c[0])+f(c[1]))
    return f(r[0])
}
// )@@

// @@(feature rib_to_bool
rib_to_bool = (r) => {
  if (r === NIL){
    return []
  }
  if (r === FALSE){
    return false
  }
  if (r === TRUE){
    return true
  }
  console.error("Cannot convert ", r, " to bool");
}
// )@@

// @@(feature rib_to_list (use rib_to_any)
rib_to_list = (r) => {
  let elems = r[2] === 0 ? r : r[0];
  let lst = [];
  let f = (c) => {
    if (c !== NIL){
      lst.push(rib_to_any(c[0]))
      f(c[1])
    }
  }
  f(elems)
  return lst
}
// )@@

// @@(feature rib_to_function (use rib_to_any any_to_rib)
func_stack = []
rib_to_function = (r) => {
  let func = (...args) => {
    func_stack.push(pc)
    push(r)
    for(a in args){
      push(any_to_rib(a))
    }
    pc = [0,args.length,[5, 0, 0]] // call function and then halt
    run()
    pc = func_stack.pop()
    return_value = pop()
    return rib_to_any(return_value)
  }
  return func
}
// )@@

// @@(feature debug-callback
debug_callback = (callback) => {
  console.log(callback())
  return true;
}
// )@@

// @@(feature rib_to_symbol (use rib_to_str)
rib_to_symbol = (r) => {
  return rib_to_str(r[1])
}
// )@@


// @@(feature rib_to_any (use rib_to_str rib_to_list rib_to_bool rib_to_bool rib_to_function rib_to_symbol)
rib_to_any = (r) => {
  if (typeof r === "number")
    return r 
  let tag = r[2]
  return [rib_to_list, rib_to_function, rib_to_symbol, rib_to_str, rib_to_list, rib_to_bool][tag](r);
}
 // )@@


// @@(feature foreign
foreign = (r) => [0, r, 6] // 6 is to tag a foreign object
// )@@

// @@(feature host_call (use rib_to_list)
// f is a foreign object representing a function
host_call = () =>{
  args = pop()
  f = pop()[1]
  return push(any_to_rib(f(...rib_to_list(args))))
} 
// )@@




is_rib = (x) => x[lengthAttr];

get_opnd = (o) => is_rib(o) ? o : list_tail(stack,o);
get_cont = () => { let s = stack; while (!s[2]) s = s[1]; return s; };

prim1 = (f) => () => push(f(pop()));
prim2 = (f) => () => push(f(pop(),pop()));
prim3 = (f) => () => push(f(pop(),pop(),pop()));

primitives = [
// @@(primitives (gen body)
  prim3((z, y, x) => [x, y, z]),                    //  @@(primitive (rib a b c))@@
  prim1((x) => x),                                  //  @@(primitive (id x))@@
  () => (pop(), true),                              //  @@(primitive (arg1 x y))@@
  () => { let y = pop(); pop(); return push(y); },  //  @@(primitive (arg2 x y))@@
  () => push([pop()[0],stack,1]),                   //  @@(primitive (close rib))@@
  prim1((x) => bool_to_rib(is_rib(x))),             //  @@(primitive (rib? rib) (use bool_to_rib))@@
  prim1((x) => x[0]),                               //  @@(primitive (field0 rib))@@
  prim1((x) => x[1]),                               //  @@(primitive (field1 rib))@@
  prim1((x) => x[2]),                               //  @@(primitive (field2 rib))@@
  prim2((y, x) => x[0]=y),                          //  @@(primitive (field0-set! rib))@@
  prim2((y, x) => x[1]=y),                          //  @@(primitive (field1-set! rib))@@
  prim2((y, x) => x[2]=y),                          //  @@(primitive (field2-set! rib))@@
  prim2((y, x) => bool_to_rib(x===y)),              //  @@(primitive (eqv? x y) (use bool_to_rib))@@
  prim2((y, x) => bool_to_rib(x<y)),                //  @@(primitive (< x y) (use bool_to_rib))@@
  prim2((y, x) => x+y),                             //  @@(primitive (+ x y))@@
  prim2((y, x) => x-y),                             //  @@(primitive (- x y))@@
  prim2((y, x) => x*y),                             //  @@(primitive (* x y))@@
  prim2((y, x) => x/y|0),                           //  @@(primitive (quotient x y))@@
  getchar,                                          //  @@(primitive (getchar))@@
  prim1(putchar),                                   //  @@(primitive (putchar c))@@
  () => pop() && halt(),//will crash with error on != 0 @@(primitive (exit n))@@
// )@@
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

// @@(location start)@@
if (nodejs) run(); //node
// @@(location end)@@
