input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"; // @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92))@@


// @@(location decl)@@

// @@(feature (or debug debug-trace)
debug = true; 
// )@@
// @@(feature (and (not debug) (not debug-trace))
debug = false;
// )@@



lengthAttr = "length";

// @@(feature (and nodejs (not web))
// Implement putchar/getchar to the terminal 

fs = require("fs"); // @@(feature (or node-fs (or getchar putchar)))@@

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
    push(pos<input[lengthAttr] ? get_byte() : getchar_sync()); 
    return true; // indicate that no further waiting is necessary 
}; 

sym2str = (s) => chars2str(s[1][0]);  //debug
chars2str = (s) => (s===NIL) ? "" : (String.fromCharCode(s[0])+chars2str(s[1]));  //debug

// @@(feature (or debug debug-trace)
show_opnd = (o) => is_rib(o) ? ("sym " + sym2str(o)) : ("int " + o);  //debug
show_stack = () => {  //debug
    let s = stack;  //debug
    let r = [];  //debug
    while (!s[2]) { r[r[lengthAttr]]=s[0]; s=s[1]; }  //debug
    console.log(require("util").inspect(r, {showHidden: false, depth: 2}).replace(/\n/g, "").replace(/  /g, " "));  //debug
}  //debug
// )@@
// )@@

// @@(feature (and web (not nodejs))
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
// )@@
// VM

// @@(feature (or error-msg debug) (use scm2str)
halt = () => {
	const error_msg = new Error(scm2str(pop()));
	throw error_msg;
}
// )@@

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
// @@(feature bool2scm
bool2scm = (x) => x ? TRUE : FALSE;
// )@@
// @@(feature str2scm
str2scm = (s) => {
    let l = s.length
    let i = l
    let a = NIL
    while (i) a=[s.charCodeAt(--i),a,0];
    return [a,l,3]
}
// )@@

// @@(feature find_sym (use scm2list)
find_sym = (name, symtbl) => {
  lst = scm2list(symtbl)
  return list_tail(symtbl, lst.indexOf(name))[0]

}
// )@@

// @@(feature function2scm (use foreign call find_sym)
function2scm = (f) => {
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

// @@(feature host2scm (use list2scm str2scm bool2scm function2scm)
host2scm = (v) => {
  return ({"number":(x)=>x,"boolean":bool2scm,"string":str2scm,"object":list2scm, 'function':function2scm, 'undefined':()=>NIL}[typeof v](v))
}
// )@@

// @@(feature list2scm (use host2scm)
list2scm = (l,i=0) => (i<l.length?[host2scm(l[i]),list2scm(l,i+1),0]:NIL)
// )@@

// @@(feature scm2str
scm2str = (r) => {
    let f = (c) => (c===NIL?"":String.fromCharCode(c[0])+f(c[1]))
    return f(r[0])
}
// )@@

// @@(feature scm2bool
scm2bool = (r) => {
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

// @@(feature scm2list (use scm2host)
scm2list = (r) => {
  let elems = r[2] === 0 ? r : r[0];
  let lst = [];
  let f = (c) => {
    if (c !== NIL){
      lst.push(scm2host(c[0]))
      f(c[1])
    }
  }
  f(elems)
  return lst
}
// )@@

// @@(feature scm2function (use scm2host host2scm)
func_stack = []
scm2function = (r) => {
  let func = (...args) => {
    func_stack.push(pc)
    push(r)
    for(a in args){
      push(host2scm(a))
    }
    pc = [0,args.length,[5, 0, 0]] // call function and then halt
    run()
    pc = func_stack.pop()
    return_value = pop()
    return scm2host(return_value)
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

// @@(feature scm2symbol (use scm2str)
scm2symbol = (r) => {
  return scm2str(r[1])
}
// )@@


// @@(feature scm2host (use scm2str scm2list scm2bool scm2bool scm2function scm2symbol)
scm2host = (r) => {
  if (typeof r === "number")
    return r 
  let tag = r[2]
  return [scm2list, scm2function, scm2symbol, scm2str, scm2list, scm2bool][tag](r);
}
 // )@@


// @@(feature foreign
foreign = r => [0, r, 6] // 6 is to tag a foreign object
// )@@

// @@(feature host_call (use scm2list)
// f is a foreign object representing a function
host_call = () =>{
  args = pop()
  f = pop()[1]
  return push(host2scm(f(...scm2list(args))))
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
  prim1((x) => bool2scm(is_rib(x))),             //  @@(primitive (rib? rib) (use bool2scm))@@
  prim1((x) => x[0]),                               //  @@(primitive (field0 rib))@@
  prim1((x) => x[1]),                               //  @@(primitive (field1 rib))@@
  prim1((x) => x[2]),                               //  @@(primitive (field2 rib))@@
  prim2((y, x) => x[0]=y),                          //  @@(primitive (field0-set! rib))@@
  prim2((y, x) => x[1]=y),                          //  @@(primitive (field1-set! rib))@@
  prim2((y, x) => x[2]=y),                          //  @@(primitive (field2-set! rib))@@
  prim2((y, x) => bool2scm(x===y)),              //  @@(primitive (eqv? x y) (use bool2scm))@@
  prim2((y, x) => bool2scm(x<y)),                //  @@(primitive (< x y) (use bool2scm))@@
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
        // @@(feature arity-check
        let nargs=pop();
        // )@@
        let c = o[0];

        if (is_rib(c)) {
            let c2 = [0,o,0];
            let s2 = c2;

            // @@(feature (and debug-trace debug)
            if(debug){
                console.log("\nDEBUG " + f + " -- nargs:", nargs, " nparams:", c[0] >> 1, "variadics:", c[0] & 1);
            }
            // )@@
            
            let nparams = c[0] >> 1; 
            // @@(feature arity-check
            if (c[0] & 1 ? nparams > nargs : nparams != nargs){
                console.log("*** Unexpected number of arguments nargs:", nargs, " nparams:", nparams, "variadics:", c[0]&1);
                halt();
            }
            // )@@

            // @@(feature rest-param (use arity-check)
            nargs-=nparams;
            if (c[0]&1) {
                let rest=NIL;
                while(nargs--) 
                    rest=[pop(), rest, 0];
                s2=[rest,s2,0]
            }
            // )@@
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
        if (debug) { console.log("--- const " + (is_rib(o) ? "" : ("int " + o))); show_stack(); } //debug
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
// @@(feature (not web)
run(); 
// )@@
// @@(location end)@@
