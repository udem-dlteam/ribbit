// #=# VM definitions
input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y";

// Make sure to compare these value with their memory address as normal in some languages, 
//  equality compares the value of the object, not the memory address.
FALSE = [0,0,5];
TRUE = [0,0,5];
NIL = [0,0,5];

stack = 0;
const push = (x) => {
  stack = [x, stack, 0];
  return true;
}
const pop = () => {
  let x = stack[0];
  stack = stack[1];
  return x;
}

let pos = 0;

const get_byte = () => {
  const byte = input[pos].charCodeAt(0);
  pos++;
  return byte;
};

const get_code = () => {
  let code = get_byte() - 35;
  if (code < 0) {
    code = 57;
  }
  return code;
};

const get_int = (n) => {
  let x = get_code();
  n *= 46;
  if (x < 46) {
    return n + x;
  } else {
    return get_int(n + x - 46);
  }
};

// #=# Primitives
fs = require("fs"); // @@(feature (or js/node/fs ##getchar ##putchar))@@

const putchar = (c) => {
  let buffer = Buffer.alloc(1);
  buffer[0] = c;
  fs.writeSync(1, buffer, 0, 1);
  return c;
};

const getchar_sync = () => {
  let buffer = Buffer.alloc(1);
  if (fs.readSync(0, buffer, 0, 1))
    return buffer[0];
  return -1;
};

const getchar = () => {
  if (pos < input.length) {
    push(get_byte());
  }
  else {
    push(getchar_sync());
  }
  return true;
};

prim1 = (f) => () => push(f(pop()));
prim2 = (f) => () => push(f(pop(),pop()));
prim3 = (f) => () => push(f(pop(),pop(),pop()));

bool2scm = (x) => x ? TRUE : FALSE;
scm2str = (r) => {
  let buildString = (c) => {
    if (c == NIL) {
      return "";
    } else {
      return String.fromCharCode(c[0]) + buildString(c[1]);
    }
  }
  return buildString(r[0]);
};

is_rib = (x) => {
  return typeof x == "object";
};

set_field = (rib, index, value) => {
  rib[index] = value;
  return true;
}

arg2 = () => {
  let x = pop();
  let y = pop();
  return push(x);
}

primitives = [
// @@(primitives (gen body)
  prim3((z, y, x) => [x, y, z]),                    //  @@(primitive (##rib a b c))@@
  prim1((x) => x),                                  //  @@(primitive (##id x))@@
  () => {pop(); return true;},                      //  @@(primitive (##arg1 x y))@@
  arg2,                                             //  @@(primitive (##arg2 x y))@@
  () => push([pop()[0],stack,1]),                   //  @@(primitive (##close rib))@@
  prim1((x) => bool2scm(is_rib(x))),                //  @@(primitive (##rib? rib) (use bool2scm))@@
  prim1((x) => x[0]),                               //  @@(primitive (##field0 rib))@@
  prim1((x) => x[1]),                               //  @@(primitive (##field1 rib))@@
  prim1((x) => x[2]),                               //  @@(primitive (##field2 rib))@@
  prim2((y, x) => set_field(x, 0, y)),              //  @@(primitive (##field0-set! rib))@@
  prim2((y, x) => set_field(x, 1, y)),              //  @@(primitive (##field1-set! rib))@@
  prim2((y, x) => set_field(x, 2, y)),              //  @@(primitive (##field2-set! rib))@@
  prim2((y, x) => bool2scm(x===y)),                 //  @@(primitive (##eqv? x y) (use bool2scm))@@
  prim2((y, x) => bool2scm(x<y)),                   //  @@(primitive (##< x y) (use bool2scm))@@
  prim2((y, x) => x+y),                             //  @@(primitive (##+ x y))@@
  prim2((y, x) => x-y),                             //  @@(primitive (##- x y))@@
  prim2((y, x) => x*y),                             //  @@(primitive (##* x y))@@
  prim2((y, x) => Math.trunc(x/y)),                 //  @@(primitive (##quotient x y))@@
  getchar,                                          //  @@(primitive (##getchar))@@
  prim1(putchar),                                   //  @@(primitive (##putchar c))@@
  () => pop() && halt(),//will crash with error on != 0 @@(primitive (##exit n))@@
// )@@
];
// --------------------------------------------------------------

// #=# Decode Symbol Table
symtbl = NIL;
n = get_int(0);
while (n-- > 0) symtbl=[[0,[NIL,0,3],2],symtbl,0]; 

accum = NIL;
n = 0;
while (1) {
  c = get_byte();
  if (c == 44) { 
    symtbl=[[0,[accum,n,3],2],symtbl,0];
    accum = NIL;
    n = 0; 
    continue; 
  }
  if (c == 59) break;
  accum = [c,accum,0];
  n++;
}


function list_tail(x, i) {
  if (i == 0) {
    return x;
  } else {
    return list_tail(x[1], i - 1);
  }
}

symtbl = [[0,[accum,n,3],2],symtbl,0];
symbol_ref = (n) => list_tail(symtbl,n)[0];

// #=# Decode instruction graph 
LENGTH_ARRAY=[20, 30, 0, 10, 11, 4]
while (1) {
  token = get_code();       // Current "token"
  arg = token;              // Will become the argument of the instruction
  op = 0;                   // Will become the opcode of the instruction
  range = LENGTH_ARRAY[op]; // represents the range of allowed values for the current opcode
  while (range + 2 < arg) {
    arg -= range + 3;
    range = LENGTH_ARRAY[++op]
  }
  if (token > 90) { 
    arg = pop(); // Argument of an if instruction is the true branch
  } else {
    // If the opcode is 0, we are creating a closure, so we need to push a new continuation
    if (!op) {
      stack = [0, stack, 0];
    }
    
    // Change the argument depending on the opcode and the range allowed for the 
    //  argument
    if (arg >= range) {
      if (arg == range) {
        //  If the argument is in the "long" range, we need to get the next 
        arg = get_int(0);
      } else {
        arg = symbol_ref(get_int(arg - range - 1));
      }
    } else {
      if (op < 3) {
        // Some instructions get their argument from the symbol table, such as get and set.
        arg = symbol_ref(arg);
      }
    }

    // The opcode 5 is used to create a closure, with arg being the number 
    // of arguments in the closure.
    if (op > 4) {
      arg = [[arg, 0, pop()], 0, 1];
      if (!stack) {
        stack = arg
        break;
      }
      op = 4;
    }
  }

  if (op != 0){
    stack[0] = [op - 1, arg, stack[0]];
  } else {
    stack[0] = [0, arg, stack[0]];
  }
}

set_global = (x) => {
  symtbl[0][0] = x;
  symtbl = symtbl[1];
};

set_global([0,symtbl,1]); // primitive 0
set_global(FALSE);
set_global(TRUE);
set_global(NIL);

// #=# Execute RVM instructions

// The last closure created (the program to run) is on top of the stack
pc = stack[0][2];

stack = [0,0,[5,0,0]]; // primordial continuation (executes halt instr.)

get_opnd = (o) =>{
  if(is_rib(o)){
    return o;
  } else {
    return list_tail(stack,o)
  }
};
get_cont = () => { 
  let s = stack;
  while (!s[2]) s = s[1]; 
  return s; 
};


run = () => {
  // Interpreter loop
  while (1) {
    let o = pc[1];
    switch (pc[0]) {
      case 5: // halt
        return;
      case 0: // jump/call
        o = get_opnd(o)[0];
        while (1) {
          let c = o[0];

          if (is_rib(c)) {
            let nargs = pop();
            let c2 = [0, o, 0];
            let s2 = c2;

            let nparams = c[0] >> 1;
            if ((c[0] & 1) && nparams > nargs || !(c[0] & 1) && nparams != nargs) {
              console.log("*** Unexpected number of arguments nargs:", nargs, " nparams:", nparams, "variadics:", c[0] & 1);
              halt();
            }
            nargs -= nparams;
            if (c[0] & 1) {
              let rest = NIL;
              while (nargs--){
                rest = [pop(), rest, 0];
              }
              s2 = [rest, s2, 0];
            }
            while (nparams--){
             s2 = [pop(), s2, 0];
            }

            if (pc[2] === 0) {
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
            o = primitives[c]();
            if (!o) return;
            if (is_rib(o)) continue;
            if (pc[2] === 0) {
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
        if (pop() !== FALSE) { 
          pc = pc[1]; 
          continue; 
        }
        break;
    }
    pc = pc[2];
  }
};

run(); 
