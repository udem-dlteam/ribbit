import "dart:io";
// @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
String input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"; // RVM code that prints HELLO!
// )@@

int pos = -1;

int getByte() {
    pos+=1;
    return input[pos].codeUnitAt(0);
}

int getCode() {
    int byte_value = getByte() - 35;
    return (byte_value < 0) ? 57 : byte_value;
}

int getInt(int accumulator) {
    var next_byte = getCode();
    accumulator *= 46;

    return (next_byte < 46) ? (accumulator + next_byte)
        : getInt(accumulator + next_byte - 46);
}


class Rib {
    dynamic car;
    dynamic cdr;
    dynamic tag;
    factory Rib.create(dynamic car, dynamic cdr, dynamic tag) {
        return Rib._internal(car, cdr, tag);
    }
    Rib._internal(this.car, this.cdr, this.tag);
}

String printRibbit(rib){
    if (rib is Rib){
        var car = printRibbit(rib.car);
        var cdr = printRibbit(rib.cdr);
        var tag = printRibbit(rib.tag);
        return "[$car, $cdr, $tag]";
    }
    return rib.toString();
}

Rib FALSE = Rib.create(0, 0, 5);
Rib TRUE = Rib.create(0, 0, 5);
Rib NIL = Rib.create(0, 0, 5);

dynamic stack = NIL;


void push(var value){
    Rib newRib = Rib.create(value, stack, 0);
    stack = newRib;
}


dynamic pop(){
    Object x = stack.car;
    stack = stack.cdr;
    return x;
}

int putChar(c){
    stdout.write(String.fromCharCode(c));
    return c;
}

void getChar() {
    int byte = stdin.readByteSync();
    push(byte);
}


Function prim1 (f) => () => push(f(pop()));
Function prim2 (f) => () => push(f(pop(), pop()));
Function prim3 (f) => () => push(f(pop(), pop(), pop()));
dynamic bool2scm (x) => x ? TRUE : FALSE;
dynamic isRib(x) => (x is Rib);

dynamic field0Set(y, x){
    x.car = y;
    return y;
}


dynamic field1Set(y, x){
    x.cdr = y;
    return y;
}

dynamic field2Set(y, x){
    x.tag = y;
    return y;
}

List<dynamic> primitives = [
    prim3((z,y,x) => Rib.create(x, y, z)),
    prim1((x) => x),
        () => [pop(), 0][1],
        () => push([pop(), pop()][0]),
        () => push(Rib.create(pop().car, stack, 1)),
    prim1((x) => bool2scm(isRib(x))),
    prim1((rib) => rib.car),
    prim1((rib) => rib.cdr),
    prim1((rib) => rib.tag),
    prim2(field0Set),
    prim2(field1Set),
    prim2(field2Set),
    prim2((y, x) => bool2scm((isRib(x) || isRib(y)) ? identical(x, y) : x == y)),
    prim2((y,x) => bool2scm(x < y)),
    prim2((y,x) => x + y),
    prim2((y,x) => x - y),
    prim2((y,x) => x * y),
    prim2((y,x) => x ~/ y),
    getChar,
    prim1(putChar),
    prim1(exit)
];


Rib list_tail(Rib lst, int i){
    while (i > 0){
        lst = lst.cdr;
        i--;
    }
    return lst;
}

dynamic symtbl = Rib.create(0, 0, 5);


var n = getInt(0);
void decodeSymbolTable() {
    for (int i = 0; i < n; i++) {
        symtbl = Rib.create(
            Rib.create(FALSE,
                Rib.create(NIL,
                    0,
                    3),
                2),
            symtbl,
            0);
    }
    var symbol_name = NIL;
    int symbol_name_len = 0;


    while (true){
        var c = getByte();

        if(c == ','.codeUnitAt(0)) {
            var symbol_to_append = Rib.create(FALSE,
                Rib.create(symbol_name,
                    symbol_name_len,
                    3),
                2);

            symtbl = Rib.create(symbol_to_append, symtbl, 0);
            symbol_name = NIL;
            symbol_name_len = 0;
        }
        else {
            if(c == ';'.codeUnitAt(0)){
                var symbol_to_append = Rib.create(FALSE,
                    Rib.create(symbol_name,
                        symbol_name_len,
                        3),
                    2);

                symtbl = Rib.create(symbol_to_append, symtbl, 0);
                break;
            }
            symbol_name = Rib.create(c, symbol_name, 0);
            symbol_name_len++;
        }
    }

}


Rib symbol_ref(int k){
    return list_tail(symtbl, k).car;
}


dynamic pc;
class UnionType {
    dynamic value;

    UnionType(this.value);

    bool get isInt => value is int;
    bool get isRib => value is Rib;

    int get asInt {
        if (isInt) return value as int;
        throw Exception("Value is not an int");
    }

    Rib get asRib {
        if (isRib) return value as Rib;
        throw Exception("Value is not a Rib");
    }
}

void decodeInstructionGraph() {
    const List<int> shortEncodings = [20, 30, 0, 10, 11, 4];

    bool continue_ = true;
    UnionType? m;

    while (continue_) {
        int x = getCode();
        int n = x;
        int op = 0;
        int d = shortEncodings[op];

        while (d + 2 < n) {
            n -= d + 3;
            op += 1;
            d = shortEncodings[op];
        }

        if (x > 90) {
            m = UnionType(pop());
        }
        else {
            if (op == 0) {
                push(0);
                op += 1;
            }

            if (n == d) {
                m = UnionType(getInt(0));
            } else if (n > d) {
                m = UnionType(symbol_ref(getInt(n - d - 1)));
            } else if (op < 3) {
                m = UnionType(symbol_ref(n));
            } else {
                m = UnionType(n);
            }

            if (op > 4) {
                Rib rib1 = Rib.create(m.value, 0, pop());
                Rib rib2 = Rib.create(rib1, NIL, 1);
                m = UnionType(rib2);

                if (stack == NIL || (stack is int && stack == 0)) {
                    continue_ = false;
                } else {
                    op = 4;
                }
            }
        }

        if (continue_) {
            if (stack is Rib) {
                stack.car = Rib.create(op - 1, m.value, stack.car);
            } else {
                throw Exception("Expected stack to be a Rib");
            }
        }
    }

    pc = (m?.value as Rib).car.tag;
}



dynamic getOpnd(dynamic o) {
    return isRib(o) ? o : list_tail(stack, o as int);
}

dynamic getCont() {
    var s = stack;
    while (s.tag == 0) {
        s = s.cdr;
    }
    return s;
}

void setGlobal(dynamic val) {
    symtbl.car.car = val;
    symtbl = symtbl.cdr;
}



void run() {
    setGlobal(Rib.create(0, symtbl, 1));
    setGlobal(FALSE);
    setGlobal(TRUE);
    setGlobal(NIL);

    stack = Rib.create(0, 0, Rib.create(5, 0, 0));

    dynamic i, o, c;
    dynamic nparams, s2, c2, k;

    while (true) {
        o = pc.cdr;
        i = pc.car;

        if (i < 1) {
            o = getOpnd(o).car;
            while (true) {
                c = o.car;
                if (isRib(c)) {
                    var nargs = pop(); // @@(feature arity-check)@@
                    c2 = Rib.create(0, o, 0); // @@(feature (not flat-closure))@@
                    var s2 = c2;
                    nparams = ((c as Rib).car as int) >> 1;
                    // @@(feature arity-check
                    if ((c.car as int) % 2 == 1 ? (nparams > nargs) : (nparams != nargs)) {
                        print('*** Unexpected number of arguments nargs: $nargs, nparams: $nparams, variadics: ${(c.car as int) % 2}');
                        exit(1);
                    }
                    // )@@
                    // @@(feature rest-param (use arity-check)
                    nargs -= nparams;
                    if ((c.car as int) % 2 == 1){
                        var rest = NIL;
                        while ((nargs as int) != 0){
                            rest=Rib.create(pop(), rest, 0);
                            nargs-=1;
                        }
                        s2 = Rib.create(rest, s2, 0);
                    }
                    // )@@

                    while (nparams != 0) {
                        s2 = Rib.create(pop(), s2, 0);
                        nparams--;
                    }
                    if (pc.tag != 0){
                        c2.car = stack;
                        c2.tag = pc.tag;
                    }
                    else{
                        k = getCont();
                        c2.car = k.car;
                        c2.tag = k.tag;
                    }
                    stack = s2;
                } else {
                    pop(); // @@(feature (and arity-check (not prim-no-arity)))@@
                    o = primitives[c]();
                    if (isRib(o)) {
                        continue;
                    }
                    if (pc.tag != 0){
                        c = pc;
                    }
                    else{
                        c = getCont();
                        stack.cdr = c.car;
                    }
                }
                pc = c;
                break;
            }
        } else if (i < 2) {
            getOpnd(o).car = stack.car;
            stack = stack.cdr;
        } else if (i < 3) {
            push(getOpnd(o).car);
        } else if (i < 4) {
            push(o);
        } else if (i < 5) {
            if (pop() != FALSE) {
                pc = pc.cdr;
                continue;
            }
        } else {
            return;
        }
        pc = pc.tag;
    }
}


void main() {
    decodeSymbolTable();
    decodeInstructionGraph();
    run();
}