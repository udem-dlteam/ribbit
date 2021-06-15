// noinspection UnnecessaryLocalVariableJS

/*
uVM implementation in Javascript
Based-off the python implementation. Clumps are lists.
 */
const fs = require('fs')
const os = require('os')

const from_fixnum = x => {
    return x >> 1
}

const to_fixnum = x => {
    if (typeof (x) === 'boolean') {
        x = x ? 1 : 0
    }

    return (x << 1) | 1
}

const TAG_PAIR = to_fixnum(0)
const TAG_PROC = to_fixnum(1)
const TAG_STR = to_fixnum(2)
const TAG_SYM = to_fixnum(3)
const TAG_TRUE = to_fixnum(4)
const TAG_FALSE = to_fixnum(5)
const TAG_NUL = to_fixnum(6)

const NIL = to_fixnum(0)

const CAR_I = 0
const CDR_I = 1
const TAG_I = 2
const CLUMP_SIZE = 3

const NULL = [0, 0, TAG_NUL]
const TRUE = [0, 0, TAG_TRUE]
const FALSE = [0, 0, TAG_FALSE]

/** Variables for the VM **/
let stack = NIL
let st = NIL
let pc = NIL

function _obj_to_str(obj) {
    if (typeof (obj) === 'object') {
        return "CLMP"
    } else if (typeof (obj) === 'number') {
        return from_fixnum(obj).toString()
    } else {
        return "err"
    }
}


function _print_stack() {
    let scout = stack

    while (NIL !== scout) {
        console.log('(' + scout.map(_obj_to_str).join(",") + ')');
        scout = scout[CDR_I]
    }
}

function push_clump() {
    stack = [NIL, stack, TAG_PAIR]
}

function _push_and_set(x) {
    push_clump()
    stack[CAR_I] = to_fixnum(x)
}

function pop_clump() {
    stack = stack[CDR_I]
}

function _field(x) {
    let field = stack[CAR_I][x]
    push_clump()
    stack[CAR_I] = field
}

function field0() {
    _field(0)
}

function field1() {
    _field(1)
}

function field2() {
    _field(2)
}

function _field_set(x) {
    let val = stack[CAR_I]
    pop_clump()
    stack[CAR_I][x] = val
}

function field0_set() {
    _field_set(0)
}

function field1_set() {
    _field_set(1)
}

function field2_set() {
    _field_set(2)
}

function _skip(n) {
    let scout = stack

    while (n-- >= 0) {
        scout = scout[CDR_I]
    }

    return scout
}

function _pop(n) {
    if (n < 1) {
        throw new Error("Cannot pop less than a single element")
    }

    let result = []

    while (n !== 1) {
        result.push(stack[CAR_I])
        pop_clump()
        n--
    }

    result.push(stack[CAR_I])

    return result
}

function clump() {
    stack = _pop(CLUMP_SIZE)
}

function _binop(op) {
    const args = _pop(2);
    const rValued = args.map(from_fixnum);
    const result = rValued.reduce(op);

    stack[CAR_I] = to_fixnum(result);
}

function _log_bin_op(op) {
    _binop(op)
    if (stack[CAR_I] === to_fixnum(1)) {
        stack[CAR_I] = TRUE;
    } else {
        stack[CAR_I] = FALSE;
    }
}

const lt = () => _log_bin_op((x, y) => x < y);
const eq = () => _log_bin_op((x, y) => x === y)
const add = () => _binop((x, y) => x + y)
const sub = () => _binop((x, y) => x - y)
const mul = () => _binop((x, y) => x * y)
const div = () => _binop((x, y) => x / y)

function putchar() {
    let char = String.fromCharCode(from_fixnum(stack[CAR_I]))
    process.stdout.write(char)
}

function getchar() {
    let char = process.stdin.read(1)
    push_clump()
    stack[CAR_I] = to_fixnum(char.charCodeAt(0))
}

function _env() {
    let slow = null
    let scout = stack

    while (scout[TAG_I] !== TAG_PROC) {
        slow = scout
        scout = scout[CDR_I]
    }

    return slow
}

const call = (proc_clump) => _call_or_jump(true, proc_clump)
const jump = (proc_clump) => _call_or_jump(false, proc_clump)

// (vector 'identity
// 'arg1
// 'arg2
// 'close
// 'cons
// 'clump?
// 'field0
// 'field1
// 'field2
// 'field0-set!
// 'field1-set!
// 'field2-set!
// 'eq?
// '<
// '+
// '-
// '*
// 'quotient
// 'getchar
// 'putchar))

function id() {

}

function arg1() {

}

function arg2() {

}

function close() {

}

function cons() {

}

function is_clump() {

}

// table symbole = list symbole en clump, champ 3 = 0
// chaque "car" clump symbole (tag 3)
//     champ 1 = str nom
//     champ 2 = val du sym (procedure, champ code = index)
//     champ 3 = 1
// symbol = clump (avec champ 3 = 3)

const primitives = [
    id,
    arg1,
    arg2,
    close,
    cons,
    is_clump,
    field0,
    field1,
    field2,
    field0_set,
    field1_set,
    field2_set,
    eq,
    lt,
    add,
    sub,
    mul,
    div,
    getchar,
    putchar,
]


function _call_or_jump(call_n_jump, proc_clump) {
    const proc_code = proc_clump[CAR_I]
    const [args, code,] = proc_code

    const is_primitive = typeof (code) === 'number';

    const old_env = _skip(args)

    if (call_n_jump) {
        if (is_primitive) {
            let prim_code = from_fixnum(code)
            let prim = primitives[prim_code]
            prim()
        } else {
            push_clump()
            stack = [old_env, proc_clump, pc]
            pc = code
        }
    } else {
        const [curr_env, , curr_code] = _env()
        if (is_primitive) {
            let prim_code = from_fixnum(code)
            let prim = primitives[prim_code]
            prim()

            stack[CDR_I] = curr_env
            pc = curr_code
        } else {
            push_clump()
            stack = [curr_env, proc_clump, curr_code]
            pc = code
        }
    }
}

function alloc_str(str) {
    return str.split("").reverse().reduce((old, chr) => {
        return [chr.charCodeAt(0), old, TAG_STR]
    }, NULL);
}

function _read_vm_str(vm_str) {
    let str = ""

    while (NULL !== vm_str) {
        str += String.fromCharCode(vm_str[CAR_I])
        vm_str = vm_str[CDR_I]
    }

    return str
}

function build_sym_table(code) {
    const symbol_table = {}
    const lines = code.split(os.EOL)
    const marker = "symbol-table: ";

    function parse_symbol_array(array_line) {
        const symbols_array = JSON.parse(array_line.substr(marker.length))
        const nb_symbols = symbols_array.length;

        let next = NULL;
        for (let j = nb_symbols - 1; j > -1; j--) {
            const name = symbols_array[j];
            let proc;

            if (j <= primitives.length) {
                proc = [to_fixnum(j), 0, TAG_PROC]
            } else {
                proc = [j, 0, 0]
            }

            const symbol = [alloc_str(name), proc, TAG_SYM]
            const entry = [symbol, next, TAG_PAIR]

            next = entry
        }

        // affect the global symbol table variable
        st = next
    }

    lines.every(line => {
        if (line.startsWith(marker)) {
            parse_symbol_array(line);
            return false
        }
        return true;
    });

    return symbol_table
}

function parse_sexpr(bytecode) {
    let scan = 1;
    let stack = []
    let word = ""
    let elements = []

    const push_word = () => {
        if (word.length !== 0) {
            elements.push(word)
            word = ""
        }
    }

    while (scan < bytecode.length - 1) {
        const c = bytecode[scan];

        if (c === '(') {
            stack.push(elements)
            elements = []
            scan++
        } else if (c === ')') {
            push_word()

            let complete = elements;
            elements = stack.pop()
            elements.push(complete)
            word = ""
            scan++
        } else if (c === ' ') {
            push_word()
            scan++
        } else {
            word += c
            scan++
        }
    }

    return JSON.stringify(elements)
}

function parse_code(code) {
    const lines = code.split(os.EOL).slice(2)

    console.dir(lines.map(parse_sexpr))
}

function init_prims() {
}


function build_clump_codes() {

}

function init_stack() {

}

function run() {

}

function _dump_symbol_table() {
    let scout = st

    while (scout !== NULL) {
        const sym = scout[CAR_I]
        const name = _read_vm_str(sym[CAR_I])
        const proc_or_pair = sym[CDR_I]

        let title;
        if (proc_or_pair[TAG_I] === TAG_PROC) {
            // TODO: check if int or clump
            title = "PRIM(" + from_fixnum(proc_or_pair[CAR_I]) + ")"
        } else {
            title = "UNALLOC"
        }

        console.log(name + ":" + title)
        scout = scout[CDR_I]
    }
}

function vm(code) {
    build_sym_table(code)
    _dump_symbol_table()
}


const main = async () => {
    fs.readFile("./lib1.o", "utf-8", (err, data) => {
        if (err) {
            console.error("Failed to read the source file: " + err)
        } else {
            vm(data)
        }
    });
}

main()




