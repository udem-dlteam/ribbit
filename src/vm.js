/*
uVM implementation in Javascript
Based-off the python implementation. Clumps are lists.
 */
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

/** Variables for the VM **/
let stack = NIL
let pc = stack

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
    [_, stack, _] = stack
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

const lt = () => _binop((x, y) => x < y)
const eq = () => _binop((x, y) => x === y)
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

const int_to_prim = [
    add,
    sub,
    mul,
    div,
    lt,
    eq,
    clump,
    push_clump,
    pop_clump,
    putchar,
    getchar,
    field0,
    field1,
    field2,
    field0_set,
    field1_set,
    field2_set,
    call,
    jump
]

function _call_or_jump(call_n_jump, proc_clump) {
    const proc_code = proc_clump[CAR_I]
    const [args, code,] = proc_code

    const is_primitive = typeof (code) === 'number';

    const old_env = _skip(args)

    if (call_n_jump) {
        if (is_primitive) {
            let prim_code = from_fixnum(code)
            let prim = int_to_prim[prim_code]
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
            let prim = int_to_prim[prim_code]
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


const main = () => {
    _push_and_set(5)
    _push_and_set(5)
    _print_stack()

    eq()
    _print_stack()
}

main()




