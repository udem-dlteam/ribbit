import kotlin.system.exitProcess

typealias Rib = ArrayList<Any>
typealias GenericRib = ArrayList<*>

const val MAX_BYTE = 92
const val MULTI_BYTE_THRESHOLD = MAX_BYTE / 2
const val BYTE_OFFSET = 35
const val BYTE_DEFAULT = '9'.code      // 57
const val STRING_SEP = ','.code        // 44
const val SYMBOL_TABLE_END = ';'.code  // 59

val SHORT_ENCODINGS = arrayOf(20, 30, 0, 10, 11, 4)

//

const val CODE = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"  // @@(replace __SOURCE__ source)@@

fun rib(x: Any, y: Any, z: Any): Rib = arrayListOf(x, y, z)

fun putChar(c: Any): Int {
    print((c as Int).toChar())
    System.out.flush() // FIXME: may be useless
    return c
}

fun getCar() {
    val char = System.`in`.readNBytes(1)[0].toInt()
    push(if (char == -1) -1 else char.toChar())
}

fun push(x: Any) {
    stack = rib(x, stack, 0)
}

fun pop(): Any {
    val valuePopped = stack.asRib()[0]
    stack = stack.asRib()[1]  // set stack to the inner stack
    return valuePopped
}

inline fun <reified T> popAs() = pop() as T

fun setGlobal(value: Any) {
    symbolTable.asRib()[0].asRib()[0] = value
    symbolTable = symbolTable.asRib()[1]
}

// @@(feature boolean
fun toBool(x: Boolean): Rib {
    return if (x) TRUE else FALSE
}
// )@@

fun getByte(): Int = CODE[pos++].code

fun getCode(): Int {
    val adjustedByte = getByte() - BYTE_OFFSET
    return if (adjustedByte < 0) BYTE_DEFAULT else adjustedByte
}

fun getInt(n: Int): Int {
    val code = getCode()
    val offset = n * MULTI_BYTE_THRESHOLD  // n represents the number of offsets x is getting
    return if (code < MULTI_BYTE_THRESHOLD) code + offset else getInt(code + offset - MULTI_BYTE_THRESHOLD)
}


inline fun prim1(crossinline f: (Any) -> Any): () -> Unit {
    return { push(f(pop())) }
}

inline fun prim2(crossinline f: (Any, Any) -> Any): () -> Unit {
    return { push(f(pop(), pop())) }
}

inline fun prim2Rib(crossinline f: (Any, Rib) -> Any): () -> Unit {
    return { push(f(pop(), popAs())) }
}

inline fun prim3(crossinline f: (Any, Any, Any) -> Any): () -> Unit {
    return { push(f(pop(), pop(), pop())) }
}

fun arg2() {
    val x = pop()
    pop()
    push(x)
}

fun close() {
    push(rib(popAs<Rib>()[0], stack, 1))
}

fun fieldSet0(y: Any, x: Rib): Any {
    x[0] = y
    return y
}

fun fieldSet1(y: Any, x: Rib): Any {
    x[1] = y
    return y
}

fun fieldSet2(y: Any, x: Rib): Any {
    x[2] = y
    return y
}

/**
 * @return The next rib that doesn't end with a 0.
 */
fun getCont(): Rib {
    var s: Rib = stack.asRib()
    while (s[2] == 0) s = s[1].asRib()
    return s
}

fun getOpND(o: Any): Any =
    if (isRib(o)) {
        o
    } else {
        listTail(stack.asRib(), o as Int)
    }


fun getSymbolFromRef(ref: Int): Any {
    return listTail(symbolTable.asRib(), ref)[0]
}

fun isRib(x: Any) = x is GenericRib

fun listTail(rib: Rib, index: Int): Rib =
    if (index == 0) {
        rib
    } else {
        listTail(rib[1].asRib(), index - 1)
    }

var PRIMITIVES = arrayOf(
    // @@(primitives (gen body ",")
    prim3 { z, y, x -> rib(x, y, z) },  // @@(primitive (rib z y x))@@
    prim1 { it },                       // @@(primitive (id x))@@
    ::pop,                              // @@(primitive (pop))@@
    ::arg2,                             // @@(primitive (arg2))@@
    ::close,                            // @@(primitive (close))@@
    prim1 { toBool(isRib(it)) },        // @@(primitive (rib? x))@@
    prim1 { it.asRib()[0] },            // @@(primitive (field-0 x))@@
    prim1 { it.asRib()[1] },            // @@(primitive (field-1 x))@@
    prim1 { it.asRib()[2] },            // @@(primitive (field-2 x))@@
    prim2Rib(::fieldSet0),              // @@(primitive (set-field-0 x))@@
    prim2Rib(::fieldSet1),              // @@(primitive (set-field-1 x))@@
    prim2Rib(::fieldSet2),              // @@(primitive (set-field-2 x))@@
    prim2 { y, x -> toBool(if (isRib(x) or isRib(y)) x === y else x == y) }, // @@(primitive (eqv? y x))@@
    prim2 { y, x -> toBool((x as Int) < y as Int) }, // @@(primitive (< x y))@@
    prim2 { y, x -> (x as Int) + y as Int }, // @@(primitive (+ x y))@@
    prim2 { y, x -> (x as Int) - y as Int }, // @@(primitive (- x y))@@
    prim2 { y, x -> (x as Int) * y as Int }, // @@(primitive (* x y))@@
    prim2 { y, x -> (x as Int) / y as Int }, // @@(primitive (/ x y))@@
    ::getCar,                            // @@(primitive (get-char))@@
    prim1(::putChar),                    // @@(primitive (put-char c))@@
    prim1 { exitProcess(0) },      // @@(primitive (exit))@@
    // )@@
)

var pos = 0
val FALSE: Rib = rib(0, 0, 5)
val TRUE: Rib = rib(0, 0, 5)
val NIL: Rib = rib(0, 0, 5)


fun buildSymbolTable(): Rib {
    var symbolTable = NIL
    var n = getInt(0)
    while (n-- > 0) symbolTable = rib(
        rib(FALSE, rib(NIL, 0, 3), 2), symbolTable, 0
    )

    var chars = NIL
    var currentStringLen = 0
    var currentByte: Int = getByte()
    while (currentByte != SYMBOL_TABLE_END) {
        if (currentByte == STRING_SEP) {
            symbolTable = rib(rib(FALSE, rib(chars, currentStringLen, 3), 2), symbolTable, 0)
            chars = NIL
            currentStringLen = 0
        } else {
            chars = rib(currentByte, chars, 0)
            currentStringLen++
        }
        currentByte = getByte()
    }
    symbolTable = rib(rib(FALSE, rib(chars, currentStringLen, 3), 2), symbolTable, 0)
    return symbolTable
}


fun stackIsValid(): Boolean {
    return ((isRib(stack) && stack.asRib().isNotEmpty())
            && (stack is Int && stack != 0))
}

fun decodeRVM(): Rib {
    var n: Any
    while (true) {
        val code = getCode()
        n = code
        var d: Int
        var op = 0
        while (true) {
            d = SHORT_ENCODINGS[op]
            if (n <= 2 + d) break
            n -= d + 3
            op++
        }
        if (code > 90) {
            n = popAs()
        } else {
            if (op == 0) {
                stack = rib(0, stack, 0)
                op++
            }
            n = when {
                n == d -> getInt(0)
                n > d -> getSymbolFromRef(getInt(n - d - 1))
                op < 3 -> getSymbolFromRef(n)
                else -> n
            }
            if (op > 4) {
                n = rib(rib(n, 0, pop()), NIL, 1)
                if (!stackIsValid()) break
                op = 4
            }
        }
        stack.asRib()[0] = rib(op - 1, n, stack.asRib()[0])
    }
    return n.asRib()[0].asRib()[2].asRib()
}

var stack: Any = NIL
var symbolTable: Any = buildSymbolTable()
var pc = decodeRVM()
fun main() {
    setGlobal(rib(0, symbolTable, 1))
    setGlobal(FALSE)
    setGlobal(TRUE)
    setGlobal(NIL)
    stack = rib(0, 0, rib(5, 0, 0))  // primordial continuation (executes the halt instruction)
    while (true) {
        var o = pc[1]
        val i = pc[0] as Int
        when (i) {
            0 -> { // jump/call
                o = getOpND(o).asRib()[0]
                var c = o.asRib()[0]
                if (isRib(c)) {
                    val c2 = rib(0, o, 0)
                    var s2 = c2
                    var nargs = c.asRib()[0] as Int
                    while (nargs > 0) {
                        s2 = rib(pop(), s2, 0)
                        nargs--
                    }
                    if (isRib(pc[2])) { // call
                        c2[0] = stack
                        c2[2] = pc[2]
                    } else { // jump
                        val k = getCont()
                        c2[0] = k[0]
                        c2[2] = k[2]
                    }
                    stack = s2
                } else {  // primitive jump/call
                    PRIMITIVES[c as Int]()
                    if (isRib(pc[2])) {  // call
                        c = pc
                    } else {  // jump
                        c = getCont()
                        stack.asRib()[1] = c[0]
                    }
                }
                pc = c.asRib()[2].asRib()
            }

            1 -> {  // set
                getOpND(o).asRib()[0] = pop()
                pc = pc[2].asRib()
            }

            2 -> {  // get
                push(getOpND(o).asRib()[0])
                pc = pc[2].asRib()
            }

            3 -> {  // const
                push(o)
                pc = pc[2].asRib()
            }

            4 -> {  // if
                pc = pc[if (pop() === FALSE) 2 else 1].asRib()
            }

            // halt (exit)
            else -> break
        }
    }
}


@Suppress("UNCHECKED_CAST")
inline fun <reified T> T.asRib() = this as Rib


