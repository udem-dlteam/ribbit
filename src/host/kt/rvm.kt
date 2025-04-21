import java.io.InputStreamReader

// @@(replace ");'u=>vC=>vQC=>vQ@=>vQ@=>vQ9=>vQ<=k['9lkv5[(9lkl[&9lkmy" (encode-writable-ascii "\"\\$")
var Input = ");'u=>vC=>vQC=>vQ@=>vQ@=>vQ9=>vQ<=k['9lkv5[(9lkl[&9lkmy"
// )@@

const val InstrCall  = 0
const val InstrSet   = 1
const val InstrGet   = 2
const val InstrConst = 3
const val InstrIf    = 4
const val InstrHalt  = 5

const val PairTag      = 0
const val ProcedureTag = 1
const val SymbolTag    = 2
const val StringTag    = 3
const val VectorTag    = 4
const val SingletonTag = 5


// equiv to initConstantRibs in go impl
var TRUE: Obj = Rib(Num(0), Num(0), Num(SingletonTag))
var FALSE: Obj = Rib(Num(0), Num(0), Num(SingletonTag))
var NIL: Obj = Rib(Num(0), Num(0), Num(SingletonTag))


// ===============================================
// ===============================================
//                 OBJ + RIB
//                   + prim
// ===============================================
// ===============================================

var preferInstr = true
sealed interface Obj {
	var car: Obj
	var cdr: Obj
	var tag: Obj
	val value: Int
	fun isPair(): Boolean
	operator fun plus(v: Int): Num
	operator fun plus(v: Obj): Num
	operator fun minus(v: Int): Num
	operator fun minus(v: Obj): Num
	operator fun times(v: Int): Num
	operator fun times(v: Obj): Num
	operator fun div(v: Int): Num
	operator fun div(v: Obj): Num
	fun pp(): String {
		return when (this) {
			is Num -> "${this.value}"
			is Rib -> "(${this.car.pp()} ${this.cdr.pp()} ${this.tag.pp()})"
		}
	}


	fun asValue(): Value {
		val obj = this
		return if (obj is Num) {
			obj
		} else  {
			if (!preferInstr && obj.tag == Num(0)) {
				return Pair(obj)
			}
			when (val out = parseInstruction(obj)) {
				is Instruction -> out
				null -> {
					if (obj.tag is Rib) {
						return OtherRib(obj)
					}
					when (obj.tag.value) {
						PairTag -> {
							when (val instr = parseInstruction(obj)) {
								is Instruction -> {
									if (preferInstr) {
										instr
									} else {
										Pair(obj)
									}
								}
								null -> Pair(obj)
							}
						}
						ProcedureTag -> Procedure(obj)
						SymbolTag -> Symbol(obj)
						StringTag -> ValueString(obj)
						VectorTag -> Vector(obj)
						SingletonTag -> {
							when (this) {
								TRUE -> True
								FALSE -> False
								NIL -> Nil
								else -> throw IllegalStateException("invalid special symbol")
							}
						}
						else -> OtherRib(obj)
					}
				}
			}
		}
	}
}

class Num(override val value: Int): Obj, Value() {
	override var car: Obj
		get() = throw IllegalAccessException("try to use a Num as a rib")
		set(_) = throw NotImplementedError()
	override var cdr: Obj
		get() = throw NotImplementedError()
		set(_) = throw NotImplementedError()
	override var tag: Obj
		get() = throw NotImplementedError()
		set(_) = throw NotImplementedError()

	override fun isPair(): Boolean {
		return false
	}


	override fun plus(v: Int): Num {
		val r = value + v
		return Num(r)
	}

	override fun plus(v: Obj): Num {
		return plus(v.value)
	}

	override fun minus(v: Int): Num {
		val r = value - v
		return Num(r)
	}

	override fun minus(v: Obj): Num {
		return this.minus(v.value)
	}

	override fun times(v: Int): Num {
		val r = value * v
		return Num(r)
	}

	override fun times(v: Obj): Num {
		return this.times(v.value)
	}

	override fun div(v: Int): Num {
		val r = value / v
		return Num(r)
	}

	override fun div(v: Obj): Num {
		return div(v.value)
	}

	override val obj: Obj
		get() = this

	override fun equals(other: Any?): Boolean {
		return when (other) {
			is Num -> value==other.value
			is Int -> value==other
			else -> false
		}
	}

	override fun hashCode(): Int {
		return value.hashCode()
	}

	override fun toString(): String {
		return value.toString()
	}
}

class Rib(override var car: Obj, override var cdr: Obj, override var tag: Obj): Obj {
	override val value: Int
		get() = throw IllegalAccessException("try to use a rib as a num")

	override fun isPair(): Boolean {
		return (tag is Num) && tag.equals(0)
	}

	override fun plus(v: Int): Num = throw IllegalAccessException("try to use a rib as a num")
	override fun plus(v: Obj): Num = throw IllegalAccessException("try to use a rib as a num")
	override fun minus(v: Int): Num = throw IllegalAccessException("try to use a rib as a num")
	override fun minus(v: Obj): Num = throw IllegalAccessException("try to use a rib as a num")
	override fun times(v: Int): Num = throw IllegalAccessException("try to use a rib as a num")
	override fun times(v: Obj): Num = throw IllegalAccessException("try to use a rib as a num")
	override fun div(v: Int): Num = throw IllegalAccessException("try to use a rib as a num")
	override fun div(v: Obj): Num = throw IllegalAccessException("try to use a rib as a num")

	override fun toString(): String {
		return toStringRecursive(this, mutableSetOf())
	}
}


// convert an obj to a string and dealing with circularity
fun toStringRecursive(obj: Obj, set: MutableSet<Obj>): String {
	if (obj !is Num && set.contains(obj)) {
		return "[...]"
	} else if (obj !is Num) {
		set.add(obj)
	}
	return when (obj) {
		is Num -> obj.value.toString()
		is Rib -> "[${toStringRecursive(obj.car, set)}, ${toStringRecursive(obj.cdr, set)}, ${toStringRecursive(obj.tag, set)}]"
	}
}





// ===============================================
// ===============================================
//                Input PROCESSING
// ===============================================
// ===============================================

var pos = 0

fun getCharValue(): Int {
	val r = Input[pos]
	pos++
	return r.code
}


fun getCode(): Int {
	val out = when (val x = getCharValue()) {
		32 -> 0 						// out 0
		33 -> 1 						// out 1
		35 -> 2 						// out 2
		in 37..91 -> x - 34 		// out 3..57
		in 93..126 -> x - 35		// out 58..91
		else -> throw IllegalAccessException("$x is not a valid byte value")
	}
	return out
}



fun getInt(n: Int): Int {
	val nextByte = getCode()
	val accumulator = n * 46

	return if (nextByte < 46) {
		accumulator + nextByte
	} else {
		getInt(accumulator + nextByte - 46)
	}
}

// ===============================================
// ===============================================
//                    CONSTANTS
// ===============================================
// ===============================================

var stack: Obj = Num(0)
var symbolTable: Obj = NIL
var pc: Obj = Num(0)


fun push(obj: Obj) {
	val tos = Rib(obj, stack, Num(PairTag))
	stack = tos
}


fun pop(): Obj {
	val out = stack.car
	stack = stack.cdr
	return out
}


fun listTail(list: Obj, i: Num): Obj {
	var cur = list
	var j = i.value
	while (j > 0) {
		cur=cur.cdr
		j -= 1
	}
	return cur
}

fun symbolRef(depth: Num): Obj {
	return listTail(symbolTable, depth).car
}

fun setGlobal(c: Obj) {
	symbolTable.car.car = c
	symbolTable = symbolTable.cdr
}

fun buildSymTable() {
	val n = Num(getInt(0))
	for (i in 0..<n.value) {
		symbolTable = cons(createSymbol(FALSE, ""), symbolTable)
	}

	var symbolNameChars = NIL
	var symbolNameLen = 0
	while (true) {
		val c = getCharValue()
		if (c == ','.code) {
			val symbolName = createString(symbolNameChars, symbolNameLen)
			val symbolToAppend = createSymbol(FALSE, symbolName)
			symbolTable=cons(symbolToAppend, symbolTable)
			symbolNameChars = NIL
			symbolNameLen = 0
		} else {
			if (c == ';'.code) {
				val symbolName = createString(symbolNameChars, n.value)
				val symbolToAppend = createSymbol(FALSE, symbolName)
				symbolTable=cons(symbolToAppend, symbolTable)
				break
			}

			symbolNameChars = cons(Num(c), symbolNameChars)
			symbolNameLen += 1
		}
	}
}


fun cons(car: Obj, cdr: Obj): Obj {
	return Rib(car, cdr, Num(PairTag))
}

fun createString(string: String): Obj {
	var tail = NIL
	for (char in string.reversed()) {
		tail = cons(Num(char.code), tail)
	}
	return Rib(tail, Num(string.length), Num(StringTag))
}

fun createString(chars: Obj, length: Int): Obj {
	return Rib(chars, Num(length), Num(StringTag))
}

fun createSymbol(value: Obj, name: String): Obj {
	return Rib(value, createString(name), Num(SymbolTag))
}

fun createSymbol(value: Obj, name: Obj): Obj {
	return Rib(value, name, Num(SymbolTag))
}

// this class and its children exist to provide nice interpretable representation of the varius Obj for debugging purpose
sealed class Value {
	abstract val obj: Obj
}
class Pair (override val obj: Obj) : Value(), Iterable<Obj> {
	val car: Obj = obj.car
	val cdr: Obj = obj.cdr
	override fun iterator(): PairIterator {
		return PairIterator(this)
	}

	override fun toString(): String {
		var out = "("
		val iter = iterator()
		for(elm in iter) {
			out += elm.asValue().toString()
			out += " "
		}
		return "$out)"
	}

	class PairIterator(private var curr: Pair): Iterator<Obj> {
		var doted: Boolean=false
		private var finished: Boolean=false
		override fun hasNext(): Boolean {
			return !finished
		}

		override fun next(): Obj {
			if (finished) {
				throw Exception("finished")
			} else if (!doted) {
				val elm = curr.car
				val nextCurr = curr.cdr
				if (nextCurr === NIL) {
					finished = true
				} else if (nextCurr.asValue() is Pair) {
					curr = nextCurr.asValue() as Pair
				} else {
					doted = true
				}
				return elm
			} else {
				finished = true
				return curr.cdr
			}
		}
	}
}

data class Procedure(override val obj: Obj) : Value() {
	val code: Obj
		get() = obj.car
	val env: Obj
		get() = obj.cdr

	override fun toString(): String {
		return "Procedure(code=${code}, env=${env})"
	}
}

data class Symbol(override val obj: Obj): Value() {
	var value: Obj
		get() = obj.car
		set(value) {obj.car = value}

	val name: Obj
		get() = obj.cdr

	override fun toString(): String {
		return "(name=${name.asValue()}, value=${value.asValue()})"
	}
}

data class ValueString(override val obj: Obj): Value() {
	val chars: Obj
		get() = obj.car
	val length: Obj
		get() = obj.cdr

	val string: String
		get() {
			var out = ""
			if (chars.asValue() is Pair) {
				for (elm in (chars.asValue() as Pair)) {
					out += Char(elm.value)
				}
			}
			return out
		}

	override fun toString(): String {
		return string
	}
}
data class Vector(override val obj: Obj): Value()
sealed class Special: Value()
data object False : Special() {
	override val obj: Obj
		get() = FALSE

	override fun toString(): String {
		return "FALSE"
	}
}
data object True : Special() {
	override val obj: Obj
		get() = TRUE

	override fun toString(): String {
		return "TRUE"
	}
}
data object Nil : Special() {
	override val obj: Obj
		get() = NIL

	override fun toString(): String {
		return "NIL"
	}
}

class OtherRib(override val obj: Obj): Value() {
	override fun toString(): String {
		return "Rib(${obj.car.asValue()} ${obj.cdr.asValue()} ${obj.tag.asValue()})"
	}
}

fun ribOrStackIndex(obj: Obj): Obj {
	return when(obj) {
		is Rib -> obj
		is Num -> listTail(stack, obj)
	}
}

fun getContinuation(): Obj {
	var curr = stack
	while (curr.tag == Num(0)) {
		curr = curr.cdr
	}
	return curr
}

sealed class Instruction(override val obj: Obj): Value() {
	fun execute(): Boolean {
		when(this) {
			is Call -> {
				val procedure = ribOrStackIndex(this.slot).car
				val code=procedure.car
				if (code is Rib) {
					val continuationRib = Rib(Num(0), procedure, Num(0))
					var stackFrame: Obj = continuationRib
					var nparams = code.car.value.shr(1)
					while (nparams!=0) {
						stackFrame=cons(pop(), stackFrame)
						nparams -= 1
					}
					continuationRib.car=stack
					continuationRib.tag=pc.tag
					stack=stackFrame
					pc=code.tag
				} else {
					prim(code.value)
					pc=next
				}
			}
			is Const -> {
				push(this.data)
				pc = this.next
			}
			is Get -> {
				val operen = ribOrStackIndex(this.slot).car
				push(operen)
				pc = this.next
			}
			is If -> {
				if (pop() != FALSE){
					pc = this.then
				} else{
					pc = this.next
				}
			}
			is Jump -> {
				val procedure = ribOrStackIndex(this.slot).car
				val code = procedure.car
				if (code is Num) {
					prim(code.value)
					val continuation = getContinuation()
					stack.cdr = continuation.car
					pc = continuation.tag
					// todo shit to fix
				} else {
					val continuationRib = Rib(Num(0), procedure, Num(0))
					var stackFrame: Obj = continuationRib
					var nparams = code.car.value.shr(1)
					while (nparams!=0) {
						stackFrame=cons(pop(), stackFrame)
						nparams -= 1
					}

					val returnContinuation = getContinuation()
					continuationRib.car = returnContinuation.car
					continuationRib.tag = returnContinuation.tag
					stack = stackFrame
					pc=code.tag
				}
			}

			is Set -> {
				val target = ribOrStackIndex(this.slot)
				target.car = pop()
				pc = this.next
			}
			is Halt -> {
				return false
			}
		}
		return true
	}
}

/**
 * This instruction should probably be call return, since it what it does. I am not exactly sure what the slot object
 * is. The information seam to be documented in the "The call protocol" section of this paper
 * https://www-labs.iro.umontreal.ca/~feeley/papers/YvonFeeleyVMIL21.pdf. Beware figure 5, the prof told me that they
 * found an error in it after publication.
 *
 * That seam like the relevant bit:
 *
 * "When a jump instruction is executed, the continuation rib is found by looping
 * through the stack until a rib with a non-zero third field is found. A local variable is accessed
 * by using a get instruction whose integer index indicates a slot before the continuation
 * rib. A free variable is also accessed by using a get instruction, but with an integer index
 * that indicates a slot past the continuation rib. This works because the chaining of the
 * stack ribs is in the second field, and in the second field of
 * the continuation rib is a reference to the current procedure,
 * and the second field of the procedure object is the env field
 * containing a reference to the stack when the closure was
 * created"
 * */
data class Jump(override val obj: Obj): Instruction(obj) {
	val slot: Obj
		get() = obj.cdr

	override fun toString(): String {
		return "Jump(slot=${slot.asValue()})"
	}
}


/**
 * call a procedure or a primitive. The slot object hold the thing to be call, the next field hold the instruction
 * to execute when the call return. If the slot object is a num, it means that it a call to a primitive
 *
 * If the slot object is not a num it start to involve the call protocol from this paper
 * https://www-labs.iro.umontreal.ca/~feeley/papers/YvonFeeleyVMIL21.pdf. Beware figure 5, the prof told me that they
 * found an error in it after publication
 * */
data class Call(override val obj: Obj): Instruction(obj) {
	val slot: Obj
		get() = obj.cdr
	val next: Obj
		get() = obj.tag

	override fun toString(): String {
		return "Call(slot=${slot.asValue()})"//, \nnext=${next.asValue()})"
	}

}

/**
 * Set the value of the obj in the slot with an element popped from the stack. The exact semantic should be checked with
 * the python implementation
 * */
data class Set(override val obj: Obj): Instruction(obj) {
	val slot: Obj
		get() = obj.cdr
	val next: Obj
		get() = obj.tag

	override fun toString(): String {
		return "Set(slot=${slot.asValue()})"//, \nnext=${next.asValue()})"
	}
}

/**
 * push the value of the obj in slot to the stack. The exact semantic should be checked with
 * the python implementation.
 * */
data class Get(override val obj: Obj): Instruction(obj) {
	val slot: Obj
		get() = obj.cdr
	val next: Obj
		get() = obj.tag

	override fun toString(): String {
		return "Get(slot=${slot.asValue()})"//, \nnext=${next.asValue()})"
	}
}

/**
 * push a constant value contain in the slot to the stack. The exact semantic should be checked with
 * the python implementation
 * */
data class Const(override val obj: Obj): Instruction(obj) {
	val data: Obj
		get() = obj.cdr
	val next: Obj
		get() = obj.tag

	override fun toString(): String {
		return "Const(data=${data.asValue()})"//, \nnext=${next.asValue()})"
	}
}

/**
 * pop an element from the stack, if this element is not the FALSE special value, the "then" obj
 * is put in the program conter (pc) become else it the "next" obj that is put there
 * */
data class If(override val obj: Obj): Instruction(obj) {
	val then: Obj
		get() = obj.cdr
	val next: Obj
		get() = obj.tag

	override fun toString(): String {
		return "If(then=${then.asValue()}, next=${next.asValue()})"
	}
}

data class Halt(override val obj: Obj): Instruction(obj) {
	override fun toString(): String {
		return "Halt"
	}
}

fun parseInstruction(obj: Obj): Instruction? {
	if (obj !is Rib || obj.car !is Num) {
		return null
	} else if (obj.tag is Num && obj.tag != Num(0)) {
		return null
	} else {
		return when (obj.car.value) {
			InstrCall -> {
				when (obj.tag) {
					Num(0) -> Jump(obj)
					is Num -> null
					else -> Call(obj)
				}
			}
			InstrSet -> Set(obj)
			InstrGet -> Get(obj)
			InstrConst -> Const(obj)
			InstrIf -> If(obj)
			InstrHalt -> {
				if (obj.cdr == Num(0) && obj.tag == Num(0)) {
					Halt(obj)
				} else {
					null
				}
			}
			else -> null
		}
	}
}

/**
 * when it is executed, each decode instruction generate one rvm instruction.
 * I have named the instruction by there action (push, link or merge) follow by the rvm instruction that they generate.
 *
 * It should be noted that the merge action of the const and the "if" do not perform the same action
 *
 * The jump instruction contain one parameter I have call next which work like a return address. It is added using a push action in which
 * it is push on the stack with the next parameter set to the arg.
 *
 * The call instruction contain two parameter, the first one I call slot, it contains the thing that should be call.
 * the second param I call next, it is the instruction that should be executed after the call return.
 * It is added using a link action in which the first param (slot) is set to the arg and second param is set
 * the first instruction on the stack which is popped after that the call instruction is push on the stack
 *
 * The set instruction contain two parameter, the first one I call slot, it contains the target that will
 * be set a runtime by the instruction. the second param I call next, it the next instruction.
 * It is added using a link action in which the first param (slot) is set to the arg and second param is set
 * the first instruction on the stack which is popped after that the instruction is push on the stack
 *
 * The get instruction contain two parameter, the first one I call slot, it contains the target that which value will be
 * obtained at runtime by the instruction. the second param I call next, it is the next instruction.
 * It is added using a link action in which the first param (slot) is set to the arg and second param is set
 * the first instruction on the stack which is popped after that the instruction is push on the stack
 *
 * The const instruction contain two parameter, the first one I call data, it the data that will be push on the stack
 * at runtime by the instruction. the second param I call next, it is the next instruction.
 * It can be added using a link action in which the first param (slot) is set to the arg and second param is set
 * the first instruction on the stack which is popped after that the instruction is push on the stack
 *
 * It can also be using a merge action in which the second parameter is obtained by popping the secound value of the stack,
 * the first value of the stack is use latter. The first parameter is a newly created closure data rib. the car of
 * a closure data rib contain the code that should be call by calling that closure. The cdr of a closure data
 * rib contain the closure environment which is set to zero. The mining of that is unclear to me.
 *
 * The code in the closure data rib is set to a new rib with the arg in it car, 0 in it cdr, and the tag set to the first value
 * of the stack. the meaning of that is unclear.
 *
 * The "if" instruction contain two parameter containing the code of the then and else branch of the "if".
 * the parameters are obtain by popping the stack twice. It is considered a merge action
 *
 * If a merge action fail to pop the stack because it is empty, that signal the end of decoding
 *
 * */
sealed class DecodeInstruction {
	fun execute(): Boolean {
		when (this) {
			is MergeConst -> {
				val y = pop()
				val value = Rib(
					Rib(arg, Num(0), y),
					NIL,
					Num(1))
				if (stack is Num) {
					pc = value.car.tag
					return false
				}
				stack.car=Rib(Num(InstrConst), value, stack.car)
			}
			is MergeIf -> {
				val y = pop()
				val x = pop()
				push(Rib(Num(InstrIf), y, x))
			}
			is PushJump -> {
				push(Rib(Num(InstrCall), arg, Num(0)))
			}
			is LinkCall -> {
				val tag = pop()
				push(Rib(Num(InstrCall), arg, tag))
			}

			is LinkSet -> {
				val tag = pop()
				push(Rib(Num(InstrSet), arg, tag))
			}
			is LinkGet -> {
				val tag = pop()
				push(Rib(Num(InstrGet), arg, tag))
			}
			is LinkConst -> {
				stack.car = Rib(Num(InstrConst), arg, stack.car)
			}
		}
		return true
	}
}

class PushJump(val arg: Obj): DecodeInstruction()
class LinkCall(val arg: Obj): DecodeInstruction()
class LinkSet(val arg: Obj): DecodeInstruction()
class LinkGet(val arg: Obj): DecodeInstruction()
class LinkConst(val arg: Obj): DecodeInstruction()
class MergeConst(val arg: Obj): DecodeInstruction()
data object MergeIf : DecodeInstruction()


/**
 * for each decode instruction we must extract 4 information
 * - The type of instruction
 * - The embedded index
 * - The index form (short or long)
 * - The index
 * - The argument form (index or symbol)
 * - The argument
 *
 *  The stream of decode instruction is conceptually encode in a series a digit in a big base (I think it is in base 91)
 *
 *  I name the first digit of an instruction the opcode digit
 *
 *  The type of instruction can be found by checking in which instruction range the opcode digit is located
 *
 *  The embedded index can be found by subtracting the instruction range start from the opcode digit
 *
 *  If the instruction encode the index in short form, the index is the embedded index
 *  If the index is in long form, it can be obtained using a variable-length encoding thingy in which the
 *  embedded index is use as a starting value.
 *
 *  The argument is the operand on which the decode instruction operate
 *  It can either be a symbol or a number.
 *  If it is a symbol, it is obtained by using to indexing into the symbol using the index
 *  If it is a number, the argument is the index
 *
 *
 */
fun decode() {
	var n: Obj
	var d: Int
	var op: Int
	do {
		val x = getCode()
		n = Num(x)
		op = 0

		while (true) {
			d = listOf(20, 30, 0, 10, 11, 4)[op]
			if (n.value <= 2 + d)
				break
			n = n.minus(d + 3)
			op += 1
		}

		val isSymbol = when (x) {
			in 0..19 -> true
			20 -> false
			in 21..22 -> true
			in 23..52 -> true
			53 -> false
			in 54..55 -> true
			56 -> false
			in 57..58 -> true
			in 59..68 -> false
			69 -> false
			in 70..71 -> true
			in 72..82 -> false
			83 -> false
			in 84..85 -> true
			in 86..89 -> false
			90 -> false
			91 -> null
			else -> throw IllegalStateException("$x is not a valid code")
		}


		val isLong = when (x) {
			in 0..19 -> false
			20 -> true
			in 21..22 -> true
			in 23..52 -> false
			53 -> true
			in 54..55 -> true
			56 -> true
			in 57..58 -> true
			in 59..68 -> false
			69 -> true
			in 70..71 -> true
			in 72..82 -> false
			83 -> true
			in 84..85 -> true
			in 86..89 -> false
			90 -> true
			91 -> null
			else -> throw IllegalStateException("$x is not a valid code")
		}

		val index = when (isLong) {
			true -> {
				if (n.value == d) {
					Num(getInt(0))
				} else {
					Num(getInt((n - d - 1).value))
				}
			}
			false -> n
			null -> null
		}
		val arg = when (isSymbol) {
			true -> symbolRef(index as Num)
			false -> index
			null -> index
		}

		val instr = when (x) {
			in 0..22 -> PushJump(arg!!)
			in 23..55 -> LinkCall(arg!!)
			in 56..58 -> LinkSet(arg!!)
			in 59..71 -> LinkGet(arg!!)
			in 72..85 -> LinkConst(arg!!)
			in 86..90 -> MergeConst(arg!!)
			91 -> MergeIf
			else -> throw IllegalStateException("$x is not a valid code")
		}
	} while(instr.execute())
}


fun setupStack() {
	stack = Rib(
		Num(0),
		Num(0),
		Rib(Num(InstrHalt),
			Num(0),
			Num(PairTag)
		)
	)
}

fun doPrim1(f: (x: Obj) -> Obj):Obj {
	val x = pop()
	val r = f(x)
	push(r)
	return r
}

fun doPrim2(f: (x: Obj, y: Obj) -> Obj):Obj {
	val x = pop()
	val y = pop()
	val r =f(x, y)
	push(r)
	return r
}

fun doPrim3(f: (x: Obj, y: Obj, z: Obj) -> Obj):Obj {
	val x = pop()
	val y = pop()
	val z = pop()
	val r = f(x, y, z)
	push(r)
	return r
}

fun boolean(x: Boolean): Obj {
	return if (x) {
		TRUE
	} else {
		FALSE
	}
}

var stdin = InputStreamReader(System.`in`)

fun prim(primNo: Int) {
	when (primNo) {
		0 -> doPrim3 { z: Obj, y: Obj, x: Obj -> Rib(x, y, z) }
		1 ->  doPrim1 { x: Obj -> x }
		2 -> { pop() ;Num(0)}
		3 -> {val x = pop(); pop(); push(x);}
		4 -> { val x = pop(); push(Rib(x.car, stack, Num(ProcedureTag))) }
		5 -> doPrim1 { x: Obj -> boolean(x is Rib) }
		6 -> doPrim1 { x: Obj -> x.car }
		7 -> doPrim1 { x: Obj -> x.cdr }
		8 -> doPrim1 { x: Obj -> x.tag }
		9 -> doPrim2 { y: Obj, x: Obj -> x.car = y; y }
		10 -> doPrim2 { y: Obj, x: Obj -> x.cdr = y; y }
		11 -> doPrim2 { y: Obj, x: Obj -> x.tag = y; y }
		12 -> { doPrim2 { y: Obj, x: Obj -> if (x is Num && y is Num) { boolean(x.value == y.value)} else if (x is Rib && y is Rib) {boolean(x === y)} else {FALSE}}}
		13 -> doPrim2 { y: Obj, x: Obj -> boolean(x.value < y.value) }
		14 -> doPrim2 { y: Obj, x: Obj -> x+y }
		15 -> doPrim2 { y: Obj, x: Obj -> x-y }
		16 -> doPrim2 { y: Obj, x: Obj -> x*y }
		17 -> doPrim2 { y: Obj, x: Obj -> x/y }
		18 -> { if (pos < Input.length) { push(Num(getCharValue())) } else { val chr = stdin.read(); push(Num(chr))}}
		19 -> { doPrim1 { x: Obj -> print(Char(x.value)); x } }
		else -> throw IllegalStateException("$primNo is not a valid primitive number")
	}
}

fun getCont(): Obj {
	var s = stack
	while (s.tag is Num && s.tag.value == PairTag) {
		s = s.cdr
	}

	return s
}


fun run() {
	do {
		val instr = parseInstruction(pc)
	} while (instr!!.execute())
}

fun resetGlobal() {
	TRUE = Rib(Num(0), Num(0), Num(SingletonTag))
	FALSE = Rib(Num(0), Num(0), Num(SingletonTag))
	NIL = Rib(Num(0), Num(0), Num(SingletonTag))
	stack = Num(0)
	symbolTable = NIL
	pc = Num(0)
}

fun initVm() {
	resetGlobal()
	buildSymTable()
	decode()

	setGlobal(Rib(Num(0), symbolTable,Num(1)) )
	setGlobal(FALSE)
	setGlobal(TRUE)
	setGlobal(NIL)
	setupStack()
	run()
}


fun main() {
	throw Exception("notre rvm ne support pas le arity-check. Veuillez utiliser l'option '-f- arity-check' sur ribbit quand vous compiler du code pour notre rvm")// @@(feature arity-check)@@
	initVm()
}
