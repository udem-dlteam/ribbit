package main

import (
	"fmt"
	"io"
	"os"
)

const DebugICall = false

//goland:noinspection SpellCheckingInspection
const Input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" // RVM code that prints HELLO!

const (
	InstrCall  = 0
	InstrSet   = 1
	InstrGet   = 2
	InstrConst = 3
	InstrIf    = 4
	InstrHalt  = 5
)

const (
	PairTag      = 0
	ClosureTag   = 1
	SymbolTag    = 2
	StringTag    = 3
	SingletonTag = 5
)

// ===============================================
// ===============================================
//                 OBJ + RIB
//                   + prim
// ===============================================
// ===============================================

type prim1 func(x Obj) Obj
type prim2 func(x, y Obj) Obj
type prim3 func(x, y, z Obj) Obj

type Obj interface {
	Number() bool
	Rib() bool
	Field0() Obj
	Field0Set(Obj) Obj
	Field1() Obj
	Field1Set(Obj) Obj
	Field2() Obj
	Field2Set(Obj) Obj
	Value() int
	Add(int) Obj
}

type Num struct {
	x int
}

type Rib struct {
	x, y, z Obj
}

func tagNum(x int) Obj {
	nb := new(Num)
	nb.x = x
	return nb
}

func (num *Num) Number() bool {
	return true
}

func (num *Num) Rib() bool {
	return false
}

func (num *Num) Field0() Obj {
	panic("Not a ")
}

func (num *Num) Field1() Obj {
	panic("Not a ")
}

func (num *Num) Field2() Obj {
	panic("Not a ")
}

func (num *Num) Field0Set(Obj) Obj {
	panic("Not a ")
}

func (num *Num) Field1Set(Obj) Obj {
	panic("Not a ")
}

func (num *Num) Field2Set(Obj) Obj {
	panic("Not a ")
}

func (num *Num) Value() int {
	return num.x
}

func (num *Num) Add(v int) Obj {
	r := num.x + v
	return tagNum(r)
}

func (*Rib) Number() bool {
	return false
}

func (*Rib) Rib() bool {
	return true
}

func (trio *Rib) Field0() Obj {
	return trio.x
}

func (trio *Rib) Field1() Obj {
	return trio.y
}

func (trio *Rib) Field2() Obj {
	return trio.z
}

func (trio *Rib) Field0Set(obj Obj) Obj {
	trio.x = obj
	return obj
}

func (trio *Rib) Field1Set(obj Obj) Obj {
	trio.y = obj
	return obj
}

func (trio *Rib) Field2Set(obj Obj) Obj {
	trio.z = obj
	return obj
}

func (*Rib) Value() int {
	panic("Not a number")
}

func (*Rib) Add(int) Obj {
	panic("Not a number")
}

// ===============================================
// ===============================================
//                Input PROCESSING
// ===============================================
// ===============================================

var pos = 0

func getByte() byte {
	r := Input[pos]
	pos++
	return r
}

func getCode() int {
	x := int(getByte()) - 35

	if x < 0 {
		return 57
	} else {
		return x
	}
}

func getInt(n int) int {
	x := getCode()
	n *= 46

	if x < 46 {
		return n + x
	} else {
		return getInt(n + x - 46)
	}
}

// ===============================================
// ===============================================
//                    CONSTANTS
// ===============================================
// ===============================================

var stack = tagNum(0)
var symbolTable = tagNum(0)
var pc = tagNum(0)

var FALSE *Rib = nil
var TRUE *Rib = nil
var NIL *Rib = nil

func push(val Obj) {
	tos := new(Rib)
	tos.x = val
	tos.y = stack
	tos.z = tagNum(PairTag)

	stack = tos
}

func pop() Obj {
	x := stack.Field0()
	stack = stack.Field1()
	return x
}

func allocRib(car, cdr, tag Obj) Obj {
	push(car)
	allocated := stack

	oldStack := allocated.Field1()
	stack = oldStack

	allocated.Field1Set(cdr)
	allocated.Field2Set(tag)

	return allocated
}

func lstLength(lst Obj) Obj {
	n := 0

	for lst.Rib() && lst.Field2().Value() == 0 {
		n++
		lst = lst.Field1()
	}

	return tagNum(n)
}

func createSym(chars Obj) Obj {
	str := allocRib(chars, lstLength(chars), tagNum(StringTag))
	sym := allocRib(FALSE, str, tagNum(SymbolTag))
	return allocRib(sym, symbolTable, tagNum(PairTag))
}

func listTail(list, i Obj) Obj {
	if i.Value() == 0 {
		return list
	} else {
		return listTail(list.Field1(), i.Add(-1))
	}
}

func listRef(list, i Obj) Obj {
	return listTail(list, i).Field0()
}

func symbolRef(depth Obj) Obj {
	return listRef(symbolTable, depth)
}

func setGlobal(c Obj) {
	symbolTable.Field0().Field0Set(c)
	symbolTable = symbolTable.Field1()
}

func getOperand(o Obj) Obj {
	var op Obj
	if o.Number() {
		op = listTail(stack, o)
	} else {
		op = o
	}

	return op.Field0()
}

func buildSymTable() {
	n := getInt(0)

	for n > 0 {
		n--
		symbolTable = createSym(NIL)
	}

	accum := Obj(NIL)

	for {
		c := int(getByte())

		if c == 44 {
			symbolTable = createSym(accum)
			accum = NIL
			continue
		}

		if c == 59 {
			break
		}

		accum = allocRib(tagNum(c), accum, tagNum(PairTag))
	}

	symbolTable = createSym(accum)
}

func decode() {

	weights := []int{20, 30, 0, 10, 11, 4}
	var n Obj
	var d, op int

	for {
		x := getCode()
		n = tagNum(x)
		op = -1

		for {
			op++
			d = weights[op]

			if n.Value() > (2 + d) {
				n = n.Add(-(d + 3))
			} else {
				break
			}
		}

		if x > 90 {
			op = InstrIf
			n = pop()
		} else {
			if op == 0 {
				stack = allocRib(tagNum(0), stack, tagNum(PairTag))
			}

			if n.Value() >= d {
				if n.Value() == d {
					n = tagNum(getInt(0))
				} else {
					n = symbolRef(tagNum(getInt(n.Value() - d - 1)))
				}
			} else {
				if op < 3 {
					n = symbolRef(n)
				}
			}

			if op > 4 {
				inner := allocRib(n, tagNum(0), pop())
				n = allocRib(inner, NIL, tagNum(ClosureTag))

				if stack == nil || (stack.Number() && stack.Value() == 0) {
					break
				}
				op = InstrConst
			} else if op > 0 {
				op--
			} else {
				op = 0
			}
		}

		stack.Field0Set(allocRib(tagNum(op), n, stack.Field0()))
	}

	pc = n.Field0().Field2()
}

func initConstantRibs() {

	init := func(x, y, z int) *Rib {
		cl := new(Rib)
		cl.x = tagNum(x)
		cl.y = tagNum(y)
		cl.z = tagNum(z)
		return cl
	}

	TRUE = init(0, 0, SingletonTag)
	FALSE = init(0, 0, SingletonTag)
	NIL = init(0, 0, SingletonTag)
}

func setupStack() {
	stack = allocRib(
		tagNum(0),
		tagNum(0),
		allocRib(tagNum(InstrHalt),
			tagNum(0),
			tagNum(PairTag)))

}

func doPrim1(f prim1) {
	x := pop()
	push(f(x))
}

func doPrim2(f prim2) {
	y := pop()
	x := pop()
	push(f(x, y))
}

func doPrim3(f prim3) {
	z := pop()
	y := pop()
	x := pop()
	push(f(x, y, z))
}

func boolean(x bool) Obj {
	if x {
		return TRUE
	} else {
		return FALSE
	}
}

func prim(primNo int) {

	if DebugICall {
		fmt.Printf("Calling primitive %d\n", primNo)
	}

	switch primNo {
	case 0: //
		doPrim3(func(x, y, z Obj) Obj {
			return allocRib(x, y, z)
		})
	case 1:
		doPrim1(func(x Obj) Obj {
			return x
		})
	case 2:
		pop()
	case 3:
		x := pop()
		pop()
		push(x)
	case 4:
		x := stack.Field0().Field0()
		y := stack.Field1()
		z := tagNum(ClosureTag)
		stack.Field0Set(allocRib(x, y, z))
	case 5:
		doPrim1(func(x Obj) Obj {
			return boolean(x.Rib())
		})
	case 6:
		doPrim1(func(x Obj) Obj {
			return x.Field0()
		})
	case 7:
		doPrim1(func(x Obj) Obj {
			return x.Field1()
		})
	case 8:
		doPrim1(func(x Obj) Obj {
			return x.Field2()
		})
	case 9:
		doPrim2(func(x, y Obj) Obj {
			return x.Field0Set(y)
		})
	case 10:
		doPrim2(func(x, y Obj) Obj {
			return x.Field1Set(y)
		})
	case 11:
		doPrim2(func(x, y Obj) Obj {
			return x.Field2Set(y)
		})
	case 12:
		doPrim2(func(x, y Obj) Obj {
			if x.Number() && y.Number() {
				return boolean(x.Value() == (y.Value()))
			} else if x.Rib() && y.Rib() {
				return boolean(x == y)
			} else {
				return FALSE
			}
		})
	case 13:
		doPrim2(func(x, y Obj) Obj {
			return boolean(x.Value() < y.Value())
		})
	case 14:
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() + y.Value())
		})
	case 15:
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() - y.Value())
		})
	case 16:
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() * y.Value())
		})
	case 17:
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() / y.Value())
		})
	case 18:
		if pos < len(Input) {
			push(tagNum(int(getByte())))
		} else {
			buff := make([]byte, 1)
			count, err := os.Stdin.Read(buff)

			if err == io.EOF {
				// we are done
				os.Exit(0)
			} else if nil != err {
				panic(err)
			} else if count != 1 {
				panic("Failed to read 1char")
			}

			push(tagNum(int(buff[0])))
		}
	case 19:
		doPrim1(func(x Obj) Obj {
			fmt.Printf("%c", byte(x.Value()))
			return x
		})
	}
}

func getCont() Obj {
	s := stack

	for s.Field2().Number() && s.Field2().Value() == 0 {
		s = s.Field1()
	}

	return s
}

func run() {
	for {
		instr := pc.Field0().Value()
		operand := pc.Field1()
		next := pc.Field2()

		switch instr {
		case InstrCall: // call
			call := next.Rib()
			if DebugICall {
				if call {
					fmt.Println("--- call")
				} else {
					fmt.Println("--- jump")
				}
			}

			proc := getOperand(operand)
			code := proc.Field0()

			if code.Number() {
				prim(code.Value())

				if call {
					code = pc
				} else {
					code = getCont()
					stack.Field1Set(code.Field0())
				}
			} else {
				if DebugICall {
					fmt.Printf("Calling a symbol\n")
				}

				argC := code.Field0().Value()

				c2 := allocRib(tagNum(0), proc, tagNum(PairTag))
				s2 := c2

				for argC > 0 {
					argC--
					s2 = allocRib(pop(), s2, tagNum(PairTag))
				}

				if call {
					c2.Field0Set(stack)
					c2.Field2Set(pc.Field2())
				} else {
					cont := getCont()
					c2.Field0Set(cont.Field0())
					c2.Field2Set(cont.Field2())
				}

				stack = s2
			}

			pc = code.Field2()
		case InstrSet: // set
			if DebugICall {
				fmt.Println("--- set")
			}
			x := pop()

			if pc.Field1().Number() {
				listTail(stack, pc.Field1()).Field0Set(x)
			} else {
				pc.Field1().Field0Set(x)
			}
			pc = pc.Field2()
		case InstrGet: // get
			if DebugICall {
				fmt.Println("--- get")
			}
			push(getOperand(pc.Field1()))
			pc = pc.Field2()
		case InstrConst: // const
			if DebugICall {
				fmt.Println("--- const")
			}
			push(pc.Field1())
			pc = pc.Field2()
		case InstrIf: // if
			if DebugICall {
				fmt.Println("--- if")
			}
			p := pop()

			if p == FALSE {
				pc = pc.Field2()
			} else {
				pc = pc.Field1()
			}
		default:
			fmt.Printf("Unknown instruction: %d\n", instr)
			fallthrough
		case InstrHalt:
			fmt.Println("Bye bye!")
			os.Exit(0)
		}
	}
}

func initVm() {
	initConstantRibs()
	buildSymTable()
	decode()

	setGlobal(allocRib(tagNum(0), symbolTable, tagNum(ClosureTag)))
	setGlobal(FALSE)
	setGlobal(TRUE)
	setGlobal(NIL)

	setupStack()

	run()
}

func main() {
	initVm()
}
