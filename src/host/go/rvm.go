package main

import (
	"fmt"
	"io" // @@(feature go/io)@@
	"os" // @@(feature go/os)@@
	"runtime" // @@(feature go/runtime)@@
	"syscall" // @@(feature go/syscall)@@
)

const DebugICall = false

//goland:noinspection SpellCheckingInspection
// @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
const Input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" // RVM code that prints HELLO!
// )@@

func ShowRib(rib Obj, depth int) {
  if depth < 0 {
    if rib.Number() {
      fmt.Printf("...")
    } else {
      fmt.Printf("[...]")
    }
    return
  }

  if rib.Number() {
    fmt.Printf("%d", rib.Value())
    return
  }
  if (rib == TRUE) {
    fmt.Printf("#t")
    return
  }
  if (rib == FALSE) {
    fmt.Printf("#f")
    return
  }
  if (rib == NIL) {
    fmt.Printf("'()")
    return
  }
  fmt.Printf("[")
  ShowRib(rib.Field0(), depth - 1)
  fmt.Printf(", ")
  ShowRib(rib.Field1(), depth - 1)
  fmt.Printf(", ")
  ShowRib(rib.Field2(), depth - 1)
  fmt.Printf("]")
}

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
	panic("Cannot call field0 on non-primitive")
}

func (num *Num) Field1() Obj {
	panic("Cannot call field1 on non-primitive")
}

func (num *Num) Field2() Obj {
	panic("Cannot call field2 on non-primitive")
}

func (num *Num) Field0Set(Obj) Obj {
	panic("Cannot call field0set on non-primitive")
}

func (num *Num) Field1Set(Obj) Obj {
	panic("Cannot call field1set on non-primitive")
}

func (num *Num) Field2Set(Obj) Obj {
	panic("Cannot call field2set on non-primitive")
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

var NUM0 = tagNum(0)
var stack = NUM0
var symbolTable = NUM0
var pc = NUM0

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

func instTail(list, i Obj) Obj {
	if i.Value() == 0 {
		return list
	} else {
		return listTail(list.Field2(), i.Add(-1))
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

// @@(feature encoding/optimal
func decode() {
	var ranges = []int{1, 2, 3} // @@(replace "{1, 2, 3}" (list->host encoding/optimal/start "{" "," "}"))@@

	//var n Obj
	var i int
	var arg Obj

	for {
		code := getCode()
		arg = tagNum(code)
		range_index := 0

		for {
			if arg.Value() < ranges[range_index] {
				break
			}

			arg = tagNum(arg.Value() - ranges[range_index])
			range_index++
		}

    if (range_index < 4) { push(tagNum(0)) } // JUMP
    if (range_index < 24) {
			if range_index%2>0{
				arg = tagNum(getInt(arg.Value()))
			}
		}

		if range_index < 20 { // jump call set get const
			i = (range_index / 4) - 1
			if i < 0 {
				i = 0
			}
			if (range_index % 4) / 2 >= 1{
				arg = symbolRef(arg)
			} else {
				arg = arg
			}
		} else if range_index < 22 { // const proc
			arg = allocRib(allocRib(arg, NUM0, pop()), Obj(NIL), tagNum(ClosureTag))
			i=3;
			if stack == NUM0 {
				break
			}
		} else if range_index < 24 { // skip
			stack = allocRib(instTail(stack.Field0(), arg), stack, NUM0)
			continue
		} else if (range_index < 25) { // if
			arg = pop()
			i=4;
		}

		stack.Field0Set(allocRib(tagNum(i), arg, stack.Field0()))
	}

	pc = arg.Field0().Field2()
}
// )@@


// @@(feature encoding/original
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
// )@@

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

// @@(feature scm2str
func chars2str(chars Obj) string {
	if chars == NIL {
		return ""
	} else {
		return string(byte(chars.Field0().Value())) + chars2str(chars.Field1())
	}
}

func scm2str(s Obj) string {
	return chars2str(s.Field0())
}
// )@@

// @@(feature str2scm
func str2scm(c string) Obj {
	obj := Obj(NIL)
	length := len(c)

	for i := length - 1; i >= 0; i-- {
		obj = allocRib(tagNum(int(c[i])), obj, tagNum(PairTag))
	}

	return allocRib(obj, tagNum(length), tagNum(StringTag))
}
// )@@

// @@(feature list2scm (use str2scm)
func list2scm(s []string) Obj {
	obj := Obj(NIL)
	for i := len(s) - 1; i >= 0; i-- {
		obj = allocRib(str2scm(s[i]), obj, tagNum(PairTag))
	}

	return obj
}
// )@@


func prim(primNo int) Obj {

	if DebugICall {
		fmt.Printf("Calling primitive %d\n", primNo)
	}

	switch primNo {
		// @@(primitives (gen "case " index ":" body)
	case 0: // @@(primitive (%%rib a b c)
		doPrim3(func(x, y, z Obj) Obj {
			return allocRib(x, y, z)
		}) // )@@
	case 1: // @@(primitive (%%id x)
		doPrim1(func(x Obj) Obj {
			return x
		}) // )@@
	case 2: // @@(primitive (%%arg1 x y)
		pop() // )@@
	case 3: // @@(primitive (%%arg2 x y)
		x := pop()
		pop()
		push(x) // )@@
	case 4: // @@(primitive (%%close rib)
		x := stack.Field0().Field0()
		y := stack.Field1()
		z := tagNum(ClosureTag)
		stack.Field0Set(allocRib(x, y, z)) // )@@
	case 5: // @@(primitive (%%rib? rib)
		doPrim1(func(x Obj) Obj {
			return boolean(x.Rib())
		}) // )@@
	case 6: // @@(primitive (%%field0 rib)
		doPrim1(func(x Obj) Obj {
			return x.Field0()
		}) // )@@
	case 7: // @@(primitive (%%field1 rib)
		doPrim1(func(x Obj) Obj {
			return x.Field1()
		}) // )@@
	case 8: // @@(primitive (%%field2 rib)
		doPrim1(func(x Obj) Obj {
			return x.Field2()
		}) // )@@
	case 9: // @@(primitive (%%field0-set! rib val)
		doPrim2(func(x, y Obj) Obj {
			return x.Field0Set(y)
		}) // )@@
	case 10: // @@(primitive (%%field1-set! rib val)
		doPrim2(func(x, y Obj) Obj {
			return x.Field1Set(y)
		}) // )@@
	case 11: // @@(primitive (%%field2-set! rib val)
		doPrim2(func(x, y Obj) Obj {
			return x.Field2Set(y)
		}) // )@@
	case 12: // @@(primitive (%%eqv? x y)
		doPrim2(func(x, y Obj) Obj {
			if x.Number() && y.Number() {
				return boolean(x.Value() == (y.Value()))
			} else if x.Rib() && y.Rib() {
				return boolean(x == y)
			} else {
				return FALSE
			}
		}) // )@@
	case 13: // @@(primitive (%%< x y)
		doPrim2(func(x, y Obj) Obj {
			return boolean(x.Value() < y.Value())
		}) // )@@
	case 14: // @@(primitive (%%+ x y)
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() + y.Value())
		}) // )@@
	case 15: // @@(primitive (%%- x y)
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() - y.Value())
		}) // )@@
	case 16: // @@(primitive (%%* x y)
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() * y.Value())
		}) // )@@
	case 17: // @@(primitive (%%quotient x y)
		doPrim2(func(x, y Obj) Obj {
			return tagNum(x.Value() / y.Value())
		}) // )@@
	case 18: // @@(primitive (%%getchar) (use go/io go/os)
		if pos < len(Input) {
			push(tagNum(int(getByte())))
		} else {
			buff := make([]byte, 1)
			count, err := os.Stdin.Read(buff)


      if err == io.EOF {
			  push(tagNum(int(-1)))
      } else if nil != err {
				panic(err)
			} else if count != 1 {
				panic("Failed to read 1char")
			} else {
			  push(tagNum(int(buff[0])))
      }

		} // )@@
	case 19: // @@(primitive (%%putchar x)
		doPrim1(func(x Obj) Obj {
			fmt.Printf("%c", byte(x.Value()))
			return x
		}) //)@@
// )@@
	}

	return tagNum(0)
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

			for {
			  code := proc.Field0()

			  if code.Number() {
			  	pop(); // @@(feature arity-check)@@

			  	ret := prim(code.Value())

					if ret.Rib() {
						proc = ret
						continue
					}

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

			  	nargs := pop().Value() // @@(feature arity-check)@@

			  	c2 := allocRib(tagNum(0), proc, tagNum(PairTag))
			  	s2 := c2

			  	arityNumber := code.Field0().Value()
			  	nparams := arityNumber >> 1

			  	// @@(feature arity-check
			  	{
			  		var shouldCrash bool
			  		if arityNumber & 1 == 1 {
			  			shouldCrash = nparams > nargs
			  		} else {
			  			shouldCrash = nparams != nargs
}
			  		if shouldCrash {
			  			panic(fmt.Sprintf("Arity mismatch: expected %d, got %d, arityNumber=%d", nparams, nargs, arityNumber))
			  		}
			  	}
			  	// )@@

			  	// @@(feature rest-param (use arity-check)
			  	nargs -= nparams
			  	if arityNumber & 1 == 1 {
			  		rest := Obj(NIL)
			  		for nargs > 0 {
			  			rest = allocRib(pop(), rest, tagNum(PairTag))
			  			nargs--
			  		}

			  		s2 = allocRib(rest, s2, tagNum(PairTag))
			  	}
			  	// )@@

			  	for nparams > 0 {
			  		nparams--
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
			  break
		  }
		case InstrSet: // set
			if DebugICall {
				fmt.Println("--- set")
			}
	    x := stack.Field0()

			if pc.Field1().Number() {
				listTail(stack, pc.Field1()).Field0Set(x)
			} else {
				pc.Field1().Field0Set(x)
			}
	    stack = stack.Field1()
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
			return
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
