package main

import (
	"fmt"
	"os"
)

const DebugICall = false
const HaltVmCode = 6
const Input = "#etouq,,fi,,!rdc-tes,,esrever,?erudecorp,enifed,,,!rac-tes,,,!tes,,,,adbmal,rddc,lobmys>-gnirts,cossa,fer-tsil,gnirts>-lobmys,,gnirts>-tsil,,tneitouq,qssa,?gnirts,,enilwen,ton,rebmun>-gnirts,lobmys-denretninu>-gnirts,lave,,rdddac,,,*,,?lobmys,,,,,,,,daer,,etirw,,,rahc-keep,,,?llun,htgnel,tsil>-gnirts,,+,,-,,,,,,?lauqe,,rddac,rdac,,<,,rahctup,=,rahc-daer,,?riap,snoc,rac,?qe,,rdc,,,,,,,;9']'9'@Z3@YLZ/YN@TvCvR3y]/94]4Z8^z]88Gi&>GjOai&kkz]O>kw(k!C(_.YCaA_D^~F^{!;(^8;YBlbA`^(`~C_D_~F_|!J8HAaRk_D`0YJdAbEai$D`^~F_|]:'`ko89^~i$'`ko89^~i$'`ko89^~Cw)M^~ClK^~YA^z]$'Z:a_m{!H'a_l'k_k~CjO_{!0'b`o8JEdDbAa_'YHewS#'d~YFbYGi&>GjOOeYCEEfi$i$akYE_oN`~Cx@^0>GgZ-ecGfOdbpNa_~CxP^0Z$dRlbNbOa_~CxJ^8JEdDbAa_'YHewS#'d~YFbYGi&>GjOOeYCEEfi$i$akYE_oN`~Cx@^0>GgZ-ecGfOdbpNa_~CxP^0Z$dRlbNbOa_~i%~CxD^'cNao~CwS$^D_~F_'bRk``n~Z(_|!=3_@J^{]33uy]%3^3^@Z%_~L`kYBWZ*u``vR%Z7u^z!M(i$8MA^@JD^~F^z]#(i$(i$9#A^@YLD^~F^@JvC~F^z!L9%^8MYDZ;^~Z(^3vE@YMYD^@JvE~Z5^3vL@Z#A^@YLD^@JvK~F^8=vLvK~YF^8=vS;vF~Ci%^8=vS-vF~Z2^z])9)8>~Iu^(^~Lk^Hy!>8>@H(^9)~IvR0^~L_vC(^~Lk^YIy!<.S^@H(i&~LvD^.S^@H(i&~i%~IvL^.S^@H(i&~i%~IvK^YIy]&.Z&^YN(i&@H~IvL^Uy!N9>_(^~^Z1^Z9ES^@H.Ei&YNwS$@H~IvJ^(i%@H(i$@H~IvS-^YI@H~IvF^9&@H~IvK^(^~Lk^Uy!I(^(^@YK_jFYO~IjA^KjFy!1(^@YKjAjF8O~IjA^KjFy]F>kkjA]AWmk]+(_9+EaD_A^~F^{]L9+i&^z]=(i$9=Aa_(^~QD__D_~F_{]6(i$96Aa_(^~CD__D_~F_{!E(k8BYEA_l~F^z]<-^9<Wl`A^~L`k{!:(i$(i$(i$(i$8:P`P^~QM`M^~QK`K^~YA_~YA^(i%~C`^{].(^!#>ki#^Z0^9.Ma_(^~Q`M^K_~F_{]>9.i#^z!P(_(i$(i$8PYBWvR%`Z*buA_~LvR/^~L_vR$D^~F^{]18PkYD^z!2,`^{!F,i&^z]I9,`^{]B4^z];6^z]0'n_kz](8?n^z!D4^z]9'mk^z]58?m^z]C6^z]M4^z!G'l`^{]K8?l^z]2,i$^z]-88A^z!887A^z]?*A^z!7-A^z]N9,`^{]G8K`^{!*6^z!-4^z!.'k`^{!/8?k^z!?(i$,`P^~YA^{!3Bv6!OBv5]7Bv4]*Bv3!@Bv2!BBv1!5Bv0!,Bv/]EBv.],Bu!KBt!9Bs!6Br!4Bq!ABp!S#Bo]HBn!)Bm!(Bl!+'lk^zy"

// ===============================================
// ===============================================
//                 OBJ + CLUMP
//                   + prim
// ===============================================
// ===============================================

type prim1 func(x Obj) Obj
type prim2 func(x, y Obj) Obj
type prim3 func(x, y, z Obj) Obj

type Obj interface {
	Number() bool
	Clump() bool
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

type Clump struct {
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

func (num *Num) Clump() bool {
	return false
}

func (num *Num) Field0() Obj {
	panic("Not a clump")
}

func (num *Num) Field1() Obj {
	panic("Not a clump")
}

func (num *Num) Field2() Obj {
	panic("Not a clump")
}

func (num *Num) Field0Set(obj Obj) Obj {
	panic("Not a clump")
}

func (num *Num) Field1Set(obj Obj) Obj {
	panic("Not a clump")
}

func (num *Num) Field2Set(obj Obj) Obj {
	panic("Not a clump")
}

func (num *Num) Value() int {
	return num.x
}

func (num *Num) Add(v int) Obj {
	r := num.x + v
	return tagNum(r)
}

func (clump *Clump) Number() bool {
	return false
}

func (clump *Clump) Clump() bool {
	return true
}

func (clump *Clump) Field0() Obj {
	return clump.x
}

func (clump *Clump) Field1() Obj {
	return clump.y
}

func (clump *Clump) Field2() Obj {
	return clump.z
}

func (clump *Clump) Field0Set(obj Obj) Obj {
	clump.x = obj
	return obj
}

func (clump *Clump) Field1Set(obj Obj) Obj {
	clump.y = obj
	return obj
}

func (clump *Clump) Field2Set(obj Obj) Obj {
	clump.z = obj
	return obj
}

func (clump *Clump) Value() int {
	panic("Not a number")
}

func (clump *Clump) Add(int) Obj {
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

var FALSE *Clump = nil
var TRUE *Clump = nil
var NIL *Clump = nil

func push(val Obj) {
	tos := new(Clump)
	tos.x = val
	tos.y = stack
	tos.z = tagNum(0)

	stack = tos
}

func pop() Obj {
	x := stack.Field0()
	stack = stack.Field1()
	return x
}

func allocClump(car, cdr, tag Obj) Obj {
	push(car)
	allocated := stack

	oldStack := allocated.Field1()
	stack = oldStack

	allocated.Field1Set(cdr)
	allocated.Field2Set(tag)

	return allocated
}

func createSym(name Obj) Obj {
	inner := allocClump(name, tagNum(0), tagNum(2))
	outer := allocClump(tagNum(0), inner, tagNum(3))
	return allocClump(outer, symbolTable, tagNum(0))
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

		accum = allocClump(tagNum(c), accum, tagNum(0))
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
			n = pop()
		} else {
			if op == 0 {
				stack = allocClump(
					tagNum(0),
					stack,
					tagNum(0))
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
				inner := allocClump(n, tagNum(0), pop())
				n = allocClump(inner, NIL, tagNum(1))

				if stack == nil || (stack.Number() && stack.Value() == 0) {
					break
				}
				op = 4
			}
		}

		stack.Field0Set(allocClump(tagNum(op), n, stack.Field0()))
	}

	pc = n.Field0().Field2()
}

func initConstantClumps() {

	init := func(x, y, z int) *Clump {
		cl := new(Clump)
		cl.x = tagNum(x)
		cl.y = tagNum(y)
		cl.z = tagNum(z)
		return cl
	}

	TRUE = init(0, 0, 4)
	FALSE = init(0, 0, 5)
	NIL = init(0, 0, 6)
}

func setupStack() {
	stack = allocClump(
		tagNum(0),
		tagNum(0),
		allocClump(tagNum(HaltVmCode),
			tagNum(0),
			tagNum(0)))

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
	case 0: // clump
		doPrim3(func(x, y, z Obj) Obj {
			return allocClump(x, y, z)
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
		z := tagNum(1)
		stack.Field0Set(allocClump(x, y, z))
	case 5:
		doPrim1(func(x Obj) Obj {
			return boolean(x.Clump())
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
			} else if x.Clump() && y.Clump() {
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

			if nil != err {
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

		switch instr {
		case 0: // jump
			fallthrough
		case 1: // call
			call := 1 == instr
			if DebugICall {
				if call {
					fmt.Println("--- call")
				} else {
					fmt.Println("--- jump")
				}
			}

			var newPc Obj

			jmpTarget := func() Obj {
				return getOperand(pc.Field1()).Field0()
			}

			if jmpTarget().Number() {
				prim(jmpTarget().Value())

				if call {
					newPc = pc
				} else {
					newPc = getCont()
					stack.Field1Set(newPc.Field0())
				}
			} else {
				if DebugICall {
					fmt.Printf("Calling a symbol\n")
				}
				argC := jmpTarget().Field0().Value()

				temp := allocClump(tagNum(0), tagNum(0), tagNum(0))
				temp.Field1Set(pc.Field1())
				temp.Field2Set(pc)
				pc = temp

				for a := 0; a < argC; a++ {
					temp = allocClump(pop(), tagNum(0), tagNum(0))
					temp.Field1Set(pc)
					pc = temp
				}

				s2 := pc
				c2 := listTail(s2, tagNum(argC))
				pc = c2.Field2()
				c2.Field2Set(tagNum(0))

				if pc.Field0().Number() && pc.Field0().Value() != 0 {
					c2.Field0Set(stack)
					c2.Field2Set(pc.Field2())
				} else {
					k := getCont()
					c2.Field0Set(k.Field0())
					c2.Field2Set(k.Field2())
				}

				stack = s2
				newPc = jmpTarget()
			}

			pc = newPc.Field2()
		case 2: // set
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
		case 3: // get
			if DebugICall {
				fmt.Println("--- get")
			}
			push(getOperand(pc.Field1()))
			pc = pc.Field2()
		case 4: // const
			if DebugICall {
				fmt.Println("--- const")
			}
			push(pc.Field1())
			pc = pc.Field2()
		case 5: // if
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
		case HaltVmCode:
			fmt.Println("Bye bye!")
			os.Exit(0)
		}
	}
}

func initVm() {
	initConstantClumps()
	buildSymTable()
	decode()

	setGlobal(symbolTable)
	setGlobal(FALSE)
	setGlobal(TRUE)
	setGlobal(NIL)
	setGlobal(allocClump(tagNum(0), NIL, tagNum(1)))

	setupStack()

	run()
}

func main() {
	initVm()
}
