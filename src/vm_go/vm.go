package main

import (
	"fmt"
	"os"
)

const DebugICall = true
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
	Car() Obj
	Cdr() Obj
	Tag() Obj
	CarSet(Obj) Obj
	CdrSet(Obj) Obj
	TagSet(Obj) Obj
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

func (this *Num) Number() bool {
	return true
}

func (this *Num) Clump() bool {
	return false
}

func (this *Num) Car() Obj {
	panic("Not a clump")
}

func (this *Num) Cdr() Obj {
	panic("Not a clump")
}

func (this *Num) Tag() Obj {
	panic("Not a clump")
}

func (this *Num) CarSet(obj Obj) Obj {
	panic("Not a clump")
}

func (this *Num) CdrSet(obj Obj) Obj {
	panic("Not a clump")
}

func (this *Num) TagSet(obj Obj) Obj {
	panic("Not a clump")
}

func (this *Num) Value() int {
	return this.x
}

func (this *Num) Add(v int) Obj {
	r := this.x + v
	return tagNum(r)
}

func (this *Clump) Number() bool {
	return false
}

func (this *Clump) Clump() bool {
	return true
}

func (this *Clump) Car() Obj {
	return this.x
}

func (this *Clump) Cdr() Obj {
	return this.y
}

func (this *Clump) Tag() Obj {
	return this.z
}

func (this *Clump) CarSet(obj Obj) Obj {
	this.x = obj
	return obj
}

func (this *Clump) CdrSet(obj Obj) Obj {
	this.y = obj
	return obj
}

func (this *Clump) TagSet(obj Obj) Obj {
	this.z = obj
	return obj
}

func (this *Clump) Value() int {
	panic("Not a number")
}

func (this *Clump) Add(int) Obj {
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

var stack Obj = nil
var symbolTable Obj = nil
var pc Obj = nil

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
	x := stack.Car()
	stack = stack.Cdr()
	return x
}

func allocClump(car, cdr, tag Obj) Obj {
	push(car)

	allocated := stack
	oldStack := allocated.Cdr()
	stack = oldStack

	allocated.CdrSet(cdr)
	allocated.TagSet(tag)

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
		return listTail(list.Cdr(), i.Add(-1))
	}
}

func listRef(list, i Obj) Obj {
	return listTail(list, i).Car()
}

func symbolRef(depth Obj) Obj {
	return listRef(symbolTable, depth)
}

func setGlobal(c Obj) {
	symbolTable.Car().CarSet(c)
	symbolTable = symbolTable.Cdr()
}

func getOperand(o Obj) Obj {
	if o.Number() {
		return listTail(stack, o).Car()
	} else {
		return o.Car()
	}
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
				stack = allocClump(tagNum(0), stack, tagNum(0))
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

		tos := stack.Car()
		stack.CarSet(allocClump(tagNum(op), n, tos))
	}

	pc = n.Car().Tag()
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
	stack = allocClump(tagNum(0), tagNum(0),
		allocClump(tagNum(HaltVmCode), tagNum(0), tagNum(0)))

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
		doPrim2(func(x, y Obj) Obj {
			return y
		})
	case 4:
		x := stack.Car().Car()
		y := stack.Cdr()
		z := tagNum(1)
		stack.CarSet(allocClump(x, y, z))
	case 5:
		doPrim1(func(x Obj) Obj {
			return boolean(x.Clump())
		})
	case 6:
		doPrim1(func(x Obj) Obj {
			return x.Car()
		})
	case 7:
		doPrim1(func(x Obj) Obj {
			return x.Cdr()
		})
	case 8:
		doPrim1(func(x Obj) Obj {
			return x.Tag()
		})
	case 9:
		doPrim2(func(x, y Obj) Obj {
			return x.CarSet(y)
		})
	case 10:
		doPrim2(func(x, y Obj) Obj {
			return x.CdrSet(y)
		})
	case 11:
		doPrim2(func(x, y Obj) Obj {
			return x.TagSet(y)
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
			// TODO
			panic("No read yet")
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

	for !s.Tag().Number() {
		s = s.Cdr()
	}

	return s
}

func run() {
	for {
		instr := pc.Car().Value()

		switch instr {
		case 0: // jump
			fallthrough
		case 1: // call
			call := 1 == instr
			if DebugICall {
				if call {
					fmt.Println("--call")
				} else {
					fmt.Println("--jump")
				}
			}
			var newPc Obj
			jmpTarget := getOperand(pc.Cdr()).Car()

			if jmpTarget.Number() {
				prim(jmpTarget.Value())

				if call {
					newPc = pc
				} else {
					newPc = getCont()
					stack.CdrSet(newPc.Car())
				}
			} else {
				argC := jmpTarget.Car().Value()

				temp := allocClump(tagNum(0), tagNum(0), tagNum(0))
				temp.CdrSet(pc.Cdr())
				temp.TagSet(pc)
				pc = temp

				for a := 0; a < argC; a++ {
					temp = allocClump(pop(), tagNum(0), tagNum(0))
					temp.CdrSet(pc)
					pc = temp
				}

				s2 := pc
				c2 := listTail(s2, tagNum(argC))
				pc = c2.Tag()
				c2.TagSet(tagNum(0))

				if pc.Car().Number() && pc.Car().Value() != 0 {
					c2.CarSet(stack)
					c2.TagSet(pc.Tag())
				} else {
					k := getCont()
					c2.CarSet(k.Car())
					c2.TagSet(k.Tag())
				}

				stack = s2
				newPc = jmpTarget
			}

			pc = newPc.Tag()
		case 2: // set
			if DebugICall {
				fmt.Println("--set")
			}
			x := pop()
			if pc.Cdr().Number() {
				listTail(stack, pc.Cdr()).CarSet(x)
			} else {
				pc.Cdr().CarSet(x)
			}
			pc = pc.Tag()
		case 3: // get
			if DebugICall {
				fmt.Println("--get")
			}
			push(getOperand(pc.Cdr()))
			pc = pc.Tag()
		case 4: // const
			if DebugICall {
				fmt.Println("--const")
			}
			push(pc.Cdr())
			pc = pc.Tag()
		case 5: // if
			if DebugICall {
				fmt.Println("--if")
			}
			p := pop()

			if p != FALSE {
				pc = pc.Cdr()
			} else {
				pc = pc.Tag()
			}
		default:
			fallthrough
		case HaltVmCode:
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
