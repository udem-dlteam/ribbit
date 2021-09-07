package main

import (
	"fmt"
	"io"
	"os"
)

const DebugICall = false
const HaltVmCode = 6

//goland:noinspection SpellCheckingInspection
const Input = "#?vqe,fer-gnirts,enifed,htgnel-gnirts,!tes-rav-labolg,!tes-rotcev,esrever,etouq,rddc,*,fer-rav-labolg,+,2gra_,vne-erudecorp,!tes-gnirts,reffub,cc/llac,<,edoc-erudecorp,!rdc-tes,tneitouq,-,foe,htgnel-rotcev,ytpme,fi,adbmal,fer-rotcev,liat,!tes,lobmys-denretninu>-gnirts,tsil>-rotcev,rahcteg_,?gnirts,!tes-2dleif_,enilwen,?rotcev,!rac-tes,?erudecorp,ton,qssa,lave,rebmun>-gnirts,lobmys>-gnirts,!tes-1dleif_,elipmoc,rotcev>-tsil,poon-neg,cossa,gnirts>-tsil,ngissa-neg,xua-rebmun>-gnirts,lper,xua-rahc-daer,tsil-daer,xua-esrever,tneitouq_,rdddac,tnemmoc-piks,?tcejbo-foe,gnirts>-lobmys,tsil-etirw,rebmun-etirw,fer-tsil,xua-lobmys>-gnirts,!tes-tsil,llac-pmoc,srahc-etirw,rahc-keep,?lobmys,liat-tsil,dnetxe,*_,erudecorp-ekam,?llun,lobmys-daer,?pmulc_,ecapsetihw-non-rahc-keep,tsil>-gnirts,esolc_,daer,etirw,?ecnatsni,pukool,?lauqe,+_,llac-neg,!tes-0dleif_,2rahctup,htgnel,2dleif_,rddac,-_,rdac,<_,pmoc,rahctup_,rahc-daer,?riap,=,1dleif_,0dleif_,rac,snoc,evitimirp,?vqe_,rdc,1gra_,ytitnedi_,pmulc_,lin_,eurt_,eslaf_,lbtmys_;92k]292k@ZCk@YClZ=l^(i$~Z+l^YDk@SmvCvR3y]=7$kZ9l^z]98Kmi&>LnjJai&kkz]J>kw(k!M(_-mYMmaAl_El^~Il^{!A(^8AnVlbAl`^(`~B_El_~Il_|]$'Umc``o8>ma^~i$'Umc``o8>ma^~HmElYFlZ*l_vS&~YOl^YAnkcaAl_El^AlaEl`5nZ$ndAlbDmai$El`^~Il_|]7'`ko8:^~i$'`ko8:^~i$'`ko8:^~Bw)G^~BlF^~YH^z]4'Z7la_m{!>'a_l'k_k~BjJ_{!5'b`o9$nDmDmRlbea_`Al`'UmewE'd~YJlbYKmi&>LnjJPleYMmDmDmfi$i$akRl_oNl`~BxL^5n>LngZ-lecLnfPldbpNla_~BxM^5nZ4mdYAnlbNlbPla_~BwS6^9$nDmDmRlbea_`Al`'UmewE'd~YJlbYKmi&>LnjJPleYMmDmDmfi$i$akRl_oNl`~BxL^5n>LngZ-lecLnfPldbpNla_~BxM^5nZ4mdYAnlbNlbPla_~i%~BxI^'cNlao~BwS1^El_~Il_'bYAnk``n~YOl_|!<4_@K^{]C4uy](4^4^@Z(l_~M`kVOYLu``vR%Z.u^z]#(i$9#lAl^@KEl^~Il^z])(i$(i$9)lAl^@YClEl^~Il^@KvC~Il^z!C9(l^4vS*@SmvS6vS9@SmvS7vF~Z@l^8ClZGl^@KvF~ZBl^9#lYFlZ*l^~YOl^4vE@Z#lYFl^@KvE~ZEl^4vL@Z)lAl^@YClEl^@KvK~Il^8<mvLvK~YJl^8<mvS;vF~Bi%^8<mvS-vF~Z?l^z],9,k8Gk~Hmu^(^~Mk^Jky!G8Gk@Jk(^9,k~HmvR0^~M_vC88lk~Z+l^YPky!I-mYIk^@Jk(i&~MvD^-mYIk^@Jk(i&~i%~HmvL^-mYIk^@Jk(i&~i%~HmvK^YPky]0-mZ0k^YDk(i&@Jk~HmvL^YGky!D9;l_(^~^Z<l^Z5lDmYIk^@Jk-mDmi&YDkwS1@Jk~HmvJ^98lYDk(i%@Jk~HmvS;^(i$@Jk~HmvS-^YPk@Jk~HmvF^90k@Jk~HmvK^(^~Mk^YGky!P(^@T_iS)Jky]1(^@TjNiS)(^~HmjP^@T_iS)z!391l^91lZF~HmjN^(^~HmjP^FiS)y!S)>kkjN]NOmk]++jP^z]POlk!S(7&lYE(_@ZDQc^@TFc^GGYEi$zGGYEi$z]/(_9/mDmaEl_Al^~Il^{!S29/mi&^z]6(i$96mAla_(^~WmEl__El_~Il_{]>(i$9>mAla_(^~BEl__El_~Il_{!;(k8?RlAl_l~Il^z!N(^8NmOl`Al^~M`k{]%9AmaYNm`^|]'.lYNm`^{!@(i$(i$(i$(i$8@mQ`Q^~WmG`G^~WmF`F^~YH_~YH^(i%~B`^{]&(^!#>ki#^ZHl^9&mGa_(^~Wm`G^F_~Il_{];9&mi#^z]3(_(i$(i$93mVOvR%`YLbuAl_~MvR/^~M_vR$El^~Il^{]<93mkYFl^z!S$9.`^{!S/8L`^{!S#88`^{!S-8?`^{!S'6`^{!1+`^{!S8+`^{!J+i&^z!S48=`^{!S./^z]*0^z]H'o_i$z!OYBlo!S*9%nb`F^|!S79'm`F^{!S50^z!F/^z]5'nRl_^z]EYBln!S39%nb`F^|]K9'm`F^{]O0^z]G/^z]8'mRl_^z]BYBlm!S+0^z!S&/^z!K'l`^{]@YBll]?+i$^z]-89lAl^z!987lAl^z!S0*lAl^z!7.lAl^z!S%9:`^{]A8=`^{!*0^z!./^z!-'k`^{!2YBlk!B8E(i$+bQ^~YH^zz!4Clv6]FClv5].Clv4!LClv3!8Clv2!?Clv1!6Clv0!+Clv/]DClv.]:Clu!=Clt!:Cls!0Clr!/Clq!HClp!EClo!S,Cln!)Clm!(Cll!,'lk^zy"

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
	tos.z = tagNum(0)

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
	list := allocRib(chars, lstLength(chars), tagNum(3))
	sym := allocRib(FALSE, list, tagNum(4))
	return allocRib(sym, symbolTable, tagNum(0))
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

		accum = allocRib(tagNum(c), accum, tagNum(0))
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
				stack = allocRib(tagNum(0), stack, tagNum(0))
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
				n = allocRib(inner, NIL, tagNum(1))

				if stack == nil || (stack.Number() && stack.Value() == 0) {
					break
				}
				op = 4
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

	TRUE = init(0, 0, 4)
	FALSE = init(0, 0, 5)
	NIL = init(0, 0, 6)
}

func setupStack() {
	stack = allocRib(
		tagNum(0),
		tagNum(0),
		allocRib(tagNum(HaltVmCode),
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
		z := tagNum(1)
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

				newCont := stack
				argC := pop().Value()

				newCont.Field1Set(proc)

				c2 := allocRib(tagNum(0), proc, tagNum(0))
				s2 := c2

				for argC > 0 {
					argC--
					s2 = allocRib(pop(), s2, tagNum(0))
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
	initConstantRibs()
	buildSymTable()
	decode()

	setGlobal(symbolTable)
	setGlobal(FALSE)
	setGlobal(TRUE)
	setGlobal(NIL)
	setGlobal(allocRib(tagNum(0), tagNum(0), tagNum(1)))

	setupStack()

	run()
}

func main() {
	initVm()
}
