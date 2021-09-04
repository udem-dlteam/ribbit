package main

const CLUMP_NB_FIELDS = 3

const IPT = "#etouq,,fi,,!rdc-tes,,esrever,?erudecorp,enifed,,,!rac-tes,,,!tes,,,,adbmal,rddc,lobmys>-gnirts,cossa,fer-tsil,gnirts>-lobmys,,gnirts>-tsil,,tneitouq,qssa,?gnirts,,enilwen,ton,rebmun>-gnirts,lobmys-denretninu>-gnirts,lave,,rdddac,,,*,,?lobmys,,,,,,,,daer,,etirw,,,rahc-keep,,,?llun,htgnel,tsil>-gnirts,,+,,-,,,,,,?lauqe,,rddac,rdac,,<,,rahctup,=,rahc-daer,,?riap,snoc,rac,?qe,,rdc,,,,,,,;9']'9'@Z3@YLZ/YN@TvCvR3y]/94]4Z8^z]88Gi&>GjOai&kkz]O>kw(k!C(_.YCaA_D^~F^{!;(^8;YBlbA`^(`~C_D_~F_|!J8HAaRk_D`0YJdAbEai$D`^~F_|]:'`ko89^~i$'`ko89^~i$'`ko89^~Cw)M^~ClK^~YA^z]$'Z:a_m{!H'a_l'k_k~CjO_{!0'b`o8JEdDbAa_'YHewS#'d~YFbYGi&>GjOOeYCEEfi$i$akYE_oN`~Cx@^0>GgZ-ecGfOdbpNa_~CxP^0Z$dRlbNbOa_~CxJ^8JEdDbAa_'YHewS#'d~YFbYGi&>GjOOeYCEEfi$i$akYE_oN`~Cx@^0>GgZ-ecGfOdbpNa_~CxP^0Z$dRlbNbOa_~i%~CxD^'cNao~CwS$^D_~F_'bRk``n~Z(_|!=3_@J^{]33uy]%3^3^@Z%_~L`kYBWZ*u``vR%Z7u^z!M(i$8MA^@JD^~F^z]#(i$(i$9#A^@YLD^~F^@JvC~F^z!L9%^8MYDZ;^~Z(^3vE@YMYD^@JvE~Z5^3vL@Z#A^@YLD^@JvK~F^8=vLvK~YF^8=vS;vF~Ci%^8=vS-vF~Z2^z])9)8>~Iu^(^~Lk^Hy!>8>@H(^9)~IvR0^~L_vC(^~Lk^YIy!<.S^@H(i&~LvD^.S^@H(i&~i%~IvL^.S^@H(i&~i%~IvK^YIy]&.Z&^YN(i&@H~IvL^Uy!N9>_(^~^Z1^Z9ES^@H.Ei&YNwS$@H~IvJ^(i%@H(i$@H~IvS-^YI@H~IvF^9&@H~IvK^(^~Lk^Uy!I(^(^@YK_jFYO~IjA^KjFy!1(^@YKjAjF8O~IjA^KjFy]F>kkjA]AWmk]+(_9+EaD_A^~F^{]L9+i&^z]=(i$9=Aa_(^~QD__D_~F_{]6(i$96Aa_(^~CD__D_~F_{!E(k8BYEA_l~F^z]<-^9<Wl`A^~L`k{!:(i$(i$(i$(i$8:P`P^~QM`M^~QK`K^~YA_~YA^(i%~C`^{].(^!#>ki#^Z0^9.Ma_(^~Q`M^K_~F_{]>9.i#^z!P(_(i$(i$8PYBWvR%`Z*buA_~LvR/^~L_vR$D^~F^{]18PkYD^z!2,`^{!F,i&^z]I9,`^{]B4^z];6^z]0'n_kz](8?n^z!D4^z]9'mk^z]58?m^z]C6^z]M4^z!G'l`^{]K8?l^z]2,i$^z]-88A^z!887A^z]?*A^z!7-A^z]N9,`^{]G8K`^{!*6^z!-4^z!.'k`^{!/8?k^z!?(i$,`P^~YA^{!3Bv6!OBv5]7Bv4]*Bv3!@Bv2!BBv1!5Bv0!,Bv/]EBv.],Bu!KBt!9Bs!6Br!4Bq!ABp!S#Bo]HBn!)Bm!(Bl!+'lk^zy"

type Obj interface {
	Number() bool
	Clump() bool
	Car() Obj
	Cdr() Obj
	Tag() Obj
	CarSet(Obj)
	CdrSet(Obj)
	TagSet(Obj)
	Value() int32
}

type Num struct {
	x int32
}

type Clump struct {
	x, y, z Obj
}

func numOf(x int32) Obj {
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

func (this *Num) CarSet(obj Obj) {
	panic("Not a clump")
}

func (this *Num) CdrSet(obj Obj) {
	panic("Not a clump")
}

func (this *Num) TagSet(obj Obj) {
	panic("Not a clump")
}

func (this *Num) Value() int32 {
	return this.x
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

func (this *Clump) Value() int32 {
	panic("Not a number")
}

func (this *Clump) CarSet(obj Obj) {
	this.x = obj
}

func (this *Clump) CdrSet(obj Obj) {
	this.y = obj
}

func (this *Clump) TagSet(obj Obj) {
	this.z = obj
}

var stack Obj = nil

func push(val Obj) {
	tos := new(Clump)
	tos.x = val
	tos.y = stack
	tos.z = numOf(0)

	stack = tos
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

func main() {
	println("Hello World!")
}
