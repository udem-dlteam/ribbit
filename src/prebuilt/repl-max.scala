import scala.concurrent.ExecutionContext
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import scala.sys.exit

enum ObjectType(val code: Int) {
    case Pair extends ObjectType(0)
    case Procedure extends ObjectType(1)
    case Symbol extends ObjectType(2)
    case String extends ObjectType(3)
    case Vector extends ObjectType(4)
    case SpecialValue extends ObjectType(5)
}

enum Opcode(val code: Int) {
    case JumpCall extends Opcode(0)
    case Set extends Opcode(1)
    case Get extends Opcode(2)
    case Const extends Opcode(3)
    case If extends Opcode(4)
}

/**
 * Represents one of the two value types that can be used within ribs.
 */
type RibField = Int | Rib

extension (f: RibField) {
    def asRib = f.asInstanceOf[Rib]
    def asInt = f.asInstanceOf[Int]
    def isRib = f.isInstanceOf[Rib]
    def isInt = f.isInstanceOf[Int]    
}

/**
 * Constant-size unit of memory used by the RVM.
 * Each field is either an integer or a reference to a rib.
 */
case class Rib(var x: RibField, var y: RibField, var z: RibField) {
    private var id =
        val idTemp = Rib.ribCounter
        Rib.ribCounter += 1
        idTemp
    
    override def toString: String =
        def toIntOrHashCode(v: RibField) = v match
            case v: Int => v
            case v: Rib => "["+v.hashCode+"]"
            "Rib<" + hashCode + ">("
                + toIntOrHashCode(x) + ", "
                + toIntOrHashCode(y) + ", "
                + toIntOrHashCode(z) + ")"

    override def hashCode: Int = id    
}

object Rib {
    private var ribCounter = 0

    def createSpecialValue() =
        Rib(0, 0, ObjectType.SpecialValue.code)
}

object Rvm {
    // By default, prints "HELLO!"  
    private val program = "R\'rdadac,>,=>,qssa,oludom,htgnel-rotcev,?=<rahc,?qe,rahc-etirw,?regetni,?orez,etacnurt,xam,?=gnirts,!llif-rotcev,!tes-gnirts,gnirtsbus,regetni>-rahc,htgnel-gnirts,gniliec,tel,fer-rotcev,?=>gnirts,raaaac,?=>rahc,rddaac,dna,?=rahc,!tes,=,raddac,?<rahc,rotaremun,mcl,rotcev-ekam,enifed,!llif-gnirts,rahc>-regetni,?>rahc,qmem,rdaaac,adbmal,?naeloob,raaddc,?=<gnirts,?evitisop,gnirts-ekam,radaac,rotanimoned,!rdc-tes,raaadc,raadac,rddadc,ro,rorre,fi,nim,roolf,?evitagen,etouq,!tes-rotcev,cc/llac,ypoc-gnirts,dnuor,radddc,nigeb,radadc,=<,dnoc,rdaddc,rdaadc,rddddc,dneppa-gnirts,?ddo,tixe,?<gnirts,pam,vmem,?>gnirts,?erudecorp,tsil>-rotcev,tpxe,gnirts>-rebmun,esrever,?rotcev,!rac-tes,fer-gnirts,redniamer,cossa,rebmun>-gnirts,lave,rebmem,vssa,?neve,hcae-rof,dcg,lobmys>-gnirts,gnirts>-lobmys,raac,radc,raadc,raaac,rdddac,lper,?gnirts,rdadc,rdaac,fer-tsil,radac,raddc,rdddc,?tcejbo-foe,enilwen,rotcev>-tsil,dneppa,+,sba,?lobmys,?llun,yalpsid,etirw,htgnel,daer,rahc-keep,?lauqe,rddac,tneitouq,,gnirts>-tsil,tsil>-gnirts,ton,,,rddc,,,*,,rdac,,,,rac,?riap,rahc-daer,<,-,rdc,snoc,,?vqe,,,,,;9)!S,9Fl@YN@YF_@YGiU7@YG^{])9)@YN@YFZ6^8N~YO^YD@YT8vCvR3y]67#YU.^z!U.8THi&:HiU6ai&kkz!U6:kw\'k!TJ\'_*YTJaB_G^~F^{!T9\'^8T9YKlbB`^\'`~?_G_~F_|!TA8TG`^YT9ka_BaG`1YTAdBbAai$G`^~F_|!U/#`kn8:^~i$#`kn8:^~i$#`kn8:^~i$#`kn8:^~YT5Q^~?w)I^~?kJ^~YT5^z!U\'#YU/a_l{!TG#a_k#k_k~?iU6_{!?1b1:VfBdbw)k~FBaG`^|!T<1V:h-w7k1Vf~?iU6fdAaaa^}(!TF*i&^z!TI*YTF`^{!TB*YTIb`^|!T7*YTBca_wS+|!1#b`n8T<fAi&AbwU4awU4`8TAAea_`~YI_B`1ci$1cYT7APdxLABKcxOGKa1cABKbxO~?GKbwU5~FBa_~?xL^1ci$1cN^1cNYTBYT7APgwS-wU4wU4YTFYTI`wU4wSN~FPbKa~FBa_~?wS-^1ci%1cN^1cNYT7i$APdwSH^~FPbKa~FBa_~?wSH^8T<fPdK`G_`GK`~?wSN^8?cBa_~?xO^#YTGewT?#d~YHbYTHi&:ViU6PeYTJAAfi$i$akYE_nK`~?wS9^1:HgZ*ecHfYAdboKa_~?wS+^1YU\'dYT9lbKbYAa_~N?wS?_8T<fAi&AbwU4awU4`8TAAea_`~YI_B`1ci$1cYT7APdxLABKcxOGKa1cABKbxO~?GKbwU5~FBa_~?xL^1ci$1cN^1cNYTBYT7APgwS-wU4wU4YTFYTI`wU4wSN~FPbKa~FBa_~?wS-^1ci%1cN^1cNYT7i$APdwSH^~FPbKa~FBa_~?wSH^8T<fPdK`G_`GK`~?wSN^8?cBa_~?xO^#YTGewT?#d~YHbYTHi&:ViU6PeYTJAAfi$i$akYE_nK`~?wS9^1:HgZ*ecHfYAdboKa_~?wS+^1YU\'dYT9lbKbYAa_~N^~^?wSF^#cKan~?wS\'^G_~F_#bYT9k``m~YI_|!T85_@L^{!N5uy!T,i5!;\'i$8;aB_@L^8;aB_@L^@LvS#~N?vS#_8;aB_@L^8;aB_@L^@LvS#~N^~^?vE^8;aB_@LvS;@LvS#~?t^8;aB_@LvS9@LvS#~?v0^8;aB_@LvS5@LvS#~?u^8;aB_@L^~S`G^~F^{!TL\'i$\'i$8TLB^@YFG^~F^@LvC~F^z!G8GZ>^8T8vS7vF~ZA^8FZ@^@LvF~Z<^8;i$T^~Z(^8GZ/^~YI^5vL@YTLB^@YFG^@LvK~F^8T8vLvK~YH^8T8vS;vF~?i%^8T8vS-vF~S^z!F8G^5vE@Ri%T^@LvE~Z(^z!TN8TN8T>~?u^\'^~Dk^Ey!T>8T>@E\'^8TN~?vR0^~D_vC\'iU8~YO^YCy!T68T6A`^8T6Aa^8T6Aat~?vS;^8T6Aav0~?vS9^8T6Aau~?vS5^E~?vS#^9=_~?vE^\'i&~YO^Ez!T=*YT=^@E\'i&~NNDvD`*YT=^@E\'i&~NN^~^?vL_*YT=^@E\'i&~N^~^?vK^YCy!TM*YTM^YD\'i&@E~?vL^YT>y!8\'_88CCvRL_M`v3@E~i$\'_88CCvRL_M`v3@E~DvS.^~D_vS\'88CCvR,_M`v3@E~i$\'_88CCvRL_M`v3@E~i$\'_88CCvRL_M`v3@E~DvS.^~D_vS\'88CCvR,_M`v3@E~DvR<^~D_vR588CCvR%_M`v3@E~i$\'_88CCvRL_M`v3@E~i$\'_88CCvRL_M`v3@E~DvS.^~D_vS\'88CCvR,_M`v3@E~i$\'_88CCvRL_M`v3@E~i$\'_88CCvRL_M`v3@E~DvS.^~D_vS\'88CCvR,_M`v3@E~DvR<^~D_vR588CCvR%_M`v3@E~DvR/^~D_vR$YCz!D90`\'^~^^Z7^UAYT=^@E8>YT6i&@E~?vE^*Ai&YDwS\'@E~?vJ^8MYD,Okk88k@E~?vP^YC@E~N?vRM_8MYD,Okk88k@E~?vP^YC@E~N^~^?vS?^\'i%@E~?vS;^\'i$@E~?vS-^YC@E~?vF^8TM@E~?vK^\'^~Dk^YT>y!C\'^!U1^Ey!U&\'^!U1iU3\'^~?iU8^!U1^z!.8U&^8U&YU+~?iU3^\'^~?iU8^iU1y!U1iU3!O(iU8^z!S%7%YT?\'_@YU0Qc^@YTDJc^IIYT?i$zIIYT?i$z]2\'i$92B`^@X$G_~F_{]D\'i&*ZDBa_X$G_~F_{!TH#l`^{]AYT:l!T&8TDYT;aI_^{!S@8MYT;k^z!S&8TOb`J^|!SM9%`J^{!T/i2]@i3!M#oYE_^z]<YT:o!S>8TDYT;aI_^{!S$8>YLi&T^z]H8>YLT`T^{!TP8>a8TPAfZ:bb`a_Cl`~Da_}\'!T$8TPi&b`^|!U*\'k\'iU8~F_\'l8U*BbB`\'l~D`^\'iU8~D__G`G^~F_~F^{!TK8U*T`T^{!SL8<ZE`^{!S68<ZB`^{]B-YTKa_k{]E-kYTK`^{!T\'(kYTK`^{!S48>YT;vC^z!T%8TOb`J^|]:9%`J^{!SPi2!=i3!>#nYE_^z](YT:n!S=i\'!T#i\'!SJiT2!T.jM!S<iT3!SCi-!SGi(!U#\'_\'i$\'i$8U#CCvR%`MbuB_~DvR/^~D_vR$G^~F^{!U)8U#k^\'i$~YH^z]7\'i$,_k~^YU)^8U)B^~?vPG^\'i$~YH^T^z!TC\'^8TC_`~DakAb^YKCMu``vR%Wu^{]>8>YTCi&^8>AYTCi&C`kvP~Dk^z]?\'^6__~ZG`Z?Wm`M_^\'l~?k_{!S#i\'!T)i\'!SOi\'!S)i\'!S2\'lz!SBi\'!SA6_WZ1``_YJ`YJ^\'k~?k_{!T@8T@_Z9__\'_~?k^{]18T@`^8T@__~D__YJ`YJ^{!T08Kb^\'^~?DkbDk`\'k~?k^CM`a_W`^{]9,MWb``^{!J\'^,_k~Dk^z!S*\'_\'^~D`^{!T(\'^\'_~D`^{]G8<Z3^z]3(MWm`m^z!S(-k^z!S5-_kz!T*(k^z!T28<D`^{]M8<D__{!T3-__{!SEi(!T+8<YT5^z@YU,ki#!U2Ii#!U$\'^!U2AiU2^YU-^8U$Ia_\'^~YB`I^J_~F_{]08U$iU2^z]/i2!U-#m_i$z!IYT:m!U(\'`8U(Aca`Cl^~D_k|!T;8U(i&`^{]8\'i$98Ba_\'^~YBG__G_~F_{!T1j4]4\'i$94Ba_\'^~?G__G_~F_{]5\'i$95B`^\'_~YBG`^~F_{!S;jC]C\'i$9CB`^\'_~?G`^~F_{!TE\'^8TECl`B^~D`k{!TO9;aYTE`^|]%0YTE`^{!U%\'_8U%AaG_B^~F^{]=8U%i&^z!L\'_*YLaB_G^~F^{!E\'k8KYEB_l~F^z!H(i&^z]I8PI^z]P8PJ^z]K9#I^z!S79#J^z!S.9\'I^z]N9\'J^z]J9,I^z!S09,J^z!SD8AJ^z!T49$I^z!S/9$J^z!SI9&I^z!S39&J^z!S:9+I^z!SK9+J^z!P89I^z]#89J^z]\'9-I^z],9-J^z]$4J^z]&9.I^z]+9.J^z]-2J^z].3J^z]*8AI^z!A4I^z!92I^z!43I^z!S1iU,];iTD!+i2!0i3!*#k`^{!/YT:k!B\'i$\'i$\'i$\'i$8BJaJ_~YBIaI_~YBQaQ_~YT5`\'i$~?pQ_~YT5_\'^~^?`^{!T-i(!S88<_\'^~^?i%^z!<(i$^z!T:8T?\'i$(bQ^~YT5^zz!U7:nl:ki&vC!U3Cmk!U8Clk!\':lkl!):lkm!7:lkn!T?:lko!T5:lkp!3:lkq!2:lkr!::lks!TD:lkt!U,:lku!U0:lkv.!(:lkv/!-:lkv0!K:lkv1!,:lkv2!6:lkv3!@:lkv4!U+:lkv5!5:lkv6]F:lkv7y"

    private val DEBUG = false
    private val NB_OF_PRIMITIVES = 20

    private val falseRib = Rib.createSpecialValue()
    private val trueRib  = Rib.createSpecialValue()
    private val nilRib   = Rib.createSpecialValue()

    private var pos = 0
    private var symtable = nilRib    
    private var topOfStack: RibField = 0
    private var pc = Rib(0,0,0)

    private var inputBuffer = ""

    def main(args: Array[String]) =
        decodeSymbolTable()
        decodeInstructionGraph()
        setupGlobals()
        topOfStack = Rib(0, 0, Rib(5,0,0)) // Primordial continuation
        run()

    private def getByte() =
        val byte = program.charAt(pos).toByte
        pos += 1
        byte

    private def getCode() =
        val x = getByte() - 35
        if x < 0 then 57 else x
    
    private def getInt(n: Int): Int =
        val x = getCode()
        val n2 = n * 46
        if x < 46 then n2 + x else getInt(n2 + x - 46)
    
    private def listTail(rf: RibField, i: Int): RibField =           
        if (i == 0) rf else listTail( rf.asRib.y, i - 1)      
    
    private def decodeSymbolTable(): Unit =
        // Symbols with empty names
        val nbEmptySyms = getInt(0)          
        for _ <- 0 to (nbEmptySyms - 1) do
            symtable = Rib(Rib(0, Rib(nilRib, 0, 3), 2), symtable, 0)
        
        // Building the symbol table
        var n = 0
        var c: Byte = 0
        var accum = nilRib
        while c != 59 do
            c = getByte()              
            if c == 44 then
                symtable = Rib(Rib(0, Rib(accum, n, 3), 2), symtable, 0)
                accum = nilRib
                n = 0
            else if c != 59 then
                accum = Rib(c, accum, 0)
                n += 1

        symtable = Rib(Rib(0, Rib(accum, n, 3), 2), symtable, 0)

    private def decodeInstructionGraph(): Unit =
        def symbolRef(n: Int): RibField =
            listTail(symtable, n).asRib.x

        var m: RibField = 0

        var continue = true        
        while continue do
            val x = getCode()
            val shortEncodings = Array(20, 30, 0, 10, 11, 4)
            var n = x
            var d = 0
            var op = -1
            m = 0

            while
                op += 1
                d = shortEncodings(op)
                d + 2 < n
            do
                n -= d + 3
              
            if x > 90 then
                m = pop()
            else
                if op == 0 then
                    topOfStack = Rib(0, topOfStack, 0)
                    op+=1
                
                m = if n == d then getInt(0)
                    else if n > d then symbolRef(getInt(n - d - 1))
                    else if op < 3 then symbolRef(n)
                    else n
                
                if op > 4 then
                    m = Rib(Rib(m, 0, pop()), 0, 1)                    
                    if topOfStack == 0 then                                          
                        continue = false
                    else
                        op = 4
            
            if continue then              
                topOfStack.asRib.x = Rib(op-1, m, topOfStack.asRib.x)

        pc = m.asRib.x.asRib.z.asRib
        
    private def setupGlobals(): Unit =
        def setGlobal(r: Rib): Unit =
            symtable.x.asRib.x = r
            symtable = symtable.y.asRib
        
        setGlobal(Rib(0, symtable, ObjectType.Procedure.code))
        setGlobal(falseRib)
        setGlobal(trueRib)
        setGlobal(nilRib)       
    
    /*
     * [Execution]
     */
    private def pop(): RibField =        
        val popped = topOfStack.asRib.x        
        topOfStack = topOfStack.asRib.y
        popped

    private def push(x: RibField): Unit =        
       topOfStack = Rib( x , topOfStack, 0)

    private def getOpnd(rf: RibField): RibField =        
        if(rf.isRib) rf.asRib
        else listTail(topOfStack.asRib, rf.asInt)

    private def getCont(): Rib = 
        var s = topOfStack.asRib        
        while{ (s.z.isInt && s.z.asInt == 0) && s.y.isRib } do
            s = s.asRib.y.asRib        
        s

    private def putChar(c: RibField): RibField =         
        print( c.asInt.toChar )
        c

    private def toBool(b: Boolean): Rib =       
        if(b) trueRib else falseRib

    private def prim1( f:(RibField) => RibField ): Unit = 
        push( f( pop() ) )
    
    private def prim2( f:(RibField, RibField) => RibField ): Unit = 
        push( f( pop(), pop() ) )

    private def prim3( f:(RibField, RibField, RibField) => RibField ): Unit =        
        push( f( pop(), pop(), pop() ) )

    private def primitives(no: Int): Unit = no match {
        case 0 => prim3( (x, y, z) => Rib(z, y, x) ) // Rib
        case 1 => prim1( (rf: RibField) => rf )      // ID
        case 2 => pop() // Pop
        case 3 => {     // Skip
            val y = pop()
            pop()
            push(y)
        }
        case 4  => push( Rib( pop().asRib.x, topOfStack, 1) ) // Close
        case 5  => prim1( (x: RibField) => toBool(x.isRib) )  // isRib ?
        case 6  => prim1( (x: RibField) => x.asRib.x ) // Field0
        case 7  => prim1( (x: RibField) => x.asRib.y ) // Field1
        case 8  => prim1( (x: RibField) => x.asRib.z ) // Field2
        case 9  => prim2( (y: RibField, x: RibField) => { // Set Field0(x)
            x.asRib.x = y
            y
        })
        case 10 => prim2( (y: RibField, x: RibField) => { // Set Field1(y)
            x.asRib.y = y
            y
        }) 
        case 11 => prim2( (y: RibField, x: RibField) => { // Set Field2(z)
            x.asRib.z = y
            y
        }) 
        case 12 => prim2( (y: RibField, x: RibField) => toBool( if(x.isRib && y.isRib) x.asRib eq y.asRib else x == y ) ) // Equal    
        case 13 => prim2( (y: RibField, x: RibField) => toBool( x.asInt < y.asInt ) ) // Less than
        case 14 => prim2( (y: RibField, x: RibField) => x.asInt + y.asInt ) // Add
        case 15 => prim2( (y: RibField, x: RibField) => x.asInt - y.asInt ) // Sub
        case 16 => prim2( (y: RibField, x: RibField) => x.asInt * y.asInt ) // Mult
        case 17 => prim2( (y: RibField, x: RibField) => if(y.asInt != 0) x.asInt/y.asInt else 0 ) // Div         
        case 18 => { // GetChar
            if inputBuffer == "" then
                inputBuffer = readLine()
                if inputBuffer != null then
                    inputBuffer += "\n"
                else
                    inputBuffer = ""
            
            if inputBuffer == "" then
                push(-1)
            else
                val retChar = inputBuffer.charAt(0)
                inputBuffer = inputBuffer.substring(1)
                push(retChar)
        }
        case 19 => prim1( putChar )// PutChar
        case 20 => { exit() }
    }
 
    private def run(): Unit = {  
        var o = pc.y      
        val instr = pc.x.asInt   
        var nextPC = true

        instr match{ 
            case 5 => return // Halt
            case 0 => {      // Jump/Call    
                o = getOpnd(o).asRib.x                   
                var c = o.asRib.x   
               
                if( c.isRib ){                    
                    var c2 = Rib(0,o,0)                   
                    var s2 = c2    
                    var nargs = c.asRib.x.asInt                   
                    while(nargs > 0)                        
                        s2 = Rib( pop(), s2, 0)                       
                        nargs -= 1 
                                                         
                    if(pc.z.isRib){    
                        c2.x = topOfStack
                        c2.z = pc.z  
                    }
                    else{                        
                        var k = getCont()                       
                        c2.x = k.x
                        c2.z = k.z  
                    }    
                                
                    topOfStack = s2
                }
                else{                                             
                    if(c.asInt <= NB_OF_PRIMITIVES) primitives(c.asInt) else return               
                                                             
                    if(pc.z.isRib) c = pc  
                    else{
                        c = getCont()
                        topOfStack.asRib.y = c.asRib.x                                                 
                    }                                                    
                }  
                
                pc = c.asRib.z.asRib  
                nextPC = false                                   
            }
            case 1 => getOpnd(o).asRib.x = pop() // Set    
            case 2 => // Get               
                val t = getOpnd(o) 
                val k = t.asRib.x               
                push(k)                  
            case 3 =>  push(o) // Const   
            case 4 =>          // If                            
                if
                    val popped = pop()                    
                    // Everything is true except for `falseRib`
                    !( popped.isRib && (popped.asRib eq falseRib ) )                   
                then  
                    pc = pc.y.asRib  
                    nextPC = false             
        }  

        if(nextPC) pc = pc.z.asRib  
        run() // Tail recursion  
    }   
}