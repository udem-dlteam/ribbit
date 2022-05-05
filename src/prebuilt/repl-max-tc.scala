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
    private val program = "R.rdadac,>,=>,qssa,oludom,htgnel-rotcev,?=<rahc,?qe,rahc-etirw,?orez,etacnurt,xam,?=gnirts,!llif-rotcev,!tes-gnirts,gnirtsbus,regetni>-rahc,htgnel-gnirts,gniliec,tel,fer-rotcev,?=>gnirts,raaaac,?=>rahc,rddaac,dna,?=rahc,!tes,=,raddac,?<rahc,+,rotaremun,mcl,rotcev-ekam,enifed,!llif-gnirts,rahc>-regetni,?>rahc,qmem,rdaaac,adbmal,?naeloob,raaddc,?=<gnirts,?evitisop,gnirts-ekam,radaac,rotanimoned,!rdc-tes,raadac,raaadc,rddadc,ro,fi,etouq,nim,roolf,?evitagen,!tes-rotcev,ypoc-gnirts,dnuor,radddc,nigeb,radadc,=<,dnoc,rdaddc,rdaadc,rddddc,dneppa-gnirts,*,?ddo,?<gnirts,pam,vmem,?>gnirts,tsil>-rotcev,tpxe,gnirts>-rebmun,esrever,!rac-tes,fer-gnirts,redniamer,cossa,rebmun>-gnirts,lave,rebmem,vssa,?neve,hcae-rof,dcg,lobmys>-gnirts,cc/llac,gnirts>-lobmys,raac,-,radc,raadc,raaac,rdddac,enilwen,rdadc,rdaac,fer-tsil,radac,raddc,rdddc,?tcejbo-foe,lper,rorre,rotcev>-tsil,dneppa,?erudecorp,sba,?llun,yalpsid,etirw,htgnel,?lobmys,daer,tneitouq,rahc-keep,?lauqe,?rotcev,rddac,gnirts>-tsil,ton,,tsil>-gnirts,,?gnirts,<,,?regetni,rddc,,,,,,rdac,,,rahc-daer,,?riap,,snoc,rac,rdc,,?vqe,,,,,;9%@MX#Z5\'j%!U3^z!U3i$]$8U3YTA9%@Z-@YKa@YLiU?@YL`y{!/9$i&iU;y]%9%@Z-@YKZ<^\'i$~Z&^YH@YT<vCvR3y]<7#YU5^z!U58TMi&:HiU>ai&kkz!U>:kw\'k!TO\'_,YTOaA_B^~E^{!T>\'^8T>YT8lbA`^\'`~?_B_~E_|!TF8TL`^YT>ka_AaB`1YTFdAbCai$B`^~E_|!U6#`kn8>^~i$#`kn8>^~i$#`kn8>^~i$#`kn8>^~YT9U^~?w)O^~?kR^~YT9^z!U/#YU6a_l{!TL#a_k#k_k~?iU>_{!T71b1:YT7fAdbw)k~EAaB`^|!TB1YT7:h-w6k1YT7f~?iU>fdCaaa^}(!TK,i&^z!TN,YTK`^{!TH,YTNb`^|!T;,YTHca_wS.|!1#b`n8TBfCi&CbwU<awU<`8TFCea_`~YI_A`1ci$1cYT;CPdxPCAJcwS%BJa1cCAJbwS%~?BJbwU=~EAa_~?xP^1ci$1cM^1cMYTHYT;CPgwS/wU<wU<YTKYTN`wU<wT#~EPbJa~EAa_~?wS/^1ci%1cM^1cMYT;i$CPdwSK^~EPbJa~EAa_~?wSK^8TBfPdJ`B_`BJ`~?wT#^8T7cAa_~?wS%^#YTLewTA#d~YMbYTMi&:YT7iU>PeYTOCCfi$i$akYJ_nJ`~?wS;^1:HgZ.ecHfYCdboJa_~?wS.^1YU/dYT>lbJbYCa_~M?wSA_8TBfCi&CbwU<awU<`8TFCea_`~YI_A`1ci$1cYT;CPdxPCAJcwS%BJa1cCAJbwS%~?BJbwU=~EAa_~?xP^1ci$1cM^1cMYTHYT;CPgwS/wU<wU<YTKYTN`wU<wT#~EPbJa~EAa_~?wS/^1ci%1cM^1cMYT;i$CPdwSK^~EPbJa~EAa_~?wSK^8TBfPdJ`B_`BJ`~?wT#^8T7cAa_~?wS%^#YTLewTA#d~YMbYTMi&:YT7iU>PeYTOCCfi$i$akYJ_nJ`~?wS;^1:HgZ.ecHfYCdboJa_~?wS.^1YU/dYT>lbJbYCa_~M^~^?wSI^#cJan~?wS-^B_~E_#bYT>k``m~YI_|!T<5_@L^{]-5uy!T./5^~Q^z!@\'i$8@aA_@L^8@aA_@L^@LvS#~M?vS#_8@aA_@L^8@aA_@L^@LvS#~M^~^?vE^8@aA_@LvS;@LvS#~?t^8@aA_@LvS9@LvS#~?v0^8@aA_@LvS5@LvS#~?u^8@aA_@L^~YA`B^~E^{!U$\'i$\'i$8U$A^@YKB^~E^@LvC~E^z!L8LZC^8T<vS7vF~YO^8KZE^@LvF~YD^8@i$V^~T^8LZ4^~YI^5vL@YU$A^@YKB^@LvK~E^8T<vLvK~YM^8T<vS;vF~?i%^8T<vS-vF~YA^z!K8L^5vE@Wi%V^@LvE~T^z!U&8U&8TD~?u^\'^~Ik^Gy!TD8TD@G\'^8U&~?vR0^~I_vC\'iU:~Z&^YFy!T:8T:C`^8T:Ca^8T:Cat~?vS;^8T:Cav0~?vS9^8T:Cau~?vS5^G~?vS#^9B_~?vE^\'i&~Z&^Gz!TC,YTC^@G\'i&~MMIvD`,YTC^@G\'i&~MM^~^?vL_,YTC^@G\'i&~M^~^?vK^YFy!U%,YU%^YH\'i&@G~?vL^YTDy!7\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~i$\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~IvR<^~I_vR587DDvR%_K`v3@G~i$\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~i$\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~IvR<^~I_vR587DDvR%_K`v3@G~IvR/^~I_vR$YFz!H96`\'^~^^Z=^YBCYTC^@G8BYT:i&@G~?vE^,Ci&YHwS-@G~?vJ^9#YH-Nkk87k@G~?vP^YF@G~M?vRM_9#YH-Nkk87k@G~?vP^YF@G~M^~^?vS?^\'i%@G~?vS;^\'i$@G~?vS-^YF@G~?vF^8U%@G~?vK^\'^~Ik^YTDy!F\'^!U8^Gy!U-\'^!U8iU@\'^~?iU:^!U8^z!08U-^8U-YU2~?iU@^\'^~?iU:^iU8y!U8iU@]&(iU:^z]5/7%YTA\'_@YU7Uc^@YT=Rc^OOYTAi$zOOYTAi$~YO^z]8/\'i$98A`^@X$B_~E_~YO^{]H/\'i&,ZHAa_X$B_~E_~YO^{!TM#l`^{!OYT@l!T)/8T=YT?aO_^~YD^{!SB9#YT?k^z!S)/8U\'b`R^~YD^|!SP/9*`R^~YD^{!T1/88^~YD^z]E/8;^~YD^z]##oYJ_^z!DYT@o!S@/8T=YT?aO_^~i$/8T=YT?aO_^~Q_~T^{!S(8BYPi&V^z]L8BYPV`V^{!U(8Ba8U(CfZ@bb`a_Dl`~Sa_}\'!T\'8U(i&b`^|!U#\'k\'iU:~E_\'l8U#AbA`\'l~I`^\'iU:~I__B`B^~E_~E^{!TP/8U#V`V^~i$/8U#V`V^~T_~T^{!SO8AZI`^{!S88AZF`^{]F2YTPa_k{]I2kYTP`^{!T*(kYTP`^{!S68BYT?vC^z!T(/8U\'b`R^~T^|]@/9*`R^~T^{!T%/88^~T^z!?/8;^~T^z!U*\'i%\'i$8U*A_~Q^B^~E^z!B/#nYJ_^~YU*^z!=YT@n!S?i\'!T&i\'!SMiT4!T0iS#!S>iT5!SFi<!SJi(!U)\'_\'i$\'i$8U)DDvR%`KbuA_~IvR/^~I_vR$B^~E^{!U18U)k^\'i$~YM^z]=/\'i$-_k~^YU1^8U1A^~?vPB^\'i$~YM^V^~T^z!TI\'^8TI_`~IakCb^YT8DKu``vR%YGu^{]C8BYTIi&^8BCYTIi&D`kvP~Sk^z]D\'^4__~ZJ`ZDYGm`ZK_^\'l~?k_{!S\'i\'!T,i\'!T$i\'!S+i\'!S4\'lz!SDi\'!SC4_YTGZ7``_YN`YN^\'k~?k_{!TE8TE_Z?__\'_~?k^{]78TE`^8TE__~I__YN`YN^{!T28T8b^\'^~?IkbIk`\'k~?k^DK`a_YG`^{]?-KYGb``^{!N\'^-_k~Sk^z!S,\'_\'^~S`^{!T+\'^\'_~S`^{]J8AZ9^z]9(KYGm`m^z!S*8<k^z!S78<_kz!T-(k^z!T48AS`^{!S#8AS__{!T58<__{!</2`^~i$/2`^~Q_~Q^{!SH/(`^~i$/(`^~Q_~Q^{!G/8TG`^9$_iUA~?k_~i$/8TG`^9$_iUA~?k_~Q_~Q^{]K/4`^~i$/4`^~Q_~Q^{]2/-`^~i$/-`^~Q_~Q^{!SE/8T8`^~i$/8T8`^~Q_~Q^{!:8AYT9^z@YU.ki#!U9Oi#!U+\'^!U9CiU9^YU4^8U+Oa_\'^~YE`O^R_~E_{]6/8U+iU9^~T^z]4/88^~YI^z!U4#m_i$z!IYT@m!U0\'`8U0Cca`Dl^~S_k|!T?8U0i&`^{]>\'i$9>Aa_\'^~YEB__B_~E_{!T3j:]:\'i$9:Aa_\'^~?B__B_~E_{];\'i$9;A`^\'_~YEB`^~E_{!S=jG]G\'i$9GA`^\'_~?B`^~E_{!TJ\'^8TJDl`A^~S`k{!U\'9AaYTJ`^|]*+YTJ`^{!U,\'_8U,CaB_A^~E^{]B8U,i&^z!P\'_,YPaA_B^~E^{!J\'k8T8YJA_l~E^z!M(i&^z]M9\'A^z!S&9\'B^z]O9(A^z!S99(B^z!S09,A^z!S$9,B^z]N90A^z!S190B^z!SG8CB^z!T69)A^z!S29)B^z!SL9+A^z!S59+B^z!S<9/A^z!SN9/B^z]\'89A^z](89B^z],91A^z]091B^z])3B^z]+93A^z]/93B^z]1*B^z]3+B^z].8CA^z!C3A^z!9*A^z!3+A^z!S3/8U.`^~E^{]A/8T=`^~E^{!*/88^~E^z!+/8;^~E^z!,#k`^{!.YT@k!E\'i$\'i$\'i$\'i$8ERaR_~YEOaO_~YEUaU_~YT9`\'i$~?pU_~YT9_\'^~^?`^{!T/i(!S:8A_\'^~^?i%^z!A(i$^z!T@8TA\'i$(bU^~YT9^zz!TGiG!4jK!-j2!T8iSE!2i<!U?:nl:ki&vC!U;:nv1:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS9vS6vS9vS9vS,vCvS,vS7vS@vS;vCvMvMvM!U@Z2mk!U:Z2lk!UA:nv2:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vR%vCvS@vS)vCvS,vS+vS0vS=vS0vS+vCvMvMvM!\':lkl!):lkm!6:lkn!TA:lko!T9:lkp!;:lkq!8:lkr!>:lks!T=:lkt!U.:lku!U7:lkv.!(:lkv/!<:lkv0!SE:lkv1]2:lkv2]K:lkv3!G:lkv4!U2:lkv5!5:lkv6y"

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