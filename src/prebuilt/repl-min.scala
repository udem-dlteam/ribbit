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
    private val program = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,enilwen,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8L!L8L@YJ@YGZ$^8J~YN^YC@PvCvR3y]$7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw\'k]@\'_*Z@aC_G^~F^{!>\'^8>YHlbC`^\'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YM_|!94_@K^{!J4uy]?\'i$9?C_@K^G^~F^z]I\'i$\'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YK^8EZ)^~YM^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z%^z!G8E^4vE@Z?YD^@KvE~YK^z]O9O8@~?u^\'^~Ik^Dy!@8@@D\'^9O~?vR0^~I_vC\'iS0~YN^YFy!?*V^@D\'i&~OOIvD`*V^@D\'i&~OO^~^?vL_*V^@D\'i&~O^~^?vK^YFy]M*ZM^YC\'i&@D~?vL^Wy!C9*`\'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC\'i%@D~?vS;^\'i$@D~?vS-^YF@D~?vF^9M@D~?vK^\'^~Ik^Wy!F\'^!S-^Dy]H\'^!S-iS.\'^~?iS0^!S-^z!-9H^9HYS#~?iS.^\'^~?iS0^iS-y!S-iS.!N(iS0^z]27%Z>\'_@YS&Jc^@YS\'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-9#`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];9#`H^{]<i+!Di1!B#nS_^z!KQn]F\'_\'i$\'i$9FLLvR%`YObuC_~IvR/^~I_vR$G^~F^{]G9Fk^\'i$~T^z!S%\'i$5_k~^ZG^9GC^~?vPG^\'i$~T^YD^z]E\'^9E_`~IakAb^YHLYOu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P\'^!S,AiS,^YS$^9PBa_\'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!MQm]J\'`9JAca`Ll^~I_k|]L9Ji&`^{]A\'^9ALl`C^~I`k{]N9\'aZA`^|]#0ZA`^{!<\'k8HSC_l~F^z!=(i&^z!P87B^z!76B^z]/+B^z!61B^z]9iS)]\'iS\'!,i+!0i1!*#k`^{!/Qk!A\'i$\'i$\'i$\'i$8AHaH_~YABaB_~YAJaJ_~R`\'i$~?pJ_~R_\'^~^?`^{]%(i$^z!:9>\'i$(bJ^~R^zz!S.Lmk!S0Llk!\':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S\':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!O:lkv3]&:lkv4!S#:lkv5!4:lkv6y"

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