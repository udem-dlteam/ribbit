/*
 * The Ribbit VM implementation in C
 */
#ifdef DEBUG_I_CALL
#define DEBUG
#endif

#ifdef DEBUG

#include <stdio.h>

#define PRINTLN()                                                              \
  do {                                                                         \
    printf("\n");                                                              \
  } while (0)

#endif

#ifndef NO_STD

#include <stdio.h>
#include <stdlib.h>

#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-pragmas"
#pragma ide diagnostic ignored "ConstantFunctionResult"

#ifdef DEFAULT_REPL_MIN

char *input =
    "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,"
    "enifed,!tes-rotcev,?rotcev,=,cc/"
    "llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,"
    "lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,"
    "lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,"
    "etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,"
    "htgnel,,,,,rddac,rdac,,-,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!"
    "K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/"
    ":kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~"
    "F_|!S+#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~RL^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_"
    "k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/"
    "NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#"
    "ZCex>#d~TbZBi&:EiS/"
    "NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?"
    "x=^G_~F_#bUk``m~YL_|!93_@J^{]%3uy]?'i$9?C_@J^G^~F^z]I'i$'i$9IC^@YGG^~F^@"
    "JvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^3vL@ZIC^@YGG^@JvK~F^89vLvK~T^"
    "89vS;vF~?i%^89vS-vF~Z$^z!G8E^3vE@Z?i%YD^@JvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'"
    "^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?"
    "vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?"
    "vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^"
    "z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Lc^@YS'Hc^BBZ>i$"
    "zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^"
    "z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FKKvR%`YNbuC_~IvR/"
    "^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$4_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~"
    "IakAb^YHKYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&K`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,"
    "AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Kl^~I_k|]"
    "L9Ji&`^{]A'^9AKl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!"
    "76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/"
    "Qk!A'i$'i$'i$'i$8ALaL_~YABaB_~YAHaH_~R`~R_'^~^?`^{]$(i$^z!:9>'i$(bL^~R^zz!"
    "S.Kmk!S0Klk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!5:lks!S':lkt!S):lku!"
    "S&:lkv.!(:lkv/!2:lkv0!H:lkv1!4:lkv2!N:lkv3]&:lkv4!S#:lkv5!3:lkv6y";

#else

char *input = "R.rdadac,>,=>,qssa,oludom,htgnel-rotcev,?=<rahc,?qe,rahc-etirw,?orez,etacnurt,xam,?=gnirts,!llif-rotcev,!tes-gnirts,gnirtsbus,regetni>-rahc,htgnel-gnirts,gniliec,tel,fer-rotcev,?=>gnirts,raaaac,?=>rahc,rddaac,dna,?=rahc,!tes,=,raddac,?<rahc,+,rotaremun,mcl,rotcev-ekam,enifed,!llif-gnirts,rahc>-regetni,?>rahc,qmem,rdaaac,adbmal,?naeloob,raaddc,?=<gnirts,?evitisop,gnirts-ekam,radaac,rotanimoned,!rdc-tes,raadac,raaadc,rddadc,ro,fi,etouq,nim,roolf,?evitagen,!tes-rotcev,ypoc-gnirts,dnuor,radddc,nigeb,radadc,=<,dnoc,rdaddc,rdaadc,rddddc,dneppa-gnirts,*,?ddo,?<gnirts,pam,vmem,?>gnirts,tsil>-rotcev,tpxe,gnirts>-rebmun,esrever,!rac-tes,fer-gnirts,redniamer,cossa,rebmun>-gnirts,lave,rebmem,vssa,?neve,hcae-rof,dcg,lobmys>-gnirts,cc/llac,gnirts>-lobmys,raac,-,radc,raadc,raaac,rdddac,enilwen,rdadc,rdaac,fer-tsil,radac,raddc,rdddc,?tcejbo-foe,lper,rorre,rotcev>-tsil,dneppa,?erudecorp,sba,?llun,yalpsid,etirw,htgnel,?lobmys,daer,tneitouq,rahc-keep,?lauqe,?rotcev,rddac,gnirts>-tsil,ton,,tsil>-gnirts,,?gnirts,<,,?regetni,rddc,,,,,,rdac,,,rahc-daer,,?riap,,snoc,rac,rdc,,?vqe,,,,,;9%@MX#Z5\'j%!U3^z!U3i$]$8U3YTA9%@Z-@YKa@YLiU?@YL`y{!/9$i&iU;y]%9%@Z-@YKZ<^\'i$~Z&^YH@YT<vCvR3y]<7#YU5^z!U58TMi&:HiU>ai&kkz!U>:kw\'k!TO\'_,YTOaA_B^~E^{!T>\'^8T>YT8lbA`^\'`~?_B_~E_|!TF8TL`^YT>ka_AaB`1YTFdAbCai$B`^~E_|!U6#`kn8>^~i$#`kn8>^~i$#`kn8>^~i$#`kn8>^~YT9U^~?w)O^~?kR^~YT9^z!U/#YU6a_l{!TL#a_k#k_k~?iU>_{!T71b1:YT7fAdbw)k~EAaB`^|!TB1YT7:h-w6k1YT7f~?iU>fdCaaa^}(!TK,i&^z!TN,YTK`^{!TH,YTNb`^|!T;,YTHca_wS.|!1#b`n8TBfCi&CbwU<awU<`8TFCea_`~YI_A`1ci$1cYT;CPdxPCAJcwS%BJa1cCAJbwS%~?BJbwU=~EAa_~?xP^1ci$1cM^1cMYTHYT;CPgwS/wU<wU<YTKYTN`wU<wT#~EPbJa~EAa_~?wS/^1ci%1cM^1cMYT;i$CPdwSK^~EPbJa~EAa_~?wSK^8TBfPdJ`B_`BJ`~?wT#^8T7cAa_~?wS%^#YTLewTA#d~YMbYTMi&:YT7iU>PeYTOCCfi$i$akYJ_nJ`~?wS;^1:HgZ.ecHfYCdboJa_~?wS.^1YU/dYT>lbJbYCa_~M?wSA_8TBfCi&CbwU<awU<`8TFCea_`~YI_A`1ci$1cYT;CPdxPCAJcwS%BJa1cCAJbwS%~?BJbwU=~EAa_~?xP^1ci$1cM^1cMYTHYT;CPgwS/wU<wU<YTKYTN`wU<wT#~EPbJa~EAa_~?wS/^1ci%1cM^1cMYT;i$CPdwSK^~EPbJa~EAa_~?wSK^8TBfPdJ`B_`BJ`~?wT#^8T7cAa_~?wS%^#YTLewTA#d~YMbYTMi&:YT7iU>PeYTOCCfi$i$akYJ_nJ`~?wS;^1:HgZ.ecHfYCdboJa_~?wS.^1YU/dYT>lbJbYCa_~M^~^?wSI^#cJan~?wS-^B_~E_#bYT>k``m~YI_|!T<5_@L^{]-5uy!T./5^~Q^z!@\'i$8@aA_@L^8@aA_@L^@LvS#~M?vS#_8@aA_@L^8@aA_@L^@LvS#~M^~^?vE^8@aA_@LvS;@LvS#~?t^8@aA_@LvS9@LvS#~?v0^8@aA_@LvS5@LvS#~?u^8@aA_@L^~YA`B^~E^{!U$\'i$\'i$8U$A^@YKB^~E^@LvC~E^z!L8LZC^8T<vS7vF~YO^8KZE^@LvF~YD^8@i$V^~T^8LZ4^~YI^5vL@YU$A^@YKB^@LvK~E^8T<vLvK~YM^8T<vS;vF~?i%^8T<vS-vF~YA^z!K8L^5vE@Wi%V^@LvE~T^z!U&8U&8TD~?u^\'^~Ik^Gy!TD8TD@G\'^8U&~?vR0^~I_vC\'iU:~Z&^YFy!T:8T:C`^8T:Ca^8T:Cat~?vS;^8T:Cav0~?vS9^8T:Cau~?vS5^G~?vS#^9B_~?vE^\'i&~Z&^Gz!TC,YTC^@G\'i&~MMIvD`,YTC^@G\'i&~MM^~^?vL_,YTC^@G\'i&~M^~^?vK^YFy!U%,YU%^YH\'i&@G~?vL^YTDy!7\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~i$\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~IvR<^~I_vR587DDvR%_K`v3@G~i$\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~i$\'_87DDvRL_K`v3@G~i$\'_87DDvRL_K`v3@G~IvS.^~I_vS\'87DDvR,_K`v3@G~IvR<^~I_vR587DDvR%_K`v3@G~IvR/^~I_vR$YFz!H96`\'^~^^Z=^YBCYTC^@G8BYT:i&@G~?vE^,Ci&YHwS-@G~?vJ^9#YH-Nkk87k@G~?vP^YF@G~M?vRM_9#YH-Nkk87k@G~?vP^YF@G~M^~^?vS?^\'i%@G~?vS;^\'i$@G~?vS-^YF@G~?vF^8U%@G~?vK^\'^~Ik^YTDy!F\'^!U8^Gy!U-\'^!U8iU@\'^~?iU:^!U8^z!08U-^8U-YU2~?iU@^\'^~?iU:^iU8y!U8iU@]&(iU:^z]5/7%YTA\'_@YU7Uc^@YT=Rc^OOYTAi$zOOYTAi$~YO^z]8/\'i$98A`^@X$B_~E_~YO^{]H/\'i&,ZHAa_X$B_~E_~YO^{!TM#l`^{!OYT@l!T)/8T=YT?aO_^~YD^{!SB9#YT?k^z!S)/8U\'b`R^~YD^|!SP/9*`R^~YD^{!T1/88^~YD^z]E/8;^~YD^z]##oYJ_^z!DYT@o!S@/8T=YT?aO_^~i$/8T=YT?aO_^~Q_~T^{!S(8BYPi&V^z]L8BYPV`V^{!U(8Ba8U(CfZ@bb`a_Dl`~Sa_}\'!T\'8U(i&b`^|!U#\'k\'iU:~E_\'l8U#AbA`\'l~I`^\'iU:~I__B`B^~E_~E^{!TP/8U#V`V^~i$/8U#V`V^~T_~T^{!SO8AZI`^{!S88AZF`^{]F2YTPa_k{]I2kYTP`^{!T*(kYTP`^{!S68BYT?vC^z!T(/8U\'b`R^~T^|]@/9*`R^~T^{!T%/88^~T^z!?/8;^~T^z!U*\'i%\'i$8U*A_~Q^B^~E^z!B/#nYJ_^~YU*^z!=YT@n!S?i\'!T&i\'!SMiT4!T0iS#!S>iT5!SFi<!SJi(!U)\'_\'i$\'i$8U)DDvR%`KbuA_~IvR/^~I_vR$B^~E^{!U18U)k^\'i$~YM^z]=/\'i$-_k~^YU1^8U1A^~?vPB^\'i$~YM^V^~T^z!TI\'^8TI_`~IakCb^YT8DKu``vR%YGu^{]C8BYTIi&^8BCYTIi&D`kvP~Sk^z]D\'^4__~ZJ`ZDYGm`ZK_^\'l~?k_{!S\'i\'!T,i\'!T$i\'!S+i\'!S4\'lz!SDi\'!SC4_YTGZ7``_YN`YN^\'k~?k_{!TE8TE_Z?__\'_~?k^{]78TE`^8TE__~I__YN`YN^{!T28T8b^\'^~?IkbIk`\'k~?k^DK`a_YG`^{]?-KYGb``^{!N\'^-_k~Sk^z!S,\'_\'^~S`^{!T+\'^\'_~S`^{]J8AZ9^z]9(KYGm`m^z!S*8<k^z!S78<_kz!T-(k^z!T48AS`^{!S#8AS__{!T58<__{!</2`^~i$/2`^~Q_~Q^{!SH/(`^~i$/(`^~Q_~Q^{!G/8TG`^9$_iUA~?k_~i$/8TG`^9$_iUA~?k_~Q_~Q^{]K/4`^~i$/4`^~Q_~Q^{]2/-`^~i$/-`^~Q_~Q^{!SE/8T8`^~i$/8T8`^~Q_~Q^{!:8AYT9^z@YU.ki#!U9Oi#!U+\'^!U9CiU9^YU4^8U+Oa_\'^~YE`O^R_~E_{]6/8U+iU9^~T^z]4/88^~YI^z!U4#m_i$z!IYT@m!U0\'`8U0Cca`Dl^~S_k|!T?8U0i&`^{]>\'i$9>Aa_\'^~YEB__B_~E_{!T3j:]:\'i$9:Aa_\'^~?B__B_~E_{];\'i$9;A`^\'_~YEB`^~E_{!S=jG]G\'i$9GA`^\'_~?B`^~E_{!TJ\'^8TJDl`A^~S`k{!U\'9AaYTJ`^|]*+YTJ`^{!U,\'_8U,CaB_A^~E^{]B8U,i&^z!P\'_,YPaA_B^~E^{!J\'k8T8YJA_l~E^z!M(i&^z]M9\'A^z!S&9\'B^z]O9(A^z!S99(B^z!S09,A^z!S$9,B^z]N90A^z!S190B^z!SG8CB^z!T69)A^z!S29)B^z!SL9+A^z!S59+B^z!S<9/A^z!SN9/B^z]\'89A^z](89B^z],91A^z]091B^z])3B^z]+93A^z]/93B^z]1*B^z]3+B^z].8CA^z!C3A^z!9*A^z!3+A^z!S3/8U.`^~E^{]A/8T=`^~E^{!*/88^~E^z!+/8;^~E^z!,#k`^{!.YT@k!E\'i$\'i$\'i$\'i$8ERaR_~YEOaO_~YEUaU_~YT9`\'i$~?pU_~YT9_\'^~^?`^{!T/i(!S:8A_\'^~^?i%^z!A(i$^z!T@8TA\'i$(bU^~YT9^zz!TGiG!4jK!-j2!T8iSE!2i<!U?:nl:ki&vC!U;:nv1:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS9vS6vS9vS9vS,vCvS,vS7vS@vS;vCvMvMvM!U@Z2mk!U:Z2lk!UA:nv2:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vR%vCvS@vS)vCvS,vS+vS0vS=vS0vS+vCvMvMvM!\':lkl!):lkm!6:lkn!TA:lko!T9:lkp!;:lkq!8:lkr!>:lks!T=:lkt!U.:lku!U7:lkv.!(:lkv/!<:lkv0!SE:lkv1]2:lkv2]K:lkv3!G:lkv4!U2:lkv5!5:lkv6y"; // RVM code that prints HELLO!

#endif

#ifndef NULL

#define NULL 0
typedef unsigned long size_t;

#endif

// basic def. of a boolean
typedef unsigned char bool;

#define true (1)

// an unsigned byte value for the REPL's code
typedef unsigned char byte;

// a tagged value
typedef unsigned long obj;

// a number
typedef long num;

// a rib obj
#define RIB_NB_FIELDS 3
typedef struct {
  obj fields[RIB_NB_FIELDS];
} rib;

#define EXIT_ILLEGAL_INSTR 6
#define EXIT_NO_MEMORY 7

#define UNTAG(x) ((x) >> 1)
#define RIB(x) ((rib *)(x))
#define NUM(x) ((num)(UNTAG((num)(x))))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x))
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))
#define TAG_NUM(num) ((((obj)(num)) << 1) | 1)

#define PRIM1() obj x = pop()
#define PRIM2()                                                                \
  obj y = pop();                                                               \
  PRIM1()
#define PRIM3()                                                                \
  obj z = pop();                                                               \
  PRIM2()

#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]
#define TOS CAR(stack)

#define NUM_0 (TAG_NUM(0))

#define INSTR_AP 0
#define INSTR_SET 1
#define INSTR_GET 2
#define INSTR_CONST 3
#define INSTR_IF 4
#define INSTR_HALT 5

#define PAIR_TAG TAG_NUM(0)
#define CLOSURE_TAG TAG_NUM(1)
#define SYMBOL_TAG TAG_NUM(2)
#define STRING_TAG TAG_NUM(3)
#define SINGLETON_TAG TAG_NUM(5)

// the only three roots allowed
obj stack = NUM_0;
obj pc = NUM_0;
obj FALSE = NUM_0;

// global, but not a root, referenced
obj symbol_table = NUM_0;

size_t pos = 0;

rib *heap_start;

// GC
#define MAX_NB_OBJS 100000 // 48000 is minimum for bootstrap
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_mid (heap_bot + (SPACE_SZ))
#define heap_top (heap_bot + (SPACE_SZ << 1))

#ifdef NO_STD
#define vm_exit(code)                                                          \
  do {                                                                         \
    asm volatile("mov $0x01, %%eax\nmov %0, %%ebx\nint $0x80" : : "i"(code));  \
  } while (0)
#else
#define vm_exit(code)                                                          \
  do {                                                                         \
    exit((code));                                                              \
  } while (0)
#endif

#if defined(NO_STD) && !defined(NO_REG)
register obj *alloc asm("edi");
#else
obj *alloc;
#endif
obj *alloc_limit;
obj *scan;

#ifdef NO_STD

void *sys_brk(void *addr) {
  long ptr;

  asm volatile("mov %0, %%ebx\n"
               "mov $0x2d, %%eax\n"
               "int $0x80\n"
               : "=a"(ptr)
               : "g"((long)addr)
               : "ebx");

  return (void *)ptr;
}

#endif

void init_heap() {
#ifdef NO_STD
  heap_start = sys_brk((void *)NULL);
  void *new_brk = sys_brk((void *)heap_top);

  if (new_brk == heap_start) {
    vm_exit(EXIT_NO_MEMORY);
  }

#else
  heap_start = malloc(sizeof(obj) * (SPACE_SZ << 1));

  if (!heap_start) {
    vm_exit(EXIT_NO_MEMORY);
  }
#endif

  alloc = heap_bot;
  alloc_limit = heap_mid;
  stack = NUM_0;
}

// NULL is a pointer (0) but would represent NULL
// so it is never present in an obj field, and
// cannot be a number because it is even. This
// saves a couple of bytes v.s having STACK
// as the broken heart value
#define GC_COPIED_OBJ ((obj)NULL)

void copy() {
  obj o = *scan;
  // we sometime reference rib that are allocated in BSS,
  // we do not want to copy those
  if (IS_RIB(o)) {
    obj *ptr = RIB(o)->fields;
    obj field0 = ptr[0];
    obj copy;

    if (field0 == GC_COPIED_OBJ) {
      copy = ptr[1]; // copied, get new address
    } else {
      copy = TAG_RIB(alloc);
      *ptr++ = GC_COPIED_OBJ; // ptr points to CDR
      *alloc++ = field0;
      *alloc++ = *ptr++; // ptr points to TAG
      *alloc++ = *ptr;
      ptr[-1] = copy; // set forward ptr. Since it points to TAG, ptr[-1]
                      // rewrites the CDR
    }
    *scan = copy; // overwrite to new address.
  }
  scan++;
}

void gc() {
#ifdef DEBUG
  obj *from_space = (alloc_limit == heap_mid) ? heap_bot : heap_mid;

  size_t objc = alloc - from_space;
  printf("\t--GC %d -> ", objc);
#endif

  // swap
  obj *to_space = (alloc_limit == heap_mid) ? heap_mid : heap_bot;
  alloc_limit = to_space + SPACE_SZ;

  alloc = to_space;

  // root: stack
  scan = &stack;
  copy();

  // root: pc
  scan = &pc;
  copy();

  // root: false
  scan = &FALSE;
  copy();

  // scan the to_space to pull all live references
  scan = to_space;
  while (scan != alloc) {
    copy();
  }

#ifdef DEBUG

  objc = alloc - to_space;
  printf("%d\n", objc);

#endif
}

obj pop() {
  obj x = CAR(stack);
  stack = CDR(stack);
  return x;
}

void push2(obj car, obj tag) {
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;
  *alloc++ = stack;
  *alloc++ = tag;

  stack = TAG_RIB((rib *)(alloc - RIB_NB_FIELDS));

  if (alloc == alloc_limit) {
    gc();
  }
}

/**
 * Allocate a rib that is not kept on the stack (can be linked
 * from anywhere). The car and cdr can be live references to other
 * ribs.
 * @param car
 * @param cdr
 * @param tag
 * @return
 */
rib *alloc_rib(obj car, obj cdr, obj tag) {
  push2(car, cdr); // tag is set
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = TAG(allocated);
  TAG(allocated) = tag;

  stack = old_stack;

  return RIB(allocated);
}

rib *alloc_rib2(obj car, obj cdr, obj tag) {
  push2(car, tag); // tag is set
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = cdr;

  stack = old_stack;

  return RIB(allocated);
}

char get_byte() { return input[pos++]; }

num get_code() {
  num x = get_byte() - 35;
  return x < 0 ? 57 : x;
}

num get_int(num n) {
  num x = get_code();
  n *= 46;
  return x < 46 ? n + x : get_int(n + x - 46);
}

rib *list_tail(rib *lst, num i) {
  return (i == 0) ? lst : list_tail(RIB(lst->fields[1]), i - 1);
}

obj list_ref(rib *lst, num i) { return list_tail(lst, i)->fields[0]; }

obj get_opnd(obj o) {
  return (IS_NUM(o) ? list_tail(RIB(stack), NUM(o)) : RIB(o))->fields[0];
}

obj get_cont() {
  obj s = stack;

  while (!NUM(TAG(s))) {
    s = CDR(s);
  }

  return s;
}

#define TRUE (CAR(FALSE))
#define NIL (CDR(FALSE))

#ifdef DEBUG

void chars2str(obj o) {
  if (o != NIL) {
    printf("%c", (char)(NUM(RIB(o)->fields[0]) % 256));
    chars2str(RIB(o)->fields[1]);
  }
}

void sym2str(rib *c) { chars2str(RIB(c->fields[1])->fields[0]); }

void show_operand(obj o) {
  if (IS_NUM(o)) {
    printf("int %ld", NUM(o));
  } else if (TAG(o) == SYMBOL_TAG) {
    printf("sym ");
    sym2str(RIB(o));
  }
}

#endif

obj boolean(bool x) { return x ? CAR(FALSE) : FALSE; }

void prim(int no) {
  switch (no) {
  case 0: { // rib
    obj new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
    PRIM3();
    CAR(new_rib) = x;
    CDR(new_rib) = y;
    TAG(new_rib) = z;
    push2(new_rib, PAIR_TAG);
    break;
  }
  case 1: { // id
    PRIM1();
    push2(x, PAIR_TAG);
    break;
  }
  case 2: { // arg1
    pop();
    break;
  }
  case 3: { // arg2
    obj x = pop();
    pop();
    push2(x, PAIR_TAG);
    break;
  }
  case 4: { // close
    obj x = CAR(TOS);
    obj y = CDR(stack);
    TOS = TAG_RIB(alloc_rib(x, y, CLOSURE_TAG));
    break;
  }
  case 5: { // is rib?
    PRIM1();
    push2(boolean(IS_RIB(x)), PAIR_TAG);
    break;
  }
  case 6: { // field0
    PRIM1();
    push2(CAR(x), PAIR_TAG);
    break;
  }
  case 7: { // field1
    PRIM1();
    push2(CDR(x), PAIR_TAG);
    break;
  }
  case 8: { // field2
    PRIM1();
    push2(TAG(x), PAIR_TAG);
    break;
  }
  case 9: { // set field0
    PRIM2();
    push2(CAR(x) = y, PAIR_TAG);
    break;
  }
  case 10: { // set field1
    PRIM2();
    push2(CDR(x) = y, PAIR_TAG);
    break;
  }
  case 11: { // set field2
    PRIM2();
    push2(TAG(x) = y, PAIR_TAG);
    break;
  }
  case 12: { // eqv?
    PRIM2();
    push2(boolean(x == y), PAIR_TAG);
    break;
  }
  case 13: { // lt
    PRIM2();
    push2(boolean(NUM(x) < NUM(y)), PAIR_TAG);
    break;
  }
  case 14: { // add
    PRIM2();
    push2(x + y - 1, PAIR_TAG);
    break;
  }
  case 15: { // sub
    PRIM2();
    push2(x - y + 1, PAIR_TAG);
    break;
  }
  case 16: { // mul
    PRIM2();
    push2(TAG_NUM((NUM(x) * NUM(y))), PAIR_TAG);
    break;
  }
  case 17: { // div
    PRIM2();
    push2(TAG_NUM((NUM(x) / NUM(y))), PAIR_TAG);
    break;
  }
  case 18: { // getc
    int read;
#ifdef NO_STD
    asm volatile("push %%eax\n"
                 "mov $0x03, %%eax\n"    // sys_call
                 "mov $0, %%ebx\n"       // fd
                 "lea 0(%%esp), %%ecx\n" // ptr
                 "mov $1, %%edx\n"       // count
                 "int $0x80\n"
                 "pop %%eax\n"
                 : "=a"(read)
                 :
                 : "ebx", "ecx", "edx", "esi", "edi");
    read &= 0xFF;
#else
    read = getchar();
    if (EOF == read) read = -1;
#endif
    push2(TAG_NUM(read), PAIR_TAG);
    break;
  }
  case 19: { // putc
    PRIM1();
#ifdef NO_STD
    {
      asm volatile("mov %0, %%eax\n"
                   "push %%eax\n"
                   "mov $1, %%ebx\n"       // fd
                   "lea 0(%%esp), %%ecx\n" // ptr
                   "mov $1, %%edx\n"       // count
                   "mov $0x04, %%eax\n"    // sys_call
                   "int $0x80\n"
                   "pop %%eax\n"
                   :
                   : "g"((int)(NUM(x) & 0xFF))
                   : "eax", "ebx", "ecx", "edx", "esi", "edi");
    }
#else
    putchar((char)NUM(x));
    fflush(stdout);
#endif
    push2(x, PAIR_TAG);
    break;
  }
  case 20: { // exit
    PRIM1();
    vm_exit(NUM(x));
    break;
  }
  default: {
    vm_exit(EXIT_ILLEGAL_INSTR);
  }
  }
}

void run() {
#define ADVANCE_PC()                                                           \
  do {                                                                         \
    pc = TAG(pc);                                                              \
  } while (0)
  while (1) {
    num instr = NUM(CAR(pc));
    switch (instr) {
    default: { // error
      vm_exit(EXIT_ILLEGAL_INSTR);
    }
    case INSTR_HALT: { // halt
      vm_exit(0);
    }
    case INSTR_AP: // call or jump
    {
      bool jump = TAG(pc) == NUM_0;
#ifdef DEBUG_I_CALL
      printf(jump ? "--- jump " : "--- call ");
      show_operand(CDR(pc));
      PRINTLN();
#endif
#define proc (get_opnd(CDR(pc)))
#define code CAR(proc)
      if (IS_NUM(code)) {
        prim(NUM(code));

        if (jump) {
          // jump
          pc = get_cont();
          CDR(stack) = CAR(pc);
        }
        pc = TAG(pc);
      } else {
        num argc = NUM(CAR(code));
        // Use the car of the PC to save the new PC
        CAR(pc) = CAR(get_opnd(CDR(pc)));

        //        pop();

        obj s2 = TAG_RIB(alloc_rib(NUM_0, proc, PAIR_TAG));

        for (int i = 0; i < argc; ++i) {
          s2 = TAG_RIB(alloc_rib(pop(), s2, PAIR_TAG));
        }

        obj c2 = TAG_RIB(list_tail(RIB(s2), argc));

        if (jump) {
          obj k = get_cont();
          CAR(c2) = CAR(k);
          TAG(c2) = TAG(k);
        } else {
          CAR(c2) = stack;
          TAG(c2) = TAG(pc);
        }

        stack = s2;

        obj new_pc = CAR(pc);
        CAR(pc) = TAG_NUM(instr);
        pc = TAG(new_pc);
      }
      break;
    }
#undef code
#undef proc
    case INSTR_SET: { // set
#ifdef DEBUG_I_CALL
      printf("--- set ");
      show_operand(CDR(pc));
      PRINTLN();
#endif
      obj x = CAR(stack);
      ((IS_NUM(CDR(pc))) ? list_tail(RIB(stack), NUM(CDR(pc))) : RIB(CDR(pc)))
          ->fields[0] = x;
      stack = CDR(stack);
      ADVANCE_PC();
      break;
    }
    case INSTR_GET: { // get
#ifdef DEBUG_I_CALL
      printf("--- get ");
      show_operand(CDR(pc));
      PRINTLN();
#endif
      push2(get_opnd(CDR(pc)), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_CONST: { // const
#ifdef DEBUG_I_CALL
      printf("--- const ");
      show_operand(CDR(pc));
      PRINTLN();
#endif
      push2(CDR(pc), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_IF: { // if
#ifdef DEBUG_I_CALL
      printf("--- if");
      PRINTLN();
#endif

      obj p = pop();
      if (p != FALSE) {
        pc = CDR(pc);
      } else {
        pc = TAG(pc);
      }
      break;
    }
    }
  }
#undef ADVANCE_PC
}

rib *symbol_ref(num n) { return RIB(list_ref(RIB(symbol_table), n)); }

obj lst_length(obj list) {
  size_t l = 0;

  while (IS_RIB(list) && NUM(TAG(list)) == 0) {
    ++l;
    list = CDR(list);
  }

  return TAG_NUM(l);
}

rib *create_sym(obj name) {
  rib *list = alloc_rib(name, lst_length(name), STRING_TAG);
  rib *sym = alloc_rib(FALSE, TAG_RIB(list), SYMBOL_TAG);
  rib *root = alloc_rib(TAG_RIB(sym), symbol_table, PAIR_TAG);
  return root;
}

void build_sym_table() {
  num n = get_int(0);

  while (n > 0) {
    n--;
    symbol_table = TAG_RIB(create_sym(NIL));
  }

  obj accum = NIL;

  while (1) {
    byte c = get_byte();

    if (c == 44) {
      symbol_table = TAG_RIB(create_sym(accum));
      accum = NIL;
      continue;
    }

    if (c == 59)
      break;

    accum = TAG_RIB(alloc_rib(TAG_NUM(c), TAG_RIB(accum), PAIR_TAG));
  }

  symbol_table = TAG_RIB(create_sym(accum));
}

void set_global(obj c) {
  CAR(CAR(symbol_table)) = c;
  symbol_table = CDR(symbol_table);
}

void decode() {
  int weights[6] = {20, 30, 0, 10, 11, 4};

  obj n;
  int d;
  int op;

  while (1) {
    num x = get_code();
    n = x;
    op = -1;

    while (n > 2 + (d = weights[++op])) {
      n -= d + 3;
    }

    if (x > 90) {
      op = INSTR_IF;
      n = pop();
    } else {
      if (!op) {
        push2(NUM_0, NUM_0);
      }

      if (n >= d) {
        n = (n == d) ? TAG_NUM(get_int(0))
                     : TAG_RIB(symbol_ref(get_int(n - d - 1)));
      } else {
        n = (op < 3) ? TAG_RIB(symbol_ref(n)) : TAG_NUM(n);
      }

      if (op > 4) {
        n = TAG_RIB(
            alloc_rib(TAG_RIB(alloc_rib2(n, NUM_0, pop())), NIL, CLOSURE_TAG));
        if (stack == NUM_0) {
          break;
        }
        op = INSTR_CONST;
      } else if (op > 0) {
        op--;
      } else {
        op = 0;
      }
    }

    rib *c = alloc_rib(TAG_NUM(op), n, 0);
    c->fields[2] = TOS;
    TOS = TAG_RIB(c);
  }

  pc = TAG(CAR(n));
}

void setup_stack() {
  push2(NUM_0, PAIR_TAG);
  push2(NUM_0, PAIR_TAG);

  obj first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;

  CAR(first) = TAG_NUM(INSTR_HALT);
  CDR(first) = NUM_0;
  TAG(first) = PAIR_TAG;
}

#ifdef NOSTART
void _start() {
#else

void init() {
#endif
  init_heap();

  FALSE = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                            TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                            SINGLETON_TAG));

  build_sym_table();
  decode();

  set_global(
      TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG))); /* primitive 0 */
  set_global(FALSE);
  set_global(TRUE);
  set_global(NIL);

  setup_stack();

  run();
}

#ifndef NOSTART

int main() { init(); }

#endif

#pragma clang diagnostic pop
