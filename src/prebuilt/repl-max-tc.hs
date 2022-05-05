{-# LANGUAGE LambdaCase,NoMonomorphismRestriction,Strict #-}
module Main where
import Control.Monad
import Data.Char
import Data.Foldable
import Data.IORef
import GHC.IO
import System.Environment
import System.IO
zb=newIORef
sa=readIORef
rb=writeIORef
yb aa t=(aa,t)
data Ha=A !Int | V !(IORef Z) deriving (Eq)
data Z=Z {nb:: !Ha,vb:: !Ha,mb:: !Ha} deriving (Eq)
class Za aa where e::aa->IO Ha
instance Za Ha where e=pure
instance Za Z where e=fmap V . zb
instance Za Int where e=e . A
u xa y ba=e=<<Z<$>e y<*>e ba<*>e xa
qa xa y ba=e=<<Z<$>e xa<*>e y<*>e ba
hb l (V c)=l<$>sa c
ib l (V c)=sa c>>=rb c . l
g=hb nb
ea=hb vb
j=hb mb
ca c f=ib (\la->la {nb=f}) c
pb c f=ib (\la->la {vb=f}) c
gb c f=ib (\la->la {mb=f}) c
pa=u (A 0)
sb=u (A 1)
oc=u (A 2)
cd=u (A 3)
gd=u (A 4)
jb=u (A 5) (A 0) (A 0) 
{-# NOINLINE ma #-}
ma=unsafePerformIO (e=<<jb)
{-# NOINLINE va #-}
va=unsafePerformIO (e=<<jb)
{-# NOINLINE eb #-}
eb=unsafePerformIO (e=<<jb)
kb=foldrM pa eb
kc ub=kb (ord<$>ub)>>=flip cd (length ub)
pc=(=<<) (oc (A 0)) . kc
ka [] b=([],b)
ka (ga:ra) b=let n=bc ga; b'=b * 46 in if n<46 then (ra,b'+n) else ka ra (b'+n-46)
bc ga=let f=ord ga-35 in if f<0 then 57 else f
type Ed=IO ()
x f=q>>=pa f>>=oa
h=q>>= \case V c->sa c>>= \(Z bd k s)->oa k>>pure bd
fa l=h>>=l>>=x
p l=flip (,)<$>h<*>h>>=uncurry l>>=x
tc l=(,,)<$>h<*>h<*>h>>=l>>=x
ic=fmap ord getChar `catchAny` const (pure (-1))
dd=do
 hc<-h>>=g
 q>>=sb hc>>=x
wc =
 [ tc (\(n,t,aa)->e $ Z aa t n)                     
 ,fa pure                                    
 ,void h                                     
 ,p (const pure)                                
 ,dd                                       
 ,fa (pure . (\case A s->ma; s->va))             
 ,fa g                                    
 ,fa ea                                    
 ,fa j                                    
 ,p $ ya ca                             
 ,p $ ya pb                             
 ,p $ ya gb                             
 ,p $ \y ba->ob (y == ba)                        
 ,p $ \(A y) (A ba)->ob (y < ba)               
 ,p $ na (+)                                 
 ,p $ na (-)                                 
 ,p $ na (*)                                 
 ,p $ na fd                                
 ,ic>>=x . A                       
 ,fa (\c@(A f)->putChar (chr f)>>pure c)            
 ]
ya l c f=l c f>>pure f
na l (A aa) (A t)=pure $ A (l aa t)
ob t=pure $ if t then va else ma
xc ad cc=kb . reverse=<<mapM pc (replicate cc "" <> wb ad)
wb ra=case span (/= ',') ra of
 (cb,"")->[reverse cb]
 (cb,k)->reverse cb : wb (drop 1 k)
ac d b=g=<<ab b d
ab= \case 0->pure; b->ea >=> ab (b-1)
ua d []=h
ua d (ga:k)=do
 let n=bc ga; (b,m,da)=qb n 0
 if n>90
 then do
  bb<-h
  i<-q
  g i>>=qa (m-1) bb>>=ca i
  ua d k
 else do
  m<-if m==0 then x (A 0)>>pure (m+1) else pure m
  (k',b)<-if b==da then pure (A<$>ka k 0) 
  else if b>=da then let (k',tb)=ka k (b-da-1) in yb k'<$>ac d tb 
  else if m<3 then yb k<$>ac d b 
  else pure (k,A b)
  b<-if 4<m then do
   t<-h>>=qa b (A 0)
   qa t (A 0) (A 1)
  else pure b
  i<-q
  case i of
   A tb->g b>>=j 
   V c->g i>>=qa (min 4 m-1) b>>=ca i>>ua d k'
qb b m=let da=[20,30,0,10,11,4]!!m in if 2+da<b then qb (b-(da+3)) (m+1) else (b,m,da)
wa d lc=g d>>=flip ca lc>>ea d
w r=do
 o<-ea r
 g r>>= \case
  A 0->do
   o<-ta o>>=g
   n<-g o
   case n of
    V c->do
     ia<-pa (A 0) o
     A yc<-g n
     sc<-foldrM (\s fb->h>>=flip pa fb) ia [1..yc] 
     j r>>= \case
      o@V {}->do
       i<-q
       ca ia i
       gb ia o
      A b->do
       ja<-xb
       ca ia=<<g ja
       gb ia=<<j ja
     oa sc
     j n>>=w
    A b->do
     wc!!b
     j r>>= \case
      V s->j r>>=w
      A s->do
       ja<-xb
       i<-q
       g ja>>=pb i
       j ja>>=w
  A 1->do
   join (ca<$>ta o<*>h)
   j r>>=w
  A 2->do
   ta o>>=g>>=x
   j r>>=w
  A 3->x o>>j r>>=w
  A 4->do
   bb<-h
   (if bb==ma then j else ea) r>>=w
  s->pure ()
ta (A b)=q>>=ab b
ta o=pure o
xb=q>>=lb
 where
 lb= \case
  c ->
   j c>>= \case
    A s->ea c>>=lb
    s->pure c
vc db=do
 let (fc,mc)=span (/= ';') db; ((nc,rc),jc)=(ka fc 0,drop 1 mc)
 d<-xc nc rc
 gc<-ua d jc
 d'<-wa d=<<sb (A 0) d 
 d''<-wa d' ma
 d'''<-wa d'' va
 wa d''' eb
 uc<-u (A 0) (A 5) (A 0)
 dc<-u uc (A 0) (A 0)
 oa dc
 pure gc
{-# NOINLINE i #-}
i=unsafePerformIO . zb $ A 0
q=sa i
oa=rb i
qc="R.rdadac,>,=>,qssa,oludom,htgnel-rotcev,?=<rahc,?qe,rahc-etirw,?orez,etacnurt,xam,?=gnirts,!llif-rotcev,!tes-gnirts,gnirtsbus,regetni>-rahc,htgnel-gnirts,gniliec,tel,fer-rotcev,?=>gnirts,raaaac,?=>rahc,rddaac,dna,?=rahc,!tes,=,raddac,?<rahc,+,rotaremun,mcl,rotcev-ekam,enifed,!llif-gnirts,rahc>-regetni,?>rahc,qmem,rdaaac,adbmal,?naeloob,raaddc,?=<gnirts,?evitisop,gnirts-ekam,radaac,rotanimoned,!rdc-tes,raadac,raaadc,rddadc,ro,fi,etouq,nim,roolf,?evitagen,!tes-rotcev,ypoc-gnirts,dnuor,radddc,nigeb,radadc,=<,dnoc,rdaddc,rdaadc,rddddc,dneppa-gnirts,*,?ddo,?<gnirts,pam,vmem,?>gnirts,tsil>-rotcev,tpxe,gnirts>-rebmun,esrever,!rac-tes,fer-gnirts,redniamer,cossa,rebmun>-gnirts,lave,rebmem,vssa,?neve,hcae-rof,dcg,lobmys>-gnirts,cc/llac,gnirts>-lobmys,raac,-,radc,raadc,raaac,rdddac,enilwen,rdadc,rdaac,fer-tsil,radac,raddc,rdddc,?tcejbo-foe,lper,rorre,rotcev>-tsil,dneppa,?erudecorp,sba,?llun,yalpsid,etirw,htgnel,?lobmys,daer,tneitouq,rahc-keep,?lauqe,?rotcev,rddac,gnirts>-tsil,ton,,tsil>-gnirts,,?gnirts,<,,?regetni,rddc,,,,,,rdac,,,rahc-daer,,?riap,,snoc,rac,rdc,,?vqe,,,,,;9%@MX#Z5'j%!U3^z!U3i$]$8U3YTA9%@Z-@YKa@YLiU?@YL`y{!/9$i&iU;y]%9%@Z-@YKZ<^'i$~Z&^YH@YT<vCvR3y]<7#YU5^z!U58TMi&:HiU>ai&kkz!U>:kw'k!TO'_,YTOaA_B^~E^{!T>'^8T>YT8lbA`^'`~?_B_~E_|!TF8TL`^YT>ka_AaB`1YTFdAbCai$B`^~E_|!U6#`kn8>^~i$#`kn8>^~i$#`kn8>^~i$#`kn8>^~YT9U^~?w)O^~?kR^~YT9^z!U/#YU6a_l{!TL#a_k#k_k~?iU>_{!T71b1:YT7fAdbw)k~EAaB`^|!TB1YT7:h-w6k1YT7f~?iU>fdCaaa^}(!TK,i&^z!TN,YTK`^{!TH,YTNb`^|!T;,YTHca_wS.|!1#b`n8TBfCi&CbwU<awU<`8TFCea_`~YI_A`1ci$1cYT;CPdxPCAJcwS%BJa1cCAJbwS%~?BJbwU=~EAa_~?xP^1ci$1cM^1cMYTHYT;CPgwS/wU<wU<YTKYTN`wU<wT#~EPbJa~EAa_~?wS/^1ci%1cM^1cMYT;i$CPdwSK^~EPbJa~EAa_~?wSK^8TBfPdJ`B_`BJ`~?wT#^8T7cAa_~?wS%^#YTLewTA#d~YMbYTMi&:YT7iU>PeYTOCCfi$i$akYJ_nJ`~?wS;^1:HgZ.ecHfYCdboJa_~?wS.^1YU/dYT>lbJbYCa_~M?wSA_8TBfCi&CbwU<awU<`8TFCea_`~YI_A`1ci$1cYT;CPdxPCAJcwS%BJa1cCAJbwS%~?BJbwU=~EAa_~?xP^1ci$1cM^1cMYTHYT;CPgwS/wU<wU<YTKYTN`wU<wT#~EPbJa~EAa_~?wS/^1ci%1cM^1cMYT;i$CPdwSK^~EPbJa~EAa_~?wSK^8TBfPdJ`B_`BJ`~?wT#^8T7cAa_~?wS%^#YTLewTA#d~YMbYTMi&:YT7iU>PeYTOCCfi$i$akYJ_nJ`~?wS;^1:HgZ.ecHfYCdboJa_~?wS.^1YU/dYT>lbJbYCa_~M^~^?wSI^#cJan~?wS-^B_~E_#bYT>k``m~YI_|!T<5_@L^{]-5uy!T./5^~Q^z!@'i$8@aA_@L^8@aA_@L^@LvS#~M?vS#_8@aA_@L^8@aA_@L^@LvS#~M^~^?vE^8@aA_@LvS;@LvS#~?t^8@aA_@LvS9@LvS#~?v0^8@aA_@LvS5@LvS#~?u^8@aA_@L^~YA`B^~E^{!U$'i$'i$8U$A^@YKB^~E^@LvC~E^z!L8LZC^8T<vS7vF~YO^8KZE^@LvF~YD^8@i$V^~T^8LZ4^~YI^5vL@YU$A^@YKB^@LvK~E^8T<vLvK~YM^8T<vS;vF~?i%^8T<vS-vF~YA^z!K8L^5vE@Wi%V^@LvE~T^z!U&8U&8TD~?u^'^~Ik^Gy!TD8TD@G'^8U&~?vR0^~I_vC'iU:~Z&^YFy!T:8T:C`^8T:Ca^8T:Cat~?vS;^8T:Cav0~?vS9^8T:Cau~?vS5^G~?vS#^9B_~?vE^'i&~Z&^Gz!TC,YTC^@G'i&~MMIvD`,YTC^@G'i&~MM^~^?vL_,YTC^@G'i&~M^~^?vK^YFy!U%,YU%^YH'i&@G~?vL^YTDy!7'_87DDvRL_K`v3@G~i$'_87DDvRL_K`v3@G~IvS.^~I_vS'87DDvR,_K`v3@G~i$'_87DDvRL_K`v3@G~i$'_87DDvRL_K`v3@G~IvS.^~I_vS'87DDvR,_K`v3@G~IvR<^~I_vR587DDvR%_K`v3@G~i$'_87DDvRL_K`v3@G~i$'_87DDvRL_K`v3@G~IvS.^~I_vS'87DDvR,_K`v3@G~i$'_87DDvRL_K`v3@G~i$'_87DDvRL_K`v3@G~IvS.^~I_vS'87DDvR,_K`v3@G~IvR<^~I_vR587DDvR%_K`v3@G~IvR/^~I_vR$YFz!H96`'^~^^Z=^YBCYTC^@G8BYT:i&@G~?vE^,Ci&YHwS-@G~?vJ^9#YH-Nkk87k@G~?vP^YF@G~M?vRM_9#YH-Nkk87k@G~?vP^YF@G~M^~^?vS?^'i%@G~?vS;^'i$@G~?vS-^YF@G~?vF^8U%@G~?vK^'^~Ik^YTDy!F'^!U8^Gy!U-'^!U8iU@'^~?iU:^!U8^z!08U-^8U-YU2~?iU@^'^~?iU:^iU8y!U8iU@]&(iU:^z]5/7%YTA'_@YU7Uc^@YT=Rc^OOYTAi$zOOYTAi$~YO^z]8/'i$98A`^@X$B_~E_~YO^{]H/'i&,ZHAa_X$B_~E_~YO^{!TM#l`^{!OYT@l!T)/8T=YT?aO_^~YD^{!SB9#YT?k^z!S)/8U'b`R^~YD^|!SP/9*`R^~YD^{!T1/88^~YD^z]E/8;^~YD^z]##oYJ_^z!DYT@o!S@/8T=YT?aO_^~i$/8T=YT?aO_^~Q_~T^{!S(8BYPi&V^z]L8BYPV`V^{!U(8Ba8U(CfZ@bb`a_Dl`~Sa_}'!T'8U(i&b`^|!U#'k'iU:~E_'l8U#AbA`'l~I`^'iU:~I__B`B^~E_~E^{!TP/8U#V`V^~i$/8U#V`V^~T_~T^{!SO8AZI`^{!S88AZF`^{]F2YTPa_k{]I2kYTP`^{!T*(kYTP`^{!S68BYT?vC^z!T(/8U'b`R^~T^|]@/9*`R^~T^{!T%/88^~T^z!?/8;^~T^z!U*'i%'i$8U*A_~Q^B^~E^z!B/#nYJ_^~YU*^z!=YT@n!S?i'!T&i'!SMiT4!T0iS#!S>iT5!SFi<!SJi(!U)'_'i$'i$8U)DDvR%`KbuA_~IvR/^~I_vR$B^~E^{!U18U)k^'i$~YM^z]=/'i$-_k~^YU1^8U1A^~?vPB^'i$~YM^V^~T^z!TI'^8TI_`~IakCb^YT8DKu``vR%YGu^{]C8BYTIi&^8BCYTIi&D`kvP~Sk^z]D'^4__~ZJ`ZDYGm`ZK_^'l~?k_{!S'i'!T,i'!T$i'!S+i'!S4'lz!SDi'!SC4_YTGZ7``_YN`YN^'k~?k_{!TE8TE_Z?__'_~?k^{]78TE`^8TE__~I__YN`YN^{!T28T8b^'^~?IkbIk`'k~?k^DK`a_YG`^{]?-KYGb``^{!N'^-_k~Sk^z!S,'_'^~S`^{!T+'^'_~S`^{]J8AZ9^z]9(KYGm`m^z!S*8<k^z!S78<_kz!T-(k^z!T48AS`^{!S#8AS__{!T58<__{!</2`^~i$/2`^~Q_~Q^{!SH/(`^~i$/(`^~Q_~Q^{!G/8TG`^9$_iUA~?k_~i$/8TG`^9$_iUA~?k_~Q_~Q^{]K/4`^~i$/4`^~Q_~Q^{]2/-`^~i$/-`^~Q_~Q^{!SE/8T8`^~i$/8T8`^~Q_~Q^{!:8AYT9^z@YU.ki#!U9Oi#!U+'^!U9CiU9^YU4^8U+Oa_'^~YE`O^R_~E_{]6/8U+iU9^~T^z]4/88^~YI^z!U4#m_i$z!IYT@m!U0'`8U0Cca`Dl^~S_k|!T?8U0i&`^{]>'i$9>Aa_'^~YEB__B_~E_{!T3j:]:'i$9:Aa_'^~?B__B_~E_{];'i$9;A`^'_~YEB`^~E_{!S=jG]G'i$9GA`^'_~?B`^~E_{!TJ'^8TJDl`A^~S`k{!U'9AaYTJ`^|]*+YTJ`^{!U,'_8U,CaB_A^~E^{]B8U,i&^z!P'_,YPaA_B^~E^{!J'k8T8YJA_l~E^z!M(i&^z]M9'A^z!S&9'B^z]O9(A^z!S99(B^z!S09,A^z!S$9,B^z]N90A^z!S190B^z!SG8CB^z!T69)A^z!S29)B^z!SL9+A^z!S59+B^z!S<9/A^z!SN9/B^z]'89A^z](89B^z],91A^z]091B^z])3B^z]+93A^z]/93B^z]1*B^z]3+B^z].8CA^z!C3A^z!9*A^z!3+A^z!S3/8U.`^~E^{]A/8T=`^~E^{!*/88^~E^z!+/8;^~E^z!,#k`^{!.YT@k!E'i$'i$'i$'i$8ERaR_~YEOaO_~YEUaU_~YT9`'i$~?pU_~YT9_'^~^?`^{!T/i(!S:8A_'^~^?i%^z!A(i$^z!T@8TA'i$(bU^~YT9^zz!TGiG!4jK!-j2!T8iSE!2i<!U?:nl:ki&vC!U;:nv1:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS9vS6vS9vS9vS,vCvS,vS7vS@vS;vCvMvMvM!U@Z2mk!U:Z2lk!UA:nv2:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vR%vCvS@vS)vCvS,vS+vS0vS=vS0vS+vCvMvMvM!':lkl!):lkm!6:lkn!TA:lko!T9:lkp!;:lkq!8:lkr!>:lks!T=:lkt!U.:lku!U7:lkv.!(:lkv/!<:lkv0!SE:lkv1]2:lkv2]K:lkv3!G:lkv4!U2:lkv5!5:lkv6y" 
main=do
 hSetBuffering stdout NoBuffering
 fb<-getArgs
 db<-case fb of
  [ec]->readFile ec
  s->pure qc
 zc<-vc db
 w zc
