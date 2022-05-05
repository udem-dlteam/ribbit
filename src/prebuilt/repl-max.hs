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
qc="R'rdadac,>,=>,qssa,oludom,htgnel-rotcev,?=<rahc,?qe,rahc-etirw,?regetni,?orez,etacnurt,xam,?=gnirts,!llif-rotcev,!tes-gnirts,gnirtsbus,regetni>-rahc,htgnel-gnirts,gniliec,tel,fer-rotcev,?=>gnirts,raaaac,?=>rahc,rddaac,dna,?=rahc,!tes,=,raddac,?<rahc,rotaremun,mcl,rotcev-ekam,enifed,!llif-gnirts,rahc>-regetni,?>rahc,qmem,rdaaac,adbmal,?naeloob,raaddc,?=<gnirts,?evitisop,gnirts-ekam,radaac,rotanimoned,!rdc-tes,raaadc,raadac,rddadc,ro,rorre,fi,nim,roolf,?evitagen,etouq,!tes-rotcev,cc/llac,ypoc-gnirts,dnuor,radddc,nigeb,radadc,=<,dnoc,rdaddc,rdaadc,rddddc,dneppa-gnirts,?ddo,tixe,?<gnirts,pam,vmem,?>gnirts,?erudecorp,tsil>-rotcev,tpxe,gnirts>-rebmun,esrever,?rotcev,!rac-tes,fer-gnirts,redniamer,cossa,rebmun>-gnirts,lave,rebmem,vssa,?neve,hcae-rof,dcg,lobmys>-gnirts,gnirts>-lobmys,raac,radc,raadc,raaac,rdddac,lper,?gnirts,rdadc,rdaac,fer-tsil,radac,raddc,rdddc,?tcejbo-foe,enilwen,rotcev>-tsil,dneppa,+,sba,?lobmys,?llun,yalpsid,etirw,htgnel,daer,rahc-keep,?lauqe,rddac,tneitouq,,gnirts>-tsil,tsil>-gnirts,ton,,,rddc,,,*,,rdac,,,,rac,?riap,rahc-daer,<,-,rdc,snoc,,?vqe,,,,,;9)!S,9Fl@YN@YF_@YGiU7@YG^{])9)@YN@YFZ6^8N~YO^YD@YT8vCvR3y]67#YU.^z!U.8THi&:HiU6ai&kkz!U6:kw'k!TJ'_*YTJaB_G^~F^{!T9'^8T9YKlbB`^'`~?_G_~F_|!TA8TG`^YT9ka_BaG`1YTAdBbAai$G`^~F_|!U/#`kn8:^~i$#`kn8:^~i$#`kn8:^~i$#`kn8:^~YT5Q^~?w)I^~?kJ^~YT5^z!U'#YU/a_l{!TG#a_k#k_k~?iU6_{!?1b1:VfBdbw)k~FBaG`^|!T<1V:h-w7k1Vf~?iU6fdAaaa^}(!TF*i&^z!TI*YTF`^{!TB*YTIb`^|!T7*YTBca_wS+|!1#b`n8T<fAi&AbwU4awU4`8TAAea_`~YI_B`1ci$1cYT7APdxLABKcxOGKa1cABKbxO~?GKbwU5~FBa_~?xL^1ci$1cN^1cNYTBYT7APgwS-wU4wU4YTFYTI`wU4wSN~FPbKa~FBa_~?wS-^1ci%1cN^1cNYT7i$APdwSH^~FPbKa~FBa_~?wSH^8T<fPdK`G_`GK`~?wSN^8?cBa_~?xO^#YTGewT?#d~YHbYTHi&:ViU6PeYTJAAfi$i$akYE_nK`~?wS9^1:HgZ*ecHfYAdboKa_~?wS+^1YU'dYT9lbKbYAa_~N?wS?_8T<fAi&AbwU4awU4`8TAAea_`~YI_B`1ci$1cYT7APdxLABKcxOGKa1cABKbxO~?GKbwU5~FBa_~?xL^1ci$1cN^1cNYTBYT7APgwS-wU4wU4YTFYTI`wU4wSN~FPbKa~FBa_~?wS-^1ci%1cN^1cNYT7i$APdwSH^~FPbKa~FBa_~?wSH^8T<fPdK`G_`GK`~?wSN^8?cBa_~?xO^#YTGewT?#d~YHbYTHi&:ViU6PeYTJAAfi$i$akYE_nK`~?wS9^1:HgZ*ecHfYAdboKa_~?wS+^1YU'dYT9lbKbYAa_~N^~^?wSF^#cKan~?wS'^G_~F_#bYT9k``m~YI_|!T85_@L^{!N5uy!T,i5!;'i$8;aB_@L^8;aB_@L^@LvS#~N?vS#_8;aB_@L^8;aB_@L^@LvS#~N^~^?vE^8;aB_@LvS;@LvS#~?t^8;aB_@LvS9@LvS#~?v0^8;aB_@LvS5@LvS#~?u^8;aB_@L^~S`G^~F^{!TL'i$'i$8TLB^@YFG^~F^@LvC~F^z!G8GZ>^8T8vS7vF~ZA^8FZ@^@LvF~Z<^8;i$T^~Z(^8GZ/^~YI^5vL@YTLB^@YFG^@LvK~F^8T8vLvK~YH^8T8vS;vF~?i%^8T8vS-vF~S^z!F8G^5vE@Ri%T^@LvE~Z(^z!TN8TN8T>~?u^'^~Dk^Ey!T>8T>@E'^8TN~?vR0^~D_vC'iU8~YO^YCy!T68T6A`^8T6Aa^8T6Aat~?vS;^8T6Aav0~?vS9^8T6Aau~?vS5^E~?vS#^9=_~?vE^'i&~YO^Ez!T=*YT=^@E'i&~NNDvD`*YT=^@E'i&~NN^~^?vL_*YT=^@E'i&~N^~^?vK^YCy!TM*YTM^YD'i&@E~?vL^YT>y!8'_88CCvRL_M`v3@E~i$'_88CCvRL_M`v3@E~DvS.^~D_vS'88CCvR,_M`v3@E~i$'_88CCvRL_M`v3@E~i$'_88CCvRL_M`v3@E~DvS.^~D_vS'88CCvR,_M`v3@E~DvR<^~D_vR588CCvR%_M`v3@E~i$'_88CCvRL_M`v3@E~i$'_88CCvRL_M`v3@E~DvS.^~D_vS'88CCvR,_M`v3@E~i$'_88CCvRL_M`v3@E~i$'_88CCvRL_M`v3@E~DvS.^~D_vS'88CCvR,_M`v3@E~DvR<^~D_vR588CCvR%_M`v3@E~DvR/^~D_vR$YCz!D90`'^~^^Z7^UAYT=^@E8>YT6i&@E~?vE^*Ai&YDwS'@E~?vJ^8MYD,Okk88k@E~?vP^YC@E~N?vRM_8MYD,Okk88k@E~?vP^YC@E~N^~^?vS?^'i%@E~?vS;^'i$@E~?vS-^YC@E~?vF^8TM@E~?vK^'^~Dk^YT>y!C'^!U1^Ey!U&'^!U1iU3'^~?iU8^!U1^z!.8U&^8U&YU+~?iU3^'^~?iU8^iU1y!U1iU3!O(iU8^z!S%7%YT?'_@YU0Qc^@YTDJc^IIYT?i$zIIYT?i$z]2'i$92B`^@X$G_~F_{]D'i&*ZDBa_X$G_~F_{!TH#l`^{]AYT:l!T&8TDYT;aI_^{!S@8MYT;k^z!S&8TOb`J^|!SM9%`J^{!T/i2]@i3!M#oYE_^z]<YT:o!S>8TDYT;aI_^{!S$8>YLi&T^z]H8>YLT`T^{!TP8>a8TPAfZ:bb`a_Cl`~Da_}'!T$8TPi&b`^|!U*'k'iU8~F_'l8U*BbB`'l~D`^'iU8~D__G`G^~F_~F^{!TK8U*T`T^{!SL8<ZE`^{!S68<ZB`^{]B-YTKa_k{]E-kYTK`^{!T'(kYTK`^{!S48>YT;vC^z!T%8TOb`J^|]:9%`J^{!SPi2!=i3!>#nYE_^z](YT:n!S=i'!T#i'!SJiT2!T.jM!S<iT3!SCi-!SGi(!U#'_'i$'i$8U#CCvR%`MbuB_~DvR/^~D_vR$G^~F^{!U)8U#k^'i$~YH^z]7'i$,_k~^YU)^8U)B^~?vPG^'i$~YH^T^z!TC'^8TC_`~DakAb^YKCMu``vR%Wu^{]>8>YTCi&^8>AYTCi&C`kvP~Dk^z]?'^6__~ZG`Z?Wm`M_^'l~?k_{!S#i'!T)i'!SOi'!S)i'!S2'lz!SBi'!SA6_WZ1``_YJ`YJ^'k~?k_{!T@8T@_Z9__'_~?k^{]18T@`^8T@__~D__YJ`YJ^{!T08Kb^'^~?DkbDk`'k~?k^CM`a_W`^{]9,MWb``^{!J'^,_k~Dk^z!S*'_'^~D`^{!T('^'_~D`^{]G8<Z3^z]3(MWm`m^z!S(-k^z!S5-_kz!T*(k^z!T28<D`^{]M8<D__{!T3-__{!SEi(!T+8<YT5^z@YU,ki#!U2Ii#!U$'^!U2AiU2^YU-^8U$Ia_'^~YB`I^J_~F_{]08U$iU2^z]/i2!U-#m_i$z!IYT:m!U('`8U(Aca`Cl^~D_k|!T;8U(i&`^{]8'i$98Ba_'^~YBG__G_~F_{!T1j4]4'i$94Ba_'^~?G__G_~F_{]5'i$95B`^'_~YBG`^~F_{!S;jC]C'i$9CB`^'_~?G`^~F_{!TE'^8TECl`B^~D`k{!TO9;aYTE`^|]%0YTE`^{!U%'_8U%AaG_B^~F^{]=8U%i&^z!L'_*YLaB_G^~F^{!E'k8KYEB_l~F^z!H(i&^z]I8PI^z]P8PJ^z]K9#I^z!S79#J^z!S.9'I^z]N9'J^z]J9,I^z!S09,J^z!SD8AJ^z!T49$I^z!S/9$J^z!SI9&I^z!S39&J^z!S:9+I^z!SK9+J^z!P89I^z]#89J^z]'9-I^z],9-J^z]$4J^z]&9.I^z]+9.J^z]-2J^z].3J^z]*8AI^z!A4I^z!92I^z!43I^z!S1iU,];iTD!+i2!0i3!*#k`^{!/YT:k!B'i$'i$'i$'i$8BJaJ_~YBIaI_~YBQaQ_~YT5`'i$~?pQ_~YT5_'^~^?`^{!T-i(!S88<_'^~^?i%^z!<(i$^z!T:8T?'i$(bQ^~YT5^zz!U7:nl:ki&vC!U3Cmk!U8Clk!':lkl!):lkm!7:lkn!T?:lko!T5:lkp!3:lkq!2:lkr!::lks!TD:lkt!U,:lku!U0:lkv.!(:lkv/!-:lkv0!K:lkv1!,:lkv2!6:lkv3!@:lkv4!U+:lkv5!5:lkv6]F:lkv7y" 
main=do
 hSetBuffering stdout NoBuffering
 fb<-getArgs
 db<-case fb of
  [ec]->readFile ec
  s->pure qc
 zc<-vc db
 w zc
