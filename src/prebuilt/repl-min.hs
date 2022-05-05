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
qc="Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,enilwen,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8L!L8L@YJ@YGZ$^8J~YN^YC@PvCvR3y]$7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YM_|!94_@K^{!J4uy]?'i$9?C_@K^G^~F^z]I'i$'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YK^8EZ)^~YM^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z%^z!G8E^4vE@Z?YD^@KvE~YK^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YN^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!N(iS0^z]27%Z>'_@YS&Jc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-9#`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];9#`H^{]<i+!Di1!B#nS_^z!KQn]F'_'i$'i$9FLLvR%`YObuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$5_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHLYOu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!MQm]J'`9JAca`Ll^~I_k|]L9Ji&`^{]A'^9ALl`C^~I`k{]N9'aZA`^|]#0ZA`^{!<'k8HSC_l~F^z!=(i&^z!P87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8AHaH_~YABaB_~YAJaJ_~R`'i$~?pJ_~R_'^~^?`^{]%(i$^z!:9>'i$(bJ^~R^zz!S.Lmk!S0Llk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!O:lkv3]&:lkv4!S#:lkv5!4:lkv6y" 
main=do
 hSetBuffering stdout NoBuffering
 fb<-getArgs
 db<-case fb of
  [ec]->readFile ec
  s->pure qc
 zc<-vc db
 w zc
