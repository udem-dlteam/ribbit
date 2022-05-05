input="Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,enilwen,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8L!L8L@YJ@YGZ$^8J~YN^YC@PvCvR3y]$7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YM_|!94_@K^{!J4uy]?'i$9?C_@K^G^~F^z]I'i$'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YK^8EZ)^~YM^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z%^z!G8E^4vE@Z?YD^@KvE~YK^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YN^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!N(iS0^z]27%Z>'_@YS&Jc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-9#`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];9#`H^{]<i+!Di1!B#nS_^z!KQn]F'_'i$'i$9FLLvR%`YObuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$5_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHLYOu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!MQm]J'`9JAca`Ll^~I_k|]L9Ji&`^{]A'^9ALl`C^~I`k{]N9'aZA`^|]#0ZA`^{!<'k8HSC_l~F^z!=(i&^z!P87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8AHaH_~YABaB_~YAJaJ_~R`'i$~?pJ_~R_'^~^?`^{]%(i$^z!:9>'i$(bJ^~R^zz!S.Lmk!S0Llk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!O:lkv3]&:lkv4!S#:lkv5!4:lkv6y"
import sys
D=sys.stdout
T=lambda e:[D.write(chr(e)),D.flush(),e][2]
def W():A=sys.stdin.read(1);m(ord(A)if len(A)else-1)
B=-1
def I():global B;B+=1;return ord(input[B])
q=[0,0,5]
G=[0,0,5]
r=[0,0,5]
z=lambda a:G if a else q
o=lambda a:type(a)is list
d=0
def m(a):global d;d=[a,d,0]
def g():global d;A=d[0];d=d[1];return A
n=lambda u:lambda:m(u(g()))
j=lambda u:lambda:m(u(g(),g()))
S=lambda u:lambda:m(u(g(),g(),g()))
def N():A=g();g();m(A)
def O():m([g()[0],d,1])
def V(c,a):a[0]=c;return c
def U(c,a):a[1]=c;return c
def Q(c,a):a[2]=c;return c
R=[S(lambda P,c,a:[a,c,P]),n(lambda a:a),g,N,O,n(lambda a:z(o(a))),n(lambda a:a[0]),n(lambda a:a[1]),n(lambda a:a[2]),j(V),j(U),j(Q),j(lambda c,a:z(a is c if o(a)or o(c)else a==c)),j(lambda c,a:z(a<c)),j(lambda c,a:a+c),j(lambda c,a:a-c),j(lambda c,a:a*c),j(lambda c,a:int(a/c)),W,n(T),n(exit)]
def K():A=I()-35;return 57 if A<0 else A
def x(b):A=K();b*=46;return b+A if A<46 else x(b+A-46)
C=lambda L,l:L if l==0 else C(L[1],l-1)
h=r
b=x(0)
while b>0:b-=1;h=[[q,[r,0,3],2],h,0]
t=r
b=0
while 1:
	e=I()
	if e==44:h=[[q,[t,b,3],2],h,0];t=r;b=0
	else:
		if e==59:break
		t=[e,t,0];b+=1
h=[[q,[t,b,3],2],h,0]
J=lambda b:C(h,b)[0]
while 1:
	a=K();b=a;p=0;k=0
	while 1:
		p=[20,30,0,10,11,4][k]
		if b<=2+p:break
		b-=p+3;k+=1
	if a>90:b=g()
	else:
		if k==0:d=[0,d,0];k+=1
		b=x(0)if b==p else J(x(b-p-1))if b>=p else J(b)if k<3 else b
		if 4<k:
			b=[[b,0,g()],r,1]
			if not d:break
			k=4
	d[0]=[k-1,b,d[0]]
f=b[0][2]
A=lambda i:i if o(i)else C(d,i)
def E():
	A=d
	while not A[2]:A=A[1]
	return A
def w(M):global h;h[0][0]=M;h=h[1]
w([0,h,1])
w(q)
w(G)
w(r)
d=[0,0,[5,0,0]]
while 1:
	i=f[1];l=f[0]
	if l<1:
		i=A(i)[0];e=i[0]
		if o(e):
			s=[0,i,0];y=s;F=e[0]
			while F:y=[g(),y,0];F-=1
			if o(f[2]):s[0]=d;s[2]=f[2]
			else:H=E();s[0]=H[0];s[2]=H[2]
			d=y
		else:
			R[e]()
			if o(f[2]):e=f
			else:e=E();d[1]=e[0]
		f=e[2]
	elif l<2:A(i)[0]=d[0];d=d[1];f=f[2]
	elif l<3:m(A(i)[0]);f=f[2]
	elif l<4:m(i);f=f[2]
	elif l<5:f=f[2 if g()is q else 1]
	else:break