import Data.IORef
import Control.Monad.State
import Data.String

import System

input : String
input = "R1llac/pmuj,htgnel-rotcev,?=<gnirts,trats-tni-llac,raaaac,po-tsnoc,?=>rahc,raaadc,_,dna,po-tes,?naeloob,!llif-rotcev,trats-tni-tsnoc,rdaadc,tes,trats-tni-teg,mcl,etouq,dnapxe-dnoc,xam,oludom,esle,trats-corp-tsnoc,radddc,dnoc,rts-ot-tuptuo-htiw,po-teg,cc/llac,adbmal,rdaddc,rdadac,raadac,!tes-rotcev,trats-llac,tel,stropxe-xtc,?>rahc,rotaremun,?tsil,rddddc,roolf,trats-teg,yllamron-margorp-tixe,margorp-daer,ypoc-gnirts,enifed,yllamronba-margorp-tixe,gnirts-ekam,?evitagen,2/be,liat,gniliec,?=<rahc,trats-mys-teg,dnuor,elif-led,trats-mys-llac,trats-tni-tes,nim,rotanimoned,tropxe,trats-tes,trats-mys-pmuj,etacnurt,radaac,slobmys-denretninu,raddac,rdaaac,trats-mys-tsnoc,ro,trohs-mys-llac,reffub,rotcev-ekam,!tes-gnirts,qmem,fer-rav-labolg,po-fi,raaddc,trats-tsnoc,rddadc,teg,*tel,tsnoc,nigeb,trats-mys-tes,!tes,edoc-tegrat-etirw,vne-erudecorp,trats-tni-pmuj,?=>gnirts,certel,trats-fi,radadc,!llif-gnirts,tibbir,dmc-llehs,lbtmys,!tes-rav-labolg,?<rahc,rahc-etirw,?evitisop,?orez,rddaac,po-llac/pmuj,fer-rotcev,fi,sedoc>-gnirts,sfed-teser,tsil>-elbat,tros-tsil,enil-dmc,edoc-etareneg,?ddo,rahcteg,tpxe,?regetni,yrarbil-daer,dcg,elif-tupni-htiw-llac,lper,relipmoc-enilepip,htap-elbatucexe,elif-tpircs,ssenevil,sisylana-ssenevil,?=rahc,noisnetxe-htap-csr,htgnel-elbat,gnirts-ot-tuptuo-htiw,dnibnu-neg,enil-daer,!tros-tsil,elif-morf-tupni-htiw,>,lobmys-denretninu>-gnirts,?qe,rebmun>-gnirts,margorp-elipmoc,vssa,?neve,?erudecorp,elipmoc,=<,lave,?>gnirts,redniamer,elif-ot-tuptuo-htiw,?<gnirts,!tes-evil-xtc,edocne,stropxe-tcartxe,xua-lobmys>-gnirts,tsil-dnapxe,yrotcerid-htap-csr,xua-rebmun>-gnirts,xua-sisylana-ssenevil,stropxe-htiw-srpxe-pmoc,fer-tsil,raadc,repo,rebmem,rahc>-regetni,xua-gnirtsbus,2xua-rebmun>-gnirts,!tes-tsil,xua-pmc-gnirts,radac,elbat-ekam,xua-rahc-daer,edoc-erudecorp,?=gnirts,xtc-ekam,*dnib-pmoc,?tnatsnoc,!tes-elbat,radc,gnirts>-lobmys,raaac,tnemmoc-piks,tsil-etirw,tsil-daer,?ni,tsila>-stropxe,rdaac,raac,xua-tsil-ekam,rdadc,xua-esrever,raddc,xua-?tsil,evil>-stropxe,elif-morf-gnirts,liat-tsil,tixe,xua-dcg,evil-xtc,lobmys>-gnirts,?2erudecorp,tnatsnoc-dnapxe,hcae-rof,lla-daer,ydob-dnapxe,lave-dnapxe-dnoc,pmc-gnirts,elif-morf-daer,!rac-tes,1tsil,xua-gnirts>-rebmun,lobmys-esu,3tsil,nigeb-dnapxe,rotcev>-tsil,2tsil,?rotcev,rdddc,tsil-ekam,sba,enilwen,ecapsetihw-non-rahc-keep,regetni>-rahc,lobmys-daer,poon-neg,?gnirts,lobmys-denretninu>-rts,tsil>-rotcev,fer-elbat,gnirtsbus,ngissa-neg,tes-etc-xtc,cossa,!tes-2dleif,gnirts>-rebmun,enod-ydob-dnapxe,2rahctup,rdddac,?tcejbo-foe,llac-neg,llac-pmoc,?ecnatsni,daer,fer-gnirts,srahc-daer,etanetacnoc-gnirts,rts>-lobmys,rahc-keep,pp,fi-dliub,?evil,dnetxe,erudecorp-ekam,sesualc-dnapxe-dnoc-dnapxe,dnib-pmoc,etirw,*nigeb-dnapxe,!tes-1dleif,dnpo,etc-xtc,gnirts>-maerts,rorre,?llun,txen,qssa,!rdc-tes,tneitouq,?bir,pukool,evil-dda,srahc-etirw,nigeb-pmoc,=>,esrever,htgnel-gnirts,dneppa,vmem,rddac,=,gnirts>-tsil,!tes-0dleif,xeh-daer,tsil>-gnirts,?lobmys,htgnel,rahctup,*,2dleif,rpxe-dnapxe,yalpsid,hguorht-epip,ecalper-gnirts,setyb-ot-edoc-mvr,rid-toor,?lauqe,pam,dnapxe-htap,rahc-daer,0dleif,1dleif,rddc,pmoc,ton,<,+,-,rdac,esolc,dneppa-gnirts,?riap,rac,?vqe,1gra,2gra,rdc,di,snoc,lin,eurt,eslaf,bir;8U0!U08BYU9YTMYS<ki$i$kiXCy!V$8TDG8Bby^8B_~TiX;^{!@7&i&kkA^[$G9@iY#Z$>aZPZ#h-_f7$IlfA^[$G7/fJldb7\'Il^~YU+ZB`h1ZBJ`dh/70>>h.ZPh.gh3h4_^Jh/c~Kk^zi$~YTHZ#gJf_|i$Z#aZ#_|!?9@`SYI_G9KYS)^z{!U9(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBi$(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBYBiX9BYBZ#^BYBiY+~Z%ldFiY*(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBi$(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBYBiX9BYBZ#^BYBiY+~Z%ldFYSEe~e_YSERUFFFdiXJbiX$(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBi$(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBYBiX9BYBZ#^BYBiY+~Z%ldFiY*(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBi$(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^LgAWiX8iX.WViXHaViXA_WViWNaViX*_Wa_`iXO(^YA_RUFFiXBeiXI(^^~ATiY&e(^YA_RUFFiXBeiXI(^^~A^~^Lg^~TiX0cBYBiX9BYBZ#^BYBiY+~Z%ldFYSEe~e_iX&~TiX(aYTA__@cDb}(!SE8U2G8U&i$^z^z!VO8PYS8`8PYS<~TiY-`YU4^{!U48S8^8S8RRUiY%FiXM^~TiXFYU*^z!S88U$iS<^z!S<\'YS<^(i&~ZG^ZCy!>8T=AYU/8T=A^~^YU.y!U>8<YI_iS)z]18LS_G8T5^8T5vD~YMvS#^JvF^zz!TA7&i&`kA^[$G7&ca_A^[$G7&cg_A^[$G7$aA^[$G/FFZ1aiX?Z@iX>SaG8LZ$YI^Z?^zZ1XBi&IYG`YGeX?i&hR%7\'@^~i$/FFZ1aiX?Z@iX>SaG8LZ$YI^Z?^zZ1XBi&IYG`YGeX?i&hR%7\'@^~YT,iX6Z?D^~E^zi$70>h-a@eJlcBYKd^@aYS%iX,7.e@ca~^Z-hR%^D^D_~E_|i$7.e@ca70>h-a@eJlcBYKd^@a@^~^Z-hM^D^D_~E_|i$70>h-`@eJlcBYKd`YS%iWP70>h-`@eJlcBYKd`@^~^Z-hH_@_D^D_~i$7&ca_A^[$G7&cg_A^[$G7$aA^[$G/FFZ1aiX?Z@iX>SaG8LZ$YI^Z?^zZ1XBi&IYG`YGeX?i&hR%7\'@^~i$/FFZ1aiX?Z@iX>SaG8LZ$YI^Z?^zZ1XBi&IYG`YGeX?i&hR%7\'@^~YT,iX6Z?D^~E^zi$70>h-a@eJlcBYKd^@aYS%iX,7.e@ca~^Z-hR%^D^D_~E_|i$7.e@ca70>h-a@eJlcBYKd^@a@^~^Z-hM^D^D_~E_|i$70>h-`@eJlcBYKd`YS%iWP70>h-`@eJlcBYKd`@^~^Z-hH_@_D^D_~KiV4^~E_|i$YU;YU<h<G4X)_X(_{BX5h<BX1h<BYS=iX)G74n^z[$G89^@`3YG_vS[rz0~^YT6iX)^D^z[%G90i&iX290_iX77+X,>ciUMZ3`Z._~CiV.^7+AZ0_iX@7+AX/c^~YS?^7+AX3dX4_iV6~YH^7+AX4d_iW@7+A>cJ_iV,~Kv.^~X-^Z3`Z._~CiWH^7+AZ0_iXG7+AX3dX4_iVE~YH^7+AX4d_iW=7+A>cJ_iW#~Ku^~X-^Z3`Z._~CiW2^7+AZ0_iXN7+AX3dX4_iV&~YH^7+AX4d_iVA~X-^Z3`Z._~CiWC^7+AZ0_iY(7+AAX4e_iVB7+AA>dJ_iW+~KiV4^X3^~YH^7+AX4d_iWJ~X-^Z3`Z._90_iY)73d_iV<\'cJ_k~Kv7^X2^~YH^73c_iUP~X,^Z3_~CZ.`k~CiUA^YT7^~Z*^{[&G8U5^z[\'G7,X4d`JoiW67,>cJ`iW6~Ko_Z._P^YT-^{[(G72e_`(^~YMk`>bJiVI^72e_`(^~YMk`>b^~CcaIYEiVI__Z+iVI^|[)G7.a`^{[*G\'``\'@`Jl`~YMJliVI^D^X/a_|[+G\'X0b`^|[,G89^YS#i$_h>z[-G(i$76n^~CiWC_77l^BZ4_aBYKiW2aX8^76m^~AAi$77l^BZ4_aBYKiW2aX8^76m^~AAZ%k`~X-`77l^BZ4_aBYKiW2aX8^76m^~AA^~^YS?_77l^BZ4_aBYKiW2aX8^76m^~A^~^YH^~CiWH_76l^~CiW2_76k^~CiUA_73^~CiV._Z3_YT7^z[.G(i$71Z.^BX0^~Z*^z[/G(i$75^~YS?^(i$9LJYD`l^~YMm`94JO`l^~YMl`8KJP`l^~YMk`AA^BYT(`ahA:kkk(i$9LJYD`l^~YMm`94JO`l^~YMl`8KJP`l^~YMk`A^~^YS#i$_h?~YH^{[0G(^BX4`^{[1G71Z.YT-^z[2G(^BX4k^z[3G(^BX6n^BX?_`[9Jlh8YS@FZKh8iX#1^~^Z-h>^(w&~Ci&^(w%~Ci%^(w$~Ci$^z[4k[5G9LX=X;YD_^P^z[6G7%`h>A^[$G(_7-XE:gbiWC_@bN`H_D^D^~E^{i$z[7G#X>bYD`O_P^(_~Ck^{[8G7%a@iX-A^[$G(_7)AAb7)AAX-c_~i$7)AAb7)AAX-c_~AAKYDak7)AAb7)AAX-c_~AA^~^KO`k7)AAb7)AAX-c_~A^~^KP_k~^YS#i$_hFDD_@^~E^{i$[$G#::::h.XBngiWCX@kw#iUAliWHkiWHHZ-iX-_iWH{i$z[9G(`[@>h@>>`a_X?k^{[:G90_iX/7@XA::fX;kw#iUAoiWHYG_^YS$^~YS/^7@XA::fX;kw#iUAniWHYG_^SYI_iS)~YS&^7?X@::eX:kw#iUAkiWH@_D^~E^#a_iWH#::eX:kw2iUAIbkiWHkiWH~Kk^~X*^#aX6m_iWH~YH^#bX7l_iW2X7^~AZ-h@_90_iX/7@XA::fX;kw#iUAoiWHYG_^YS$^~YS/^7@XA::fX;kw#iUAniWHYG_^SYI_iS)~YS&^7?X@::eX:kw#iUAkiWH@_D^~E^#a_iWH#::eX:kw2iUAIbkiWHkiWH~Kk^~X*^#aX6m_iWH~YH^#bX7l_iW2X7^~A^~^YOiX%^{[;i&[<YT/i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i${!UMJJloiW6!W6JmiV6!V6JliW@!W@Jv.iV,!V,JmiVE!VEJliW=!W=JuiW#!W#JmiV&!V&JliVA!VAJkiV=!V=JmiVB!VBJliWJ!WJJiV4iW+!W+JmiV<!V<JliUP!UPJv7k!V4IJJJJv4v7uov.vS#!VIZ+mvS#!U-(cBX\'i%i&b[$G(i$7)`@^BX+i$`D^~E^{[%G(i$7+a_7.i$Z:c_YN`H_~CwW0^7+a@_~CwV\'^7.i$YPcS`i-YN`BX,bS_i1H_~CwW*^7-i$aZH_BX-i$aYN_BX-i$aH_~CwU?^(i$70i$d_BZ,>@``^~^Z;h1_Bi$(i$70i$d_BZ,>@``^~^Z;h1_BX/_~h17/i$c^~YSOc_YN`H_~CwV%^7-^H_~CwW;^D^~E^7,^(i$~YSO`^~YH^|[&G(i$8S=YS$_c~YS/^7)@^BX)D^~E^7*^~YH^z[\'G(k[,Z(g^zi$i$i$i$|!SO5CZ)ka_^{!T)(i$(i$(i$,DH_wW;~EH^~Z/N^~E@^z];(i$9;@a_(^~CD__D_~E_{](\'a^>i&^(_~Z;`^{!U=7$_A^[$G(i$7\'@^BZ,i&D^~E^zi$z!SF(i&\'YSF@_>i&DD^~E^z!T;7$Z(Z(Z(Z(YSFAi&7$Z(Z(Z(Z(YSFA^~^dw(w0w*w+A^[$G7(^(_~C`^YU-Le_bBYU=^zi${!U,(^8T;a_~ZM_wUHYT;i&^{!T>(i&\'YT>@_YCD^~E^z]8(_98a@_95a@^~AYS:D_98a@_95a@^~A^~^CD_wW7D^~E^{!S:,wUJ^5LYOS@`iS:i%~i$,wUJ^5LYOS@`iS:i%~CwV5D^~E^5YOS@`iS:i$~i$,wUJ^5LYOS@`iS:i%~i$,wUJ^5LYOS@`iS:i%~CwV5D^~E^5YOS@`iS:i$~CwWDD^~E^5YS:H^~i$,wUJ^5LYOS@`iS:i%~i$,wUJ^5LYOS@`iS:i%~CwV5D^~E^5YOS@`iS:i$~i$,wUJ^5LYOS@`iS:i%~i$,wUJ^5LYOS@`iS:i%~CwV5D^~E^5YOS@`iS:i$~CwWDD^~E^5YS:H^~Cw5D^~E^z]5(_\'_YC_98_@_~i$\'_YC_98_@_~CwW:D_~E_95_@_~i$\'_YC_98_@_~i$\'_YC_98_@_~CwW:D_~E_95_@_~CwV\'D_~E_Z5a@_D^~E^{!S28S>k-^\'_wV\'~E@^~E^Z5i&^z]J8S2_8C>>aZ$_wUN~E^{!S;7%i&_A^[$G9JiX<_9J``7+>c>Na_@`7+>c>>i&>>Nc@awW0D_@`~E^H^~i$9J``7+>c>Na_@`7+>c>>i&>>Nc@awW0D_@`~E^H^~i$9J``7+>c>Na_@`7+>c>>i&>>Nc@awW0D_@`~E^H^~E@^~CD_wVM~E^D^~E^{i$z!S>\'>i&_wW;z!C8S>^8T>_8Ci$8C>>>>i&>NcwW4>@HbwV\'DH`wU?8C>@H`wV\'~CDH`wW7~E@_~CwW4^8Ci$8CH_8C>>>i&>>>>i&>NewV5wWEwWEwU?>i&>>i&HawWEwW*~EN_~E@_~CwV5^8Ci%8CH_8C>>>>i&i$>NbwWDH`wU?~EN_~E@_~CwWD^\'>>i&YCYNb_wV%\'>>i&YC>>Nd@awW0D_wV%~E^H_~CwVM^8S2@_~CwV\'^8C>>YPNcSaG\'>>i&H`D_wV%zS`G\'>i&i$D^zwW*H_~CwUN^8C>@a8C>>>i&>>Nd@awV)>i&D_~i$8C>@a8C>>>i&>>Nd@awV)>i&D_~E@_~E_wW*H_~CwV)^8S;Na\'>>i&YS;NcS`G\'>i&YCH_D^zwW*~E^^8CA>S`i1>>>i&a>i&>>i&>>YS.eSbi-wW0`wUNYN`~YH^H_~CwW*^\'>>i&YS;Nb_wW0H_~CwW0^\'>>>i&i$\'>>>i&YCZHb~EYS.bYCYNaYCH`wU?~CwU?^\'>>i&YCYNb_wV%H_~CwV%^8S>H_~CwW;^D^~E^(^~YH^z!TM(^Bi$(^BZ=@^BYBiX3~Z%nbBi$(^Bi$(^BZ=@^BYBiX3~Z%nbBZ=D^BYBiXK~Z%mbYT:YSN_>i&i$(^Bi$(^BZ=@^BYBiX3~Z%nbBi$(^Bi$(^BZ=@^BYBiX3~Z%nbBZ=D^BYBiXK~Z%mbYT:YSN__~E_@_D^YT@_{!T:\'ASaG\'_^D^z\'A^~^bZ9i&:MiVHbYT+Ai&\'ASaG\'_^D^z\'A^~^bZ9i&:MiVHbYT+A^~^eai&kkYU,a^YS2^{!SN(^8<_G\'H_D^\'_^~YH^z~E^z!T@7&i$i&_A^[$G\'aZ$_7,c>b_@_7,YPAi&7,YPA^~^d@`a@_~i$7,c>b_@_7,YPAi&7,YPA^~^d@`a@_~CwV>D^~E^D^~E^|i$z!VH:kw(iUA]:(_\'Z:a@_D^~E^{])(^9)Jlb@`^(`~C_D_~E_|]E7&^6ZEd@bZN>Z2bi$`D`^~E_|]&6b6:Z&f@dbYS4w+aiUA~E@aD`^|!S4(_BYTBZ(YSA``^{!U\'#aYS4w*_iUA(_~CiVH_{!T*9&eca6YT*YU\'h2gh/ZN>Z2h/eh-@f@dZN>Z2di$b_`DaD_~E_})]78T*geab`^}(!S\'#`kiWH8D^~i$#`kiWH8D^~i$#`kiWH8D^~i$#`kiWH8D^~Z*YD^~Cw+O^~CiUAP^~Z*^z]O#YS\'a_iWC{]F#a_iUA#k_iUA~CiVH_{!6#b`iWH97f>i&>bwWE>i&aiXD`9EG9Fh.^Z)kZ2_dz_`~YH_@`97gaSbi1Sai-aNaH`~CwW*^9&c@a_~CwV\'^#ZFeYS4w0b#d~Z/Z2bZ9i&:Z&iVHNeZNZ:>>Z2gi$i$bckYG_iWHH`~CwW0^6`Hdb:a_iV.MdYNb`McZHa_~CwU?^6ZOg``b8S\'e6ZOh-aac8S\'e~YT)^~^Z;YSAc`~Ca^Z)lZ2b_YNaH`~CwV%^#cHaiWH~CwW;^D_~E_#d`iW2#dHH_iWH~i$#d`iW2#dHH_iWH~YT)^~^Z;YSA``Z)kZ2__~YH_|!TB8S7`O^{]N#YD`O__{!W)8D^z!SA-O^z]289^z!T+#b>i&`^|].8D^z]388^z!T789^z!S?iTJ!V.wU?!WHwV(!W2wV*!WCwW>!UAwWM]@(iY\'7%YIDa@`A^[$G8L_7)YPYPbeYID_@^~E^{i$YI`Z$^~E^{!U/(iXPy!U.-YU:y!U;8U%YPi&`^{!U%7(_c(i&~YM_kYGb[$G(^BZ,b_(^BZ,a_7/f@dc`BZ,ca7/@fdd`BZ,da~X0`^DbD`~Ea~E`}\'[%G(_(^7-d@ba`7-@dbba~X.`^D`D^~E_~E^{[&G7%a_A^[$G71_X2eeX1Ief_7)@`Il^~YU#k^{i$Z+m_(^BZ,i&^~YMl_{i$i$i${]=8S+BZ6^z!U&7$i&A^[$G7(>`^8LZ$_~ACf_7(>`^8LZ$_~A^~^ZG^Qbzi${!;/F`iX+_/__~YMvR$YS)ZBIlZ#`_(^~YMkZ#_{!T=7$IlZ#_A^[$G7\'Il^9PJl`kb~YMvR$YS)ZB_b(iWO~Kk^zi$z!U*7$IlZ#_A^[$G7\'Il^9PZ#d_b~YMvR#YS)ZB_b(iX1~Kk^zi$z]?8S#YT&`_iV9z!S%(^BYT(b_iV9YS@^FZKYU)iV9iX:z!V9YT/!U<-^z!U)8GD^z!T(8S7>Da>ca_9,b^~^ZMD__|!S#(a)^~^ZMD__|!T/\'i&i&y!W38U(^z!VL8SCly!VP8SCky!U:\'i&iY,y!A(_BYBiX\'BYB^BYBiXLBYBiY$BYBiX={!VC(i$z!UI(i$z]08SClBYS+BZ6_BYBiXEBYB^{!U18U1BYS+BZ6YTG^8S+~ZG^ZCBZIvCvR3y!TG7#YTI^z!TI99i&:MiVHai&kkz!VH:kw(iUA]:(_\'Z:a@_D^~E^{])(^9)Jlb@`^(`~C_D_~E_|]E9F`^Z)ka_@aD`6ZEd@b>ai$D`^~E_|!S\'#`kiWH8D^~i$#`kiWH8D^~i$#`kiWH8D^~i$#`kiWH8D^~Z*YD^~Cw+O^~CiUAP^~Z*^z]O#YS\'a_iWC{]F#a_iUA#k_iUA~CiVH_{]&6b6:Z&f@dbw+iUA~E@aD`^|]76Z&:h-w*iUA6Z&f~CiVHfd>aaa^}(!S6\'i&^z!S0\'YS6`^{!S3\'YS0b`^|]<\'YS3ca_wU?|!6#b`iWH97f>i&>bwWEawWE`9E>ea_`~YH_@`6ci$6cZ<>NdwW4>@HcwV\'DHa6c>@HbwV\'~CDHbwW7~E@a_~CwW4^6ci$6cA^6cAYS3Z<>NgwV5wWEwWEYS6YS0`wWEwW*~ENbHa~E@a_~CwV5^6ci%6cA^6cAZ<i$>NdwWD^~ENbHa~E@a_~CwWD^97fNdH`D_`DH`~CwW*^9&c@a_~CwV\'^#ZFew0#d~Z/bZ9i&:Z&iVHNeZ:>>fi$i$akYG_iWHH`~CwW0^6:MgZHecMfYNdbiV.Ha_~CwU?^6ZOdZ)lbHbYNa_~ACwVM_97f>i&>bwWEawWE`9E>ea_`~YH_@`6ci$6cZ<>NdwW4>@HcwV\'DHa6c>@HbwV\'~CDHbwW7~E@a_~CwW4^6ci$6cA^6cAYS3Z<>NgwV5wWEwWEYS6YS0`wWEwW*~ENbHa~E@a_~CwV5^6ci%6cA^6cAZ<i$>NdwWD^~ENbHa~E@a_~CwWD^97fNdH`D_`DH`~CwW*^9&c@a_~CwV\'^#ZFew0#d~Z/bZ9i&:Z&iVHNeZ:>>fi$i$akYG_iWHH`~CwW0^6:MgZHecMfYNdbiV.Ha_~CwU?^6ZOdZ)lbHbYNa_~A^~^CwV%^#cHaiWH~CwW;^D_~E_#bZ)k``iW2~YH_|!V.o!WHn!W2m!WCl!UAk]I8F_BYF^{!S+8Fuy!UEiF]\'(i$9\'a@_BYF^9\'a@_BYF^BYFvS#~ACvS#_9\'a@_BYF^9\'a@_BYF^BYFvS#~A^~^CvE^9\'a@_BYFvS;BYFvS#~Ct^9\'a@_BYFvS9BYFvS#~Cv0^9\'a@_BYFvS5BYFvS#~Cu^9\'a@_BYF^~L`D^~E^{!T#(i$(i$8T#@^BZ6D^~E^BYFvC~E^z!B8BZK^9IvS7vF~YTJ^96YS$^BYFvF~YS/^9\'i$YI^~YS&^8BYT&^~YH^8FvLBYT#@^BZ6D^BYFvK~E^9IvLvK~Z/^9IvS;vF~Ci%^9IvS-vF~L^z]68B^8FvEBZ\'i%YI^BYFvE~YS&^z!T$8T$8S*~Cu^(^~Kk^Qy!S*8S*BQ(^8T$~CvR0^~K_vC(iX5~ZG^Z>y]A9A>`^9A>a^9A>at~CvS;^9A>av0~CvS9^9A>au~CvS5^Q~CvS#^9$_~CvE^(i&~ZG^Qz!S(\'YS(^BQ(i&~AAKvD`\'YS(^BQ(i&~AA^~^CvL_\'YS(^BQ(i&~A^~^CvK^Z>y!SP\'YSP^ZC(i&BQ~CvL^YS*y!J(_8JIIvRL_YE`v3BQ~i$(_8JIIvRL_YE`v3BQ~KvS.^~K_vS\'8JIIvR,_YE`v3BQ~i$(_8JIIvRL_YE`v3BQ~i$(_8JIIvRL_YE`v3BQ~KvS.^~K_vS\'8JIIvR,_YE`v3BQ~KvR<^~K_vR58JIIvR%_YE`v3BQ~i$(_8JIIvRL_YE`v3BQ~i$(_8JIIvRL_YE`v3BQ~KvS.^~K_vS\'8JIIvR,_YE`v3BQ~i$(_8JIIvRL_YE`v3BQ~i$(_8JIIvRL_YE`v3BQ~KvS.^~K_vS\'8JIIvR,_YE`v3BQ~KvR<^~K_vR58JIIvR%_YE`v3BQ~KvR/^~K_vR$Z>z]C8S@`(^~^^YTN^YL>YS(^BQ8LZAi&BQ~CvE^\'>i&ZCwW;BQ~CvJ^8S1ZC2YJkk8JkBQ~CvP^Z>BQ~ACvRM_8S1ZC2YJkk8JkBQ~CvP^Z>BQ~A^~^CvS?^(i%BQ~CvS;^(i$BQ~CvS-^Z>BQ~CvF^8SPBQ~CvK^(^~Kk^YS*y]>(^!V3^Qy!T.(^!V3iX4(^~CiX5^!V3^z!:8T.^8T.YU7~CiX4^(^~CiX5^iV3y!V3iX4]G,iX5^z!W17%G(_BZLYDc^BYKPc^OOGi$zOOGi$z!S=(i$8S=@`^BX$D_~E_{!<(i&\'S@a_X$D_~E_{!V#i8!T-i9]9#l`^{!TJZDl!WA8KYS-aO_^{!V28S1YS-k^z!W,8T2b`P^|!U@8T9`P^{!WLi8!S$i9!S1#oYG_^z!S/ZDo!UK8KYS-aO_^{!VN8LYPi&YI^z!/8LYPYI`YI^{!T48La8T4>fZBbb`a_Il`~Ka_}\']P8T4i&b`^|!T1(k(iX5~E_(l8T1@b@`(l~K`^(iX5~K__D`D^~E_~E^{!S98T1YI`YI^{!UO5YTC`^{!WK5YTF`^{!TF4YS9a_k{!TC4kYS9`^{!T,,kYS9`^{!VK8LYS-vC^z!V18T2b`P^|]B8T9`P^{]#i8!Ii9!L#nYG_^z!S&ZDn!T5i(!S)i(!WGj%!VFiTH!W(iU#!UFi4!U+i,!T3(_(i$(i$8T3IIvR%`YEbu@_~KvR/^~K_vR$D^~E^{!T<8T3k^(i$~Z/^z!TN(i$2_k~^YT<^8T<@^~CvPD^(i$~Z/^YI^z!S5(^8S5_`~Kak>b^JIYEu``vR%Z+u^{]K8LYS5i&^8L>YS5i&I`kvP~Kk^z!U6(^8E__~YU8`YU6Z+m`YE_^(l~Ck_{!VDi(!V;i(!VGi(!W$i(!V?(lz!W\'i(!W<8E_Z+YU3``_YS,`YS,^(k~Ck_{!SB8SB_YTE__(_~Ck^{!U38SB`^8SB__~K__YS,`YS,^{!W83b^(^~CKkbKk`(k~Ck^IYE`a_Z+`^{!TE2YEZ+b``^{!S,(^2_k~Kk^z!V@(_(^~K`^{!W9(^(_~K`^{!U85YTK^z!TK,YEZ+m`m^z!VJ4k^z!UD4_kz!UC,k^z]%5K`^{!TH5K__{!U#4__{!Mi,!U55Z*^zBZ4ki#!UHOi#!T?(^!UH>iUH^YTP^8T?Oa_(^~T`O^P_~E_{!S@8T?iUH^z!UGiK!V/i9!T&i8!TP#m_i$z!HZDm!SK(`8SK>ca`Il^~K_k|!S-8SKi&`^{]M(i$9M@a_(^~TD__D_~E_{]-iTL!TL(i$8TL@a_(^~CD__D_~E_{!T6(i$8T6@`^(_~TD`^~E_{!V0iO!O(i$8O@`^(_~CD`^~E_{!SD(^8SDIl`@^~K`k{!T28S7aYSD`^|!T9-YSD`^{!SI(_8SI>aD_@^~E^{]$8SIi&^z!P(_\'YPa@_D^~E^{!G(k3YG@_l~E^z!SG9/^9/^8SG@a@^~E^(i$~YTOa^@^~E^{!W&8SG_^z]/,i&^z!W%8S.O^z!W58S.P^z!W/8SHO^z!V-8SHP^z!V+8SJO^z!UL8SJP^z!W?8T8O^z!WF8T8P^z!V88NP^z!W.8T0O^z!W-8T0P^z!UB8SMO^z!V:8SMP^z!V78T%O^z!WI8T%P^z!S.87O^z!SH87P^z!SJ8T\'O^z!T88T\'P^z!T01P^z!SM8SLO^z!T%8SLP^z!T\'88P^z!SL89P^z]H8NO^z!N1O^z!788O^z!189O^z],j4!S7iK!)i8!-i9!\'#k`^{!.ZDk!=(i$(i$(i$(i$8=PaP_~TOaO_~TYDaYD_~Z*`(i$~CpYD_~Z*_(^~^C`^{!TOi,!WB5_(^~^Ci%^z!5,i$^z]D0(i$,bYD^~Z*^zz!XC:nn:k:k:ki&vS4vS=vS9!X;:nl:ki&vP!Y#:nki&!XJ:np:k:k:k:k:ki&vR#vS4vS=vS9vR$!X$:np:k:k:k:k:ki&vR$vS;vS:vS6vS/!Y*:nki&!X8:nv::k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS4vS(vS9vS.vS6vS9vS7vCvS,vS/vS;vCvS-vS6vCvS,vS+vS6vS*vCvRBvRKvRG!X.:nv>:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vDvRDvRAvRAvR:vR=vCvS:vS;vS5vS0vS9vS7vCvS;vS(vS/vS;vCvS,vS+vS6vS*vCvRBvRKvRG!XH:nl:ki&vO!XA:nl:ki&vO!WN:nl:ki&vC!X*:nl:ki&vC!XO:nvR$:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS@vR+vS=vS2vS3vR/vJvDvS4vS2vS3vR/vKvDvR2vRGvS=vR3vR4vR/vRGvS=vR3vR4vR6vRGvS=vR3vR4vR6vRGvS=vR3vR4vR9vRGvS=vR3vR4vR9vS=vR3vR4vS<vJvR0vL!Y&:nn:k:k:ki&vS4vS=vS9!XB:nr:k:k:k:k:k:k:ki&vS@vS-vS0vS5vS0vS4vR$!XI:np:k:k:k:k:ki&vR$vS;vS:vS6vS/!X0:nn:k:k:ki&vS4vS=vS9!X9:nr:k:k:k:k:k:k:ki&uvS:vS,vS;vS@vS)vC!Y+:nv8:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vCvR/vS/vS;vS.vS5vS,vS3vCvS,vS+vS6vS*vCvRBvRKvRGvCvMvMvM!X&:nki&!X(:nn:k:k:ki&vS4vS=vS9!Y-:nl:ki&vP!Y%:nn:k:k:ki&vS)vS0vS3!XM:no:k:k:k:ki&vS4vS*vS:vR#!XF:nki&!X?:nl:ki&vR0!X>:nl:ki&vO!X6:nki&!X,:nki&!WP:nki&!X):k:k:k:ki&w&w%w$w#!X2:nv/:k:k:k:k:k:k:k:k:k:k:k:ki&vS+vS,vS;vS*vS,vS7vS?vS,vCvS)vS0vS9!X7:nu:k:k:k:k:k:k:k:k:k:ki&vS7vS6vCvS5vS>vS6vS5vS2vS5vS<!X@:nv5:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS:vS5vS6vS*vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!XG:nv3:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS,vS.vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!XN:nv3:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS,vS:vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!Y(:nv4:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS3vS3vS(vS*vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!Y):nv4:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS7vS4vS<vS1vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!X#:nl:ki&vS&!X-:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&:k:ki&v7wSC:k:ki&v6wF:k:ki&v5wU7:k:ki&v4x+:k:ki&v3wE:k:ki&v2w2:k:ki&v1w3:k:ki&v0w4:k:ki&v/w,:k:ki&v.xL:k:ki&ux4:k:ki&twK:k:ki&swD:k:ki&rw8:k:ki&qw9:k:ki&px*:k:ki&ow0:k:ki&nw*:k:ki&mw+:k:ki&lw(:k:ki&kw#!X/:nv7:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS5vS(vS;vS:vS5vS6vS*vCvS+vS3vS0vS<vS)vCvS;vJvS5vS(vS*!X%:k:k:ki&i&i%i$!X<:ki&k!X3:nv0:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR/vS:vS;vS9vS6vS7vS?vS,vCvMvMvM!XK:nv1:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR/vS,vS+vS6vS*vCvRBvRKvRGvCvMvMvM!XD:ki&wWE!Y\':nki&!XP:nki&!X+:nl:ki&vR$!WO:nm:k:ki&vR$vR#!X1:nki&!X::nn:k:k:ki&vR5vR5vR5!Y,:nki&!X\':nv.:k:k:k:k:k:k:k:k:k:k:ki&uvR#vS@vS3vS3vS(vS<vS5vS(vS4vC!XL:nvE:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vCvS.vS5vS0vS5vS5vS<vS9vCvS@vS9vS;vCvS6vS;vCvS;vS5vS(vS>vCvS;vS/vS.vS0vS4vCvS<vS6vRNvCvMvMvM!Y$:nvO:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR#vS+vS,vS0vS-vS0vS5vS0vS4vCvS;vS6vS5vCvS:vS(vS>vCvS,vS+vS6vS*vCvS+vS,vS;vS(vS9vS,vS5vS,vS.vCvS,vS/vS;vCvS6vS:vCvMvMvM!X=:nvR/:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvS4vS,vS;vS:vS@vS:vCvS,vS4vS,vS/vS*vRHvCvS:vS0vS/vS;vCvS/vS;vS0vS>vCvS+vS,vS;vS9vS6vS7vS7vS<vS:vCvS;vS6vS5vCvS:vS0vCvS5vS6vS0vS;vS(vS*vS0vS-vS0vS5vS0vRBvCvMvMvM!XE:nl:ki&vC!X4Imk!X5Ilk!(:lkl!+:lkm!*:lkn!0:lko]*:lkp!9:lkq!8:lkr!D:lks!K:lkt]4:lku]L:lkv.!,:lkv/!4:lkv0!3:lkv1!2:lkv2!E:lkv3]+:lkv4!U7:lkv5!F:lkv6!SC:lkv7y" -- RVM code that prints HELLO!


----------------------
-- Helper functions --
----------------------
error : String -> HasIO io => io (a)
error str = do putStrLn ("*** error : " ++ str)
               exitFailure

getAndIncrement : IORef Int -> HasIO io => io (Int)
getAndIncrement ref = do v <- readIORef ref
                         modifyIORef ref (+ 1)
                         pure v

--------------------
-- Rib Definition --
--------------------
mutual
 -- DELICIOUS RIBS
 data Rib = RInt Int | RRef (IORef RibContainer)
 record RibContainer where
     constructor MakeRibContainer
     id : Int
     -- We called the third field c(g)r because a(1) --+3--> d(4) --+3--> g(7)
     car, cdr, cgr : Rib

(==) : Rib -> Rib -> HasIO io => io Bool
(==) (RInt n) (RInt m) = do pure (n == m)
(==) (RRef ref1)  (RRef ref2) = do rib1 <- readIORef ref1
                                   rib2 <- readIORef ref2
                                   pure (rib1.id == rib2.id)
(==) _ _  = do pure False


MakeRib : IORef Int ->  Rib -> Rib -> Rib -> HasIO io => io (Rib)
MakeRib ribCounter e1 e2 e3 = do v <- getAndIncrement ribCounter
                                 ref <- newIORef (MakeRibContainer v e1 e2 e3)
                                 pure (RRef ref)

eb2 : Int
eb2 = 46

getByte : IORef Int -> HasIO io => io (Int)
getByte posRef = do pos <- getAndIncrement posRef
                    pure (cast {to=Int} (assert_total (strIndex input pos)))

getCode : Int -> Int
getCode b =
  let x = b - 35 in
  if x < 0 then 57 else x

getInt : IORef Int -> HasIO io => Int -> io (Int)
getInt posRef n = do
  x <- (getByte posRef)
  let x = getCode x
  let y = n * eb2   --  <- pure (n * eb2)
  if x < eb2 then
    pure (y + x)
   else
    getInt posRef (y + (x - eb2))




rPairType : Int
rPairType = 0

rProcedureType : Int
rProcedureType = 1

rSymbolType : Int
rSymbolType = 2

rStringType : Int
rStringType = 3

rVectorType : Int
rVectorType = 4

rSingletonType : Int
rSingletonType = 5

rToInt : Rib -> HasIO io => io (Int)
rToInt (RInt n) = do pure n
rToInt _ = do error "Cannot cast rib to int"

rIsInstance : Int -> Rib -> HasIO io => io (Bool)
rIsInstance type (RRef rib) =
  do rib <- readIORef rib
     val <- rToInt rib.cgr
     pure (val == type)
rIsInstance _ _ = do pure False

rIsPair : Rib -> HasIO io => io Bool
rIsPair = rIsInstance rPairType

rId : Rib -> HasIO io => io Int
rId (RRef rib) = do v <- readIORef rib
                    pure (RibContainer.id v)
rId _ = do error "Cannot id on number"

rCar : Rib -> HasIO io => io Rib
rCar (RRef rib) = do v <- readIORef rib
                     pure (RibContainer.car v)
rCar _ = do error "Cannot car on number"

rCdr :  Rib -> HasIO io => io (Rib)
rCdr (RRef rib) = do v <- readIORef rib
                     pure (RibContainer.cdr v)
rCdr _ = do error "Cannot cdr on number"

-- extension to acces the third field
rCgr : Rib -> HasIO io => io Rib
rCgr (RRef rib) = do v <- readIORef rib
                     pure (RibContainer.cgr v)
rCgr _ = do error "Cannot cgr on number"

rListTail : Rib -> Int -> HasIO io => io (Rib)
rListTail rib i =
  if 0 < i then -- we took 2h to find that its i < 0 instead of 0 < i...
        do
           cdr <- rCdr rib
           rListTail cdr (i - 1)
   else
     do pure rib

rLength : Rib -> HasIO io => io (Int)
rLength list = do
  isPair <- rIsPair list
  if isPair
    then do
       cdr <- rCdr list
       l <- rLength cdr
       pure (1 + l)
    else do pure 0

-----------
-- State --
-----------

record State where
  constructor MkState
  true : Rib
  false : Rib
  nil : Rib
  pos : IORef Int
  globalCounter : IORef Int
  symtbl : Rib

-- really not optimal...
toString : State -> Rib -> IORef (List Int) -> HasIO io => io (String)
toString state (RInt n) _ = do pure (cast n)
toString state rib seen = do isNil <- rib == state.nil
                             isTrue <- rib == state.true
                             isFalse <- rib == state.false

                             if isNil
                                then do pure "RNIL"
                                else if isTrue
                                  then do pure "RTRUE"
                                  else if isFalse
                                    then do pure "RFALSE"
                                    else do car <- rCar rib
                                            cdr <- rCdr rib
                                            cgr <- rCgr rib
                                            id <- rId rib
                                            seenLst <- readIORef seen
                                            if elem id seenLst
                                              then if id > 99 then do pure ("#:" ++ (cast id)) else if id > 9 then do pure ("#: " ++ (cast id)) else do pure ("#:  " ++ (cast id))
                                              else do modifyIORef seen (\x => (id :: x))
                                                      scar <- toString state car seen
                                                      scdr <- toString state cdr seen
                                                      scgr <- toString state cgr seen
                                                      pure (
                                                       --"#:" ++ (cast id) ++
                                                       "[" ++ scar ++ ", " ++ scdr ++ ", " ++ scgr ++ "]")


print : State -> Rib -> HasIO io => io ()
print state rib = do x <- newIORef ([0, 0, 0])
                     str <- toString state rib x
                     putStrLn ("" ++ str ++ "\n")



rStringToUninternedSymbol : State -> Rib -> HasIO io => io (Rib)
rStringToUninternedSymbol state string = MakeRib state.globalCounter state.false string (RInt rSymbolType)

rListToString : State -> Rib -> HasIO io => io (Rib)
rListToString state list = do l <- rLength list
                              MakeRib state.globalCounter list (RInt l) (RInt rStringType)

rCons : State -> Rib -> Rib -> HasIO io => io (Rib)
rCons state car cdr = MakeRib state.globalCounter car cdr (RInt rPairType)


rIsRib : Rib -> Bool
rIsRib (RRef _) = True
rIsRib (RInt n) = False

setCar : Rib -> Rib -> HasIO io => io ()
setCar (RRef stack) newval = do ref <- readIORef stack
                                let v = (MakeRibContainer ref.id newval ref.cdr ref.cgr )
                                writeIORef stack v
                                --modifyIORef stack (\ref => { car := newval } ref)
setCar _ _ = do error "First argument of setcar is not a rib"

setCdr : Rib -> Rib -> HasIO io => io ()
setCdr (RRef stack) newval = do ref <- readIORef stack
                                let v = (MakeRibContainer ref.id ref.car newval ref.cgr )
                                writeIORef stack v
setCdr _ _ = do error "Second argument of setcar is not a rib"

setCgr : Rib -> Rib -> HasIO io => io ()
setCgr (RRef stack) newval = do ref <- readIORef stack
                                let v = (MakeRibContainer ref.id ref.car ref.cdr newval)
                                writeIORef stack v
setCgr _ _ = do error "Third argument of setcar is not a rib"

rSetRef : Rib -> Rib -> HasIO io => io()
rSetRef (RRef elem1) (RRef elem2) = do v <- readIORef elem2
                                       writeIORef elem1 v
                                       putStrLn "Ref has been modified"
                                       --modifyIORef elem1 (\ref => v)
rSetRef _ _ = do error "Cannot setref of non ref elements"

rToRef : Rib -> HasIO io => io (IORef RibContainer)
rToRef (RRef rib) = do pure rib
rToRef _ = do error "cannot cast int to IORef"

------------------
-- Symbol table --
------------------

addSymbol : State -> Rib -> Rib -> HasIO io => io (Rib)
addSymbol state chars symtbl = do lst <- rListToString state chars
                                  sym <- rStringToUninternedSymbol state lst
                                  rCons state sym symtbl


buildChar : State -> Rib -> Rib -> HasIO io => io (Rib)
buildChar state chars symtbl = do x <- getByte state.pos
                                  if x == 44
                                    then do sym <- addSymbol state chars symtbl
                                            buildChar state state.nil sym
                                    else if x == 59
                                      then addSymbol state chars symtbl
                                      else do r <- rCons state (RInt x) chars
                                              buildChar state r symtbl

buildSymtbl : State -> Int -> Rib -> HasIO io => io (Rib)
buildSymtbl state n symtbl = if 0 < n
                                then do sym <- addSymbol state state.nil symtbl
                                        buildSymtbl state (n - 1) sym
                                else do buildChar state state.nil symtbl


---------------------------
-- Inscturction decoding --
---------------------------


sym : State -> Int -> HasIO io => io Rib
sym state n = do tail <- rListTail state.symtbl n
                 r <- rCar tail
                 --print state r
                 rCar tail

ins : List Int
ins = [20, 30, 0, 10, 11, 4]

unsafeIndex : List Int -> Int -> Int
unsafeIndex lst n = case lst of
    (head :: tail) => if n == 0 then head else unsafeIndex tail (n-1)
    _ => -1

mutual
  addInstruction : State -> Rib -> Rib -> Rib -> HasIO io => io (Rib)
  addInstruction state op opnd stack = do oscar <- rCar stack
                                          newRib <- MakeRib state.globalCounter op opnd oscar
                                          setCar stack newRib
                                          decodeStack state stack

  decodeStackAux : State -> Int -> Int -> Int -> Rib -> HasIO io => io Rib
  decodeStackAux state op n x oriStack =
    let d = (unsafeIndex ins op) in
        if d + 2 < n
             then decodeStackAux state (op+1) (n-(d+3)) x oriStack
             else
               if 90 < x then
                 do cdrStack <- rCdr oriStack
                    carStack <- rCar oriStack
                    addInstruction state (RInt 4) carStack cdrStack
               else
                 do stack <- if op == 0 then rCons state (RInt 0) oriStack else do pure oriStack
                    opnd <- if (n < d)
                               then if (op < 3)
                                    then sym state n
                                    else do pure (RInt n)
                               else if n == d
                                    then do v <- getInt state.pos 0
                                            pure (RInt v)
                                    else do v <- getInt state.pos ((n - d) - 1)
                                            sym state v
                    if 4 < op
                      then do scar <- rCar stack
                              inter <- MakeRib state.globalCounter opnd (RInt 0) scar
                              proc <- MakeRib state.globalCounter inter state.nil (RInt rProcedureType)
                              stack <- rCdr stack
                              if rIsRib stack
                                then do (addInstruction state (RInt 3) proc stack)
                                else do pure proc
                      else do addInstruction state
                                             (if (0 < op) then (RInt (op - 1)) else (RInt 0))
                                             opnd
                                             stack

  decodeStack : State -> Rib -> HasIO io => io Rib
  decodeStack state stack = do x <- (getByte state.pos)
                               let x = getCode x

                               decodeStackAux state 0 x x stack


----------------
-- Primitives --
----------------

prim3 : State -> (Rib -> Rib -> Rib -> HasIO io => io Rib) -> Rib -> HasIO io => io Rib
prim3 state foo stack = do z <- rCar stack
                           stack <- rCdr stack
                           y <- rCar stack
                           stack <- rCdr stack
                           x <- rCar stack
                           stack <- rCdr stack
                           newVal <- foo x y z
                           rCons state newVal stack

prim2 : State -> (Rib -> Rib -> HasIO io => io Rib) -> Rib -> HasIO io => io Rib
prim2 state foo stack = do y <- rCar stack
                           stack <- rCdr stack
                           x <- rCar stack
                           stack <- rCdr stack
                           newVal <- foo x y
                           rCons state newVal stack


prim1 : State -> (Rib -> HasIO io => io Rib) -> Rib -> HasIO io => io Rib
prim1 state foo stack = do x <- rCar stack
                           stack <- rCdr stack
                           newVal <- foo x
                           rCons state newVal stack

prim1Pure : State -> (Rib -> Rib) -> Rib -> HasIO io => io Rib
prim1Pure state foo stack = do x <- rCar stack
                               stack <- rCdr stack
                               let newVal = foo x
                               rCons state newVal stack

prim2Pure : State -> (Rib -> Rib ->  Rib) -> Rib -> HasIO io => io Rib
prim2Pure state foo stack = do y <- rCar stack
                               stack <- rCdr stack
                               x <- rCar stack
                               stack <- rCdr stack
                               let newVal = foo x y
                               rCons state newVal stack

prim0 : State -> Rib -> Rib -> HasIO io => io Rib
prim0 state return stack = do rCons state return stack

opRInt : (Int -> Int -> Int) -> Rib -> Rib -> Rib
opRInt op (RInt n) (RInt m) = (RInt (op n m))
opRInt _ _ _ = (RInt (0 - 999)) --lol

rEqv : State -> Rib -> Rib -> HasIO io => io Rib
rEqv state x y = do isTrue <- (x == y)
                    if isTrue then do pure state.true else do pure state.false

lessthan : State -> Rib -> Rib -> HasIO io => io Rib
lessthan state (RInt n) (RInt m) = if n < m then do pure state.true else do pure state.false
lessthan _ _ _ = do error "Less than on ribs (non-numbers)"

arg1 : Rib -> Rib -> HasIO io => io Rib
arg1 x y = do pure x

arg2 : Rib -> Rib -> HasIO io => io Rib
arg2 x y = do pure y

close : State -> Rib -> HasIO io => io Rib
close state stack = do x <- rCar stack
                       stack <- rCdr stack
                       xCar <- rCar x
                       newRib <- MakeRib state.globalCounter xCar stack (RInt rProcedureType)
                       rCons state newRib stack

setAndReturn : (Rib -> Rib -> HasIO io => io ()) -> Rib -> Rib -> HasIO io => io Rib
setAndReturn foo x y = do foo x y
                          pure y

writeAndReturn : Rib -> HasIO io => io Rib
writeAndReturn rib = case rib of
                      RInt n => do let x = (cast {to=Char} n)
                                   putChar x
                                   pure rib
                      _ => do error "this is bad :/"

primitive : State -> Int -> Rib -> HasIO io => io Rib
primitive state 0 stack = do let globalCounter = state.globalCounter
                             ribCreator <- pure (MakeRib globalCounter)
                             prim3 state ribCreator stack
primitive state 1 stack = do pure stack
primitive state 2 stack = do rCdr stack
primitive state 3 stack = do prim2 state arg2 stack
primitive state 4 stack = do close state stack

primitive state 5 stack = do prim1Pure state (\x => if rIsRib x then state.true else state.false) stack
primitive state 6 stack = do prim1 state rCar stack
primitive state 7 stack = do prim1 state rCdr stack
primitive state 8 stack = do prim1 state rCgr stack
primitive state 9 stack = do foo <- pure (setAndReturn setCar)
                             prim2 state foo stack
primitive state 10 stack = do foo <- pure (setAndReturn setCdr)
                              prim2 state foo stack
primitive state 11 stack = do foo <- pure (setAndReturn setCgr)
                              prim2 state foo stack
primitive state 12 stack = do foo <- pure (rEqv state)
                              prim2 state foo stack
primitive state 13 stack = do foo <- pure (lessthan state)
                              prim2 state foo stack
primitive state 14 stack = do prim2Pure state (opRInt (+)) stack
primitive state 15 stack = do prim2Pure state (opRInt (-)) stack
primitive state 16 stack = do prim2Pure state (opRInt (*)) stack
primitive state 17 stack = do prim2Pure state (opRInt div) stack
primitive state 18 stack = do pos <- readIORef state.pos
                              if pos < strLength input
                                then do v <- getByte state.pos
                                        if v == 255
                                          then do prim0 state (RInt (-1)) stack
                                          else do prim0 state (RInt v) stack
                                else do x <- getChar
                                        let v = (cast {to=Int} x)
                                        if v == 255
                                          then do prim0 state (RInt (-1)) stack
                                          else do prim0 state (RInt v) stack
primitive state 19 stack = do prim1 state writeAndReturn stack



primitive _ n _ = do error ("Primitive #" ++ (cast n) ++ " is not yet implemented ")

-------------------
-- Globals setup --
-------------------

setGlobal : Rib -> Rib -> HasIO io => io (Rib)
setGlobal symtbl val = do symCar <- rCar symtbl
                          setCar symCar val
                          rCdr symtbl

setupGlobal : State -> HasIO io => io (Rib)
setupGlobal state =
  do primitive0 <- MakeRib state.globalCounter (RInt 0) state.symtbl (RInt rProcedureType)
     let symtbl = state.symtbl
     symtbl <- setGlobal symtbl primitive0
     symtbl <- setGlobal symtbl state.false
     symtbl <- setGlobal symtbl state.true
     setGlobal symtbl state.nil

-----------------------
-- Runtime execution --
-----------------------

getCont : Rib -> HasIO io => io Rib
getCont stack = do stackCgr <- rCgr stack
                   stackCdr <- rCdr stack
                   if rIsRib stackCgr
                    then do pure stack
                    else do getCont stackCdr

getVar : Rib -> Rib -> HasIO io => io Rib
getVar stack (RInt n) = do stackTail <- rListTail stack n
                           stackTailCar <- rCar stackTail
                           pure stackTailCar
getVar stack opnd = do rCar opnd

setVar : Rib -> Rib -> Rib -> HasIO io => io ()
setVar stack (RInt n) val = do stackTail <- rListTail stack n
                               stackTailCar <- rCar stackTail
                               setCar stackTail val
setVar stack opnd val = do setCar opnd val


mutual
  lmdaCall : State -> Rib -> Rib -> Int -> Rib -> Rib -> Rib -> Int -> HasIO io => io ()
  lmdaCall state code next nargs newStack newCont stack id =
    do let isNextRib = rIsRib next
       if 0 < nargs
          then do let nextNargs = nargs - 1
                  stackCar <- rCar stack
                  stackCdr <- rCdr stack
                  superNewStack <- rCons state stackCar newStack
                  lmdaCall state code next nextNargs superNewStack newCont stackCdr id
          else if isNextRib
                  then do setCar newCont stack
                          setCgr newCont next
                          codeCgr <- rCgr code
                          run state codeCgr newStack (id + 1)

                  else do k <- getCont stack
                          k0 <- rCar k
                          k2 <- rCgr k
                          setCar newCont k0
                          setCgr newCont k2
                          codeCgr <- rCgr code
                          run state codeCgr newStack (id + 1)


  run : State -> Rib -> Rib -> Int -> HasIO io => io ()
  run state pc stack id =
    do instr <- rCar pc
       opnd <- rCdr pc
       next <- rCgr pc
       v <- (rToInt instr)

       case instr of
        RInt 0 => do proc <- getVar stack opnd --Jump/Call
                     code <- rCar proc
                     codeIsRib <- pure (rIsRib code)
                     if codeIsRib
                       then do
                               ribCreator <- pure (MakeRib state.globalCounter) --lambda
                               newCont <- ribCreator (RInt 0) proc (RInt 0)
                               codeCar <- rCar code
                               codeCarInt <- rToInt codeCar
                               lmdaCall state code next codeCarInt newCont newCont stack id
                       else do codeInt <- rToInt code
                               stack <- primitive state codeInt stack --primitive
                               nextIsRib <- pure (rIsRib next)
                               output <- if nextIsRib
                                 then do pure next
                                 else do cont <- getCont stack
                                         contCar <- rCar cont
                                         setCdr stack contCar
                                         rCgr cont
                               run state output stack (id + 1)
        RInt 1 => do stackCar <- rCar stack --Set
                     stackCdr <- rCdr stack
                     setVar stack opnd stackCar
                     run state next stackCdr (id + 1)
        RInt 2 => do opndVar <- getVar stack opnd --Get
                     newStack <- rCons state opndVar stack
                     run state next newStack (id + 1)
        RInt 3 => do newStack <- rCons state opnd stack --Const
                     run state next newStack (id + 1)
        RInt 4 => do stackCar <- rCar stack --If
                     stackCdr <- rCdr stack
                     isFalse <- stackCar == state.false
                     if isFalse
                       then do run state next stackCdr (id + 1)
                       else do run state opnd stackCdr (id + 1)
        RInt 5 => do putStr ""
        _ => do error "Unknown instruction"


--------------------------------------
-- Decode instructions and run them --
--------------------------------------

decodeAndRun : IO ()
decodeAndRun = do
  globalCounter <- newIORef 0 -- only used for the ribCreator
  ribCreator <- pure (MakeRib globalCounter)

  true <- ribCreator (RInt 0) (RInt 0) (RInt rSingletonType)
  false <- ribCreator (RInt 0) (RInt 0) (RInt rSingletonType)
  nil <- ribCreator (RInt 0) (RInt 0) (RInt rSingletonType)

  pos <- newIORef 0
  let symtbl = nil

  -- Create state
  let state = MkState true false nil pos globalCounter nil

  v <- (getInt pos 0)
  symtbl <- buildSymtbl state v state.symtbl

  state <- pure (MkState state.true state.false state.nil state.pos state.globalCounter symtbl)
  stack <- decodeStack state (RInt 0)
  symtbl <- setupGlobal state

  x <- rCar stack
  pc <- rCgr x

  y <- ribCreator (RInt 5) (RInt 0) (RInt 0)
  stack <- ribCreator (RInt 0) (RInt 0) y

  run state pc stack 0

-- :)
main : IO ()
main = do
  decodeAndRun
