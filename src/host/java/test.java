import java.nio.charset.StandardCharsets;
import java.io.IOException;

class test
{

  public test(String input, boolean debugging)
  {
    //System.out.println("input = "+input);
    this.input = input;
    //this.input = "R:fi,adbmal,enifed,*,,,,,,,,,-,<,,,,,,,,,,,,,,,,,,,,;92]292AZ4AYJZA^94~Z(^YMAVvCvR3y]A7#ZL^z]L9#i&:BiS)ai&kkz!S):kw)k]))_(Z)aC_F^~E^{!@)^8@Z-lbC`^)`~>_F_~E_|]$9.`^Wka_CaF`+Z$dCb?ai$F`^~E_|]D#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~SL^~>w*K^~>kT^~S^z];#ZDa_l{].#a_k#k_k~>iS)_{!9+b+:PfCdbw*k~ECaF`^|!H+P:h-w1k+Pf~>iS)fd?aaa^}(!P(i&^z]%(YP`^{]'(Z%b`^|!:(Z'ca_wD|!+#b`n8Hf?i&?bxOaxO`9$?ea_`~YN_C`+ci$+cQ?JdwS'?CDcxNFDa+c?CDbxN~>FDbwS&~ECa_~>wS'^+ci$+cH^+cHZ'Q?JgwS(xOxOYPZ%`xOwS%~EJbDa~ECa_~>wS(^+ci%+cH^+cHQi$?JdwS#^~EJbDa~ECa_~>wS#^8HfJdD`F_`FD`~>wS%^89cCa_~>xN^#Z.exJ#d~YEbZ#i&:PiS)JeZ)??fi$i$akYF_nD`~>wC^+:BgZ<ecBfUdboDa_~>wD^+Z;dWlbDbUa_~H>wB_8Hf?i&?bxOaxO`9$?ea_`~YN_C`+ci$+cQ?JdwS'?CDcxNFDa+c?CDbxN~>FDbwS&~ECa_~>wS'^+ci$+cH^+cHZ'Q?JgwS(xOxOYPZ%`xOwS%~EJbDa~ECa_~>wS(^+ci%+cH^+cHQi$?JdwS#^~EJbDa~ECa_~>wS#^8HfJdD`F_`FD`~>wS%^89cCa_~>xN^#Z.exJ#d~YEbZ#i&:PiS)JeZ)??fi$i$akYF_nD`~>wC^+:BgZ<ecBfUdboDa_~>wD^+Z;dWlbDbUa_~H^~^>wS*^#cDan~>xP^F_~E_#bWk``m~YN_|!?0_AG^{]40uy!6)i$6aC_AG^6aC_AG^AGvS#~H>vS#_6aC_AG^6aC_AG^AGvS#~H^~^>vE^6aC_AGvS5AGvS#~>u^6aC_AG^~Z6`F^~E^{]3)i$)i$93C^AYJF^~E^AGvC~E^z]&9&Z?^8?vS7vF~Z@^8JZB^AGvF~ZG^6i$Z+^~Z/^9&ZF^~YN^0vLAZ3C^AYJF^AGvK~E^8?vLvK~YE^8?vS;vF~>i%^8?vS-vF~Z6^z!J9&^0vEAMi%Z+^AGvE~Z/^z]:9:8K~>u^)^~Nk^Iy!K8KAI)^9:~>vR0^~N_vC)iS$~Z(^Z*y!I8I?`^8I?a^8I?au~>vS5^I~>vS#^9K_~>vE^)i&~Z(^Iz!G(YG^AI)i&~HHNvD`(YG^AI)i&~HH^~^>vL_(YG^AI)i&~H^~^>vK^Z*y]1(Z1^YM)i&AI~>vL^YKy!M9>`)^~^^ZC^YO?YG^AI8OYIi&AI~>vE^(?i&YMxPAI~>vJ^9EYM)i%AI~>vS;^)i$AI~>vS-^Z*AI~>vF^91AI~>vK^)^~Nk^YKy]*)^!S,^Iy]7)^!S,iS+)^~>iS$^!S,^z!297^97ZI~>iS+^)^~>iS$^iS,y!S,iS+]('iS$^z]##l`^{]@Rl]Bi=]E#oYF_^z]GRo]+i=!O#nYF_^z]/Rn]0)_)i$)i$90OOvR%`YAbuC_~NvR/^~N_vR$F^~E^{]890k^)i$~YE^z]C)i$88_k~^Z8^98C^~>vPF^)i$~YE^Z+^z],)^9,_`~Nak?b^Z-OYAu``vR%Z=u^{]?8OZ,i&^8O?Z,i&O`kvP~Nk^zAZHki#!S-Ki#]5)^!S-?iS-^ZM^95Ka_)^~YL`K^T_~E_{]>95iS-^z]Fi4]M#m_i$z!NRm]9)_99?aF_C^~E^{]K99i&^z!F)k9-YFC_l~E^z!E'i&^z]<8>K^z!>-K^z!34K^z!-8=K^z!,i4!/i=!(#k`^{!.Rk!L)i$)i$)i$)i$8LTaT_~YLKaK_~YLLaL_~S`)i$~>pL_~S_)^~^>`^{]6'i$^z!;9J)i$'bL^~S^zz!S+Omk!S$Olk!):lkl!*:lkm!1:lkn]J:lko!<:lkp!=:lkq!4:lkr!5:lks]H:lku!':lkv/!7:lkv0]-:lkv1!8:lkv2!A:lkv3]=:lkv4]I:lkv5!0:lkv6y";
    this.debug = debugging;
    //System.exit(0);
  }

  private abstract class Field
  {
    public abstract Object getField();
    public abstract boolean isInteger();

    @Override
    public abstract String toString();
  }

  private class IntField extends Field
  {
    int value;

    public IntField(int val)
    {
      value = val;
    }

    @Override
    public Object getField()
    {
      return value;
    }

    @Override
    public boolean isInteger()
    {
      return true;
    }

    @Override
    public boolean equals(Object that)
    {
      if(!(that instanceof IntField))
      {
        return false;
      }
      else
      {
        return this.value == ((IntField) that).value;
      }
    }

    public boolean equals(int that)
    {
      return this.value == that;
    }

    @Override
    public String toString()
    {
      return Integer.toString(value);
    }
  }

  private class RibField extends Field
  {
    Rib reference;

    public RibField(Rib ref)
    {
      reference = ref;
    }

    @Override
    public Object getField()
    {
      return reference;
    }

    @Override
    public boolean isInteger()
    {
      return false;
    }

    @Override
    public boolean equals(Object that)
    {
      if(!(that instanceof RibField))
      {
        return false;
      }
      else if (this.reference == ((RibField) that).reference)
      {
        return true;
      }
      else
      {
        return (this.reference.getField0().equals(((RibField) that).reference.getField0()) &&
                this.reference.getField1().equals(((RibField) that).reference.getField1()) &&
                this.reference.getField2().equals(((RibField) that).reference.getField2()));
      }
    }

    @Override
    public String toString()
    {
      return "RibField";
    }
  }

  private class Rib
  {
    Field[] fields;

    public Rib(Field field0, Field field1, Field field2)
    {
      fields = new Field[3];
      setField0(field0);
      setField1(field1);
      setField2(field2);
    }



    public Field getField0() {
      return fields[0];
    }


    public Field getField1() {
      return fields[1];
    }


    public Field getField2() {
      return fields[2];
    }


    public void setField0(Field field0) {
      if(field0 != null)
      {
        if (field0.isInteger()) {
          fields[0] = new IntField((int) field0.getField());
        } else {
          fields[0] = new RibField((Rib) field0.getField());
        }
      }
      else
      {
        fields[0] = null;
      }
    }

    public void setField1( Field field1) {
      if(field1 != null)
      {
        if (field1.isInteger()) {
          fields[1] = new IntField((int) field1.getField());
        } else {
          fields[1] = new RibField((Rib) field1.getField());
        }
      }
      else
      {
        fields[1] = null;
      }
    }

    public void setField2(Field field2) {
      if(field2 != null) {
        if (field2.isInteger()) {
          fields[2] = new IntField((int) field2.getField());
        } else {
          fields[2] = new RibField((Rib) field2.getField());
        }
      }
      else
      {
        fields[2] = null;
      }
    }

    @Override
    public boolean equals(Object that)
    {
      if(!(that instanceof Rib))
      {
        return false;
      }
      else if(this == (Rib)that)
      {
        return true;
      }
      else
      {
        return (this.fields[0].equals(((Rib) that).getField0()) &&
                this.fields[1].equals(((Rib) that).getField1()) &&
                this.fields[2].equals(((Rib) that).getField2()));
      }
    }

    @Override
    public String toString()
    {
      return "["+getField0().toString() + ", " + getField1().toString() + ", " + getField2().toString()+"]";
    }
  }

  final IntField pair_tag = new IntField(0);
  final IntField procedure_tag = new IntField(1);
  final IntField symbol_tag = new IntField(2);
  final IntField string_tag = new IntField(3);
  final IntField vector_tag = new IntField(4);
  final IntField singleton_tag = new IntField(5);

  final int jump_call_code = 0;
  final int set_code = 1;
  final int get_code = 2;
  final int const_code = 3;
  final int if_code = 4;
  final int halt_code = 5;

  private final Rib FALSE = new Rib(new IntField(0),new IntField(1), singleton_tag);
  private final Rib TRUE = new Rib(new IntField(1),new IntField(1), singleton_tag);
  private final Rib NIL = new Rib(new IntField(0),new IntField(0), singleton_tag);
  private final Rib EMPTY = new Rib(new IntField(-1),new IntField(0),singleton_tag);


  Rib stack=EMPTY, pc;
  Rib symbolTable = NIL;

  int pos=0;
  String input;
  boolean debug;

  public static void main(String[] args)
    {
      test ribbit;
      if(args.length == 0)
      {
         ribbit = new test("R:fi,adbmal,enifed,*,,,,,,,,,-,<,,,,,,,,,,,,,,,,,,,,;92]292AZ4AYJZA^94~Z(^YMAVvCvR3y]A7#ZL^z]L9#i&:BiS)ai&kkz!S):kw)k]))_(Z)aC_F^~E^{!@)^8@Z-lbC`^)`~>_F_~E_|]$9.`^Wka_CaF`+Z$dCb?ai$F`^~E_|]D#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~SL^~>w*K^~>kT^~S^z];#ZDa_l{].#a_k#k_k~>iS)_{!9+b+:PfCdbw*k~ECaF`^|!H+P:h-w1k+Pf~>iS)fd?aaa^}(!P(i&^z]%(YP`^{]'(Z%b`^|!:(Z'ca_wD|!+#b`n8Hf?i&?bxOaxO`9$?ea_`~YN_C`+ci$+cQ?JdwS'?CDcxNFDa+c?CDbxN~>FDbwS&~ECa_~>wS'^+ci$+cH^+cHZ'Q?JgwS(xOxOYPZ%`xOwS%~EJbDa~ECa_~>wS(^+ci%+cH^+cHQi$?JdwS#^~EJbDa~ECa_~>wS#^8HfJdD`F_`FD`~>wS%^89cCa_~>xN^#Z.exJ#d~YEbZ#i&:PiS)JeZ)??fi$i$akYF_nD`~>wC^+:BgZ<ecBfUdboDa_~>wD^+Z;dWlbDbUa_~H>wB_8Hf?i&?bxOaxO`9$?ea_`~YN_C`+ci$+cQ?JdwS'?CDcxNFDa+c?CDbxN~>FDbwS&~ECa_~>wS'^+ci$+cH^+cHZ'Q?JgwS(xOxOYPZ%`xOwS%~EJbDa~ECa_~>wS(^+ci%+cH^+cHQi$?JdwS#^~EJbDa~ECa_~>wS#^8HfJdD`F_`FD`~>wS%^89cCa_~>xN^#Z.exJ#d~YEbZ#i&:PiS)JeZ)??fi$i$akYF_nD`~>wC^+:BgZ<ecBfUdboDa_~>wD^+Z;dWlbDbUa_~H^~^>wS*^#cDan~>xP^F_~E_#bWk``m~YN_|!?0_AG^{]40uy!6)i$6aC_AG^6aC_AG^AGvS#~H>vS#_6aC_AG^6aC_AG^AGvS#~H^~^>vE^6aC_AGvS5AGvS#~>u^6aC_AG^~Z6`F^~E^{]3)i$)i$93C^AYJF^~E^AGvC~E^z]&9&Z?^8?vS7vF~Z@^8JZB^AGvF~ZG^6i$Z+^~Z/^9&ZF^~YN^0vLAZ3C^AYJF^AGvK~E^8?vLvK~YE^8?vS;vF~>i%^8?vS-vF~Z6^z!J9&^0vEAMi%Z+^AGvE~Z/^z]:9:8K~>u^)^~Nk^Iy!K8KAI)^9:~>vR0^~N_vC)iS$~Z(^Z*y!I8I?`^8I?a^8I?au~>vS5^I~>vS#^9K_~>vE^)i&~Z(^Iz!G(YG^AI)i&~HHNvD`(YG^AI)i&~HH^~^>vL_(YG^AI)i&~H^~^>vK^Z*y]1(Z1^YM)i&AI~>vL^YKy!M9>`)^~^^ZC^YO?YG^AI8OYIi&AI~>vE^(?i&YMxPAI~>vJ^9EYM)i%AI~>vS;^)i$AI~>vS-^Z*AI~>vF^91AI~>vK^)^~Nk^YKy]*)^!S,^Iy]7)^!S,iS+)^~>iS$^!S,^z!297^97ZI~>iS+^)^~>iS$^iS,y!S,iS+]('iS$^z]##l`^{]@Rl]Bi=]E#oYF_^z]GRo]+i=!O#nYF_^z]/Rn]0)_)i$)i$90OOvR%`YAbuC_~NvR/^~N_vR$F^~E^{]890k^)i$~YE^z]C)i$88_k~^Z8^98C^~>vPF^)i$~YE^Z+^z],)^9,_`~Nak?b^Z-OYAu``vR%Z=u^{]?8OZ,i&^8O?Z,i&O`kvP~Nk^zAZHki#!S-Ki#]5)^!S-?iS-^ZM^95Ka_)^~YL`K^T_~E_{]>95iS-^z]Fi4]M#m_i$z!NRm]9)_99?aF_C^~E^{]K99i&^z!F)k9-YFC_l~E^z!E'i&^z]<8>K^z!>-K^z!34K^z!-8=K^z!,i4!/i=!(#k`^{!.Rk!L)i$)i$)i$)i$8LTaT_~YLKaK_~YLLaL_~S`)i$~>pL_~S_)^~^>`^{]6'i$^z!;9J)i$'bL^~S^zz!S+Omk!S$Olk!):lkl!*:lkm!1:lkn]J:lko!<:lkp!=:lkq!4:lkr!5:lks]H:lku!':lkv/!7:lkv0]-:lkv1!8:lkv2!A:lkv3]=:lkv4]I:lkv5!0:lkv6y", false);
      }
      else if(args.length == 2)
      {
        String in = args[0];
        boolean dbg = args[1].toLowerCase().equals("true") || args[1].equals("1");
        ribbit = new test(in,dbg);
      }
      else
      {
        boolean dbg = args[0].toLowerCase().equals("true") || args[0].equals("1");
        ribbit = new test("R:fi,adbmal,enifed,*,,,,,,,,,-,<,,,,,,,,,,,,,,,,,,,,;92]292AZ4AYJZA^94~Z(^YMAVvCvR3y]A7#ZL^z]L9#i&:BiS)ai&kkz!S):kw)k]))_(Z)aC_F^~E^{!@)^8@Z-lbC`^)`~>_F_~E_|]$9.`^Wka_CaF`+Z$dCb?ai$F`^~E_|]D#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~SL^~>w*K^~>kT^~S^z];#ZDa_l{].#a_k#k_k~>iS)_{!9+b+:PfCdbw*k~ECaF`^|!H+P:h-w1k+Pf~>iS)fd?aaa^}(!P(i&^z]%(YP`^{]'(Z%b`^|!:(Z'ca_wD|!+#b`n8Hf?i&?bxOaxO`9$?ea_`~YN_C`+ci$+cQ?JdwS'?CDcxNFDa+c?CDbxN~>FDbwS&~ECa_~>wS'^+ci$+cH^+cHZ'Q?JgwS(xOxOYPZ%`xOwS%~EJbDa~ECa_~>wS(^+ci%+cH^+cHQi$?JdwS#^~EJbDa~ECa_~>wS#^8HfJdD`F_`FD`~>wS%^89cCa_~>xN^#Z.exJ#d~YEbZ#i&:PiS)JeZ)??fi$i$akYF_nD`~>wC^+:BgZ<ecBfUdboDa_~>wD^+Z;dWlbDbUa_~H>wB_8Hf?i&?bxOaxO`9$?ea_`~YN_C`+ci$+cQ?JdwS'?CDcxNFDa+c?CDbxN~>FDbwS&~ECa_~>wS'^+ci$+cH^+cHZ'Q?JgwS(xOxOYPZ%`xOwS%~EJbDa~ECa_~>wS(^+ci%+cH^+cHQi$?JdwS#^~EJbDa~ECa_~>wS#^8HfJdD`F_`FD`~>wS%^89cCa_~>xN^#Z.exJ#d~YEbZ#i&:PiS)JeZ)??fi$i$akYF_nD`~>wC^+:BgZ<ecBfUdboDa_~>wD^+Z;dWlbDbUa_~H^~^>wS*^#cDan~>xP^F_~E_#bWk``m~YN_|!?0_AG^{]40uy!6)i$6aC_AG^6aC_AG^AGvS#~H>vS#_6aC_AG^6aC_AG^AGvS#~H^~^>vE^6aC_AGvS5AGvS#~>u^6aC_AG^~Z6`F^~E^{]3)i$)i$93C^AYJF^~E^AGvC~E^z]&9&Z?^8?vS7vF~Z@^8JZB^AGvF~ZG^6i$Z+^~Z/^9&ZF^~YN^0vLAZ3C^AYJF^AGvK~E^8?vLvK~YE^8?vS;vF~>i%^8?vS-vF~Z6^z!J9&^0vEAMi%Z+^AGvE~Z/^z]:9:8K~>u^)^~Nk^Iy!K8KAI)^9:~>vR0^~N_vC)iS$~Z(^Z*y!I8I?`^8I?a^8I?au~>vS5^I~>vS#^9K_~>vE^)i&~Z(^Iz!G(YG^AI)i&~HHNvD`(YG^AI)i&~HH^~^>vL_(YG^AI)i&~H^~^>vK^Z*y]1(Z1^YM)i&AI~>vL^YKy!M9>`)^~^^ZC^YO?YG^AI8OYIi&AI~>vE^(?i&YMxPAI~>vJ^9EYM)i%AI~>vS;^)i$AI~>vS-^Z*AI~>vF^91AI~>vK^)^~Nk^YKy]*)^!S,^Iy]7)^!S,iS+)^~>iS$^!S,^z!297^97ZI~>iS+^)^~>iS$^iS,y!S,iS+]('iS$^z]##l`^{]@Rl]Bi=]E#oYF_^z]GRo]+i=!O#nYF_^z]/Rn]0)_)i$)i$90OOvR%`YAbuC_~NvR/^~N_vR$F^~E^{]890k^)i$~YE^z]C)i$88_k~^Z8^98C^~>vPF^)i$~YE^Z+^z],)^9,_`~Nak?b^Z-OYAu``vR%Z=u^{]?8OZ,i&^8O?Z,i&O`kvP~Nk^zAZHki#!S-Ki#]5)^!S-?iS-^ZM^95Ka_)^~YL`K^T_~E_{]>95iS-^z]Fi4]M#m_i$z!NRm]9)_99?aF_C^~E^{]K99i&^z!F)k9-YFC_l~E^z!E'i&^z]<8>K^z!>-K^z!34K^z!-8=K^z!,i4!/i=!(#k`^{!.Rk!L)i$)i$)i$)i$8LTaT_~YLKaK_~YLLaL_~S`)i$~>pL_~S_)^~^>`^{]6'i$^z!;9J)i$'bL^~S^zz!S+Omk!S$Olk!):lkl!*:lkm!1:lkn]J:lko!<:lkp!=:lkq!4:lkr!5:lks]H:lku!':lkv/!7:lkv0]-:lkv1!8:lkv2!A:lkv3]=:lkv4]I:lkv5!0:lkv6y", dbg);
      }
      ribbit.buildAndDecode();
    }

    private void putchar(char c)
    {
	System.out.print(c);
    }

    private char getchar()
    {
      char c;
      try {
        c= (char) System.in.read();
      } catch (IOException ioException)
      {
        c= (char) -1;
      }
      return c;
    }

    private Byte getByte()
    {
      return input.getBytes(StandardCharsets.US_ASCII)[pos++];
    }


  //VM

  private Rib toBool(boolean x){
    if (x) return TRUE;
    else return FALSE;
  }
  private boolean isRib(Object x){
    return (x instanceof Rib || x instanceof RibField);
  }



  //Les fonctions utilisees pour le deboguage de la VM

  private String chars2str(Rib s)
  {
    if(s.equals(NIL) || !s.getField0().isInteger())
    {
      return "";
    }
    else
    {
      return String.valueOf(Character.toChars((int)s.getField0().getField())) + chars2str((Rib)s.getField1().getField());
    }
  }

  private String sym2str(Rib s)
  {
    return chars2str((Rib) ((Rib) s.getField1().getField()).getField0().getField());
  }

  private String showOpnd(Object o)
  {
    return isRib(o)? o instanceof Field? "sym "+sym2str((Rib) ((Field) o).getField()) :"sym "+sym2str((Rib)o) : "int "+ o.toString();
  }

  private void showStack()
  {
    Rib s = stack;
    while(s.getField2().isInteger() && ((IntField)s.getField2()).equals(0))
    {
      System.out.println(s.getField0());
      s = (Rib) s.getField1().getField();
    }
    System.out.println(s.getField0());
  }

  //Fin des fonctions de deboguage

  private void push(Field x){
    stack = new Rib(x,new RibField(stack),new IntField(0));
  }

  private Field pop(){
    Field x =  stack.getField0();
    if (stack.getField1().getField().equals(0)){
      stack = EMPTY;
    }else{
      stack = (Rib) stack.getField1().getField();
    }
    return x;
  }



  // On va pas faire de tab primitives, au moment de l'appel, on utilisera un
  //switch sur les 20 primitives pour savoir laquelle lancer

  private int getCode(){
    int x = getByte() - 35;
    return x < 0 ? 57 : x;
  }

  private int getInt(int n){
    int x = getCode();
    n*=46;
    return x < 46 ? n+x : getInt(n+x-46);
  }

  private Rib listTail(Rib lst, int i){
    if (i == 0) return lst;
    else return listTail((Rib) lst.getField1().getField(),i-1);
  }

  //build the initial symbol table, a mettre dans le main

  private Field symbolRef(int n){ //peut etre Object
    return listTail(symbolTable,n).getField0();
  }

  private void setGlobal(Field val)
  {
    ((Rib)(symbolTable.getField0().getField())).setField0(val);
    symbolTable = (Rib) symbolTable.getField1().getField();
  }

  private Rib getOpnd(Field f)
  {
    return f.isInteger() ? listTail(stack,(int)f.getField()) : ((Rib)f.getField());
  }

  private Rib getCont()
  {
    Rib s = stack;
    while(s.getField2().isInteger() && ((IntField)s.getField2()).equals(0))
    {
      s = (Rib) s.getField1().getField();
    }
    return s;
  }

  private void buildAndDecode(){
    int n = getInt(0);
    while (n > 0){
      n-= 1;
      Rib builder = new Rib(new RibField(NIL),new IntField(0), new IntField(3));
      Rib builder2 = new Rib(new IntField(0),new RibField(builder),new IntField(2));

      symbolTable = new Rib(new RibField(builder2), new RibField(symbolTable), new IntField(0));
    }

    Rib accum = NIL;
    n = 0;
    while (true){
      //System.out.println("while 1");
      Byte c = getByte();
      if (c == 44){
        Rib builder = new Rib(new RibField(accum),new IntField(n),new IntField(3));
        Rib builder2 = new Rib(new IntField(0),new RibField(builder),new IntField(2));

        symbolTable = new Rib(new RibField(builder2),new RibField(symbolTable),new IntField(0));

        accum = NIL;
        n = 0;
      }
      else{
        if (c == 59) break;
        accum = new Rib(new IntField(c),new RibField(accum),new IntField(0));
        n+=1;
      }
    }

    symbolTable = new Rib(
            new RibField(new Rib(
                    new IntField(0),new RibField(
                            new Rib(new RibField(accum),new IntField(n),new IntField(3))),
                    new IntField(2))),
            new RibField(symbolTable), new IntField(0));

    //TODO 1ere partie du decoder RVM par François Luc (jusqu'au dernier set_global)

    int[] weights = {20,30,0,10,11,4};

    Field fieldN;
    while(true)
    {
      //System.out.println("while 2");
      int x = getCode();
      n = x;
      int d = 0;
      int op = 0;
      while(true)
      {
        //System.out.println("while 3");
        d=weights[op];
        if(n<=2+d)
          break;
        n-=d+3;
        op++;
      }
      if(x>90)
      {
        fieldN = pop();
      }
      else
      {
        if(op==jump_call_code)
        {
          if (stack.equals(EMPTY)){
            stack = new Rib(new IntField(0),new IntField(0),new IntField(0));
          }else{
            RibField stack_tmp = new RibField((new Rib(stack.getField0(), stack.getField1(), stack.getField2())));
            stack = new Rib(new IntField(0),stack_tmp,new IntField(0));
          }
          //push(new IntField(0));
          op++;
        }



        if(n==d)
        {
          fieldN = new IntField(getInt(0));
        }
        else
        {
          if(n>=d)
          {
            fieldN = symbolRef(getInt(n - d - 1));
          }
          else
          {
            if (op<3)
            {
              fieldN =  symbolRef(n);
            }
            else {
              fieldN = new IntField(n);
            }
          }
        }

        /*if(n >= d)
        {
          if(n == d)
          {
            fieldN = new IntField(getInt(0));
          }
          else
          {
            fieldN = symbol_ref(getInt(n-d-1));
          }
        } else
        {
          fieldN = (op<3) ? symbol_ref(n) : new IntField(n);
        }*/

        if(op>4)
        {
          fieldN = new RibField(new Rib(
                  new RibField(new Rib(new IntField(n), new IntField(0), pop())),
                  new IntField(0),
                  procedure_tag));
          if (stack.equals(EMPTY))
          {
            break;
          }
          op = if_code;
        }
      }

      stack.setField0(new RibField(new Rib(new IntField(op-1),fieldN,stack.getField0())));

    }

    //pc = (Rib)((Rib)fieldN.getField()).getField2().getField();

    pc = (Rib)((Rib) ((Rib)fieldN.getField()).getField0().getField()).getField2().getField();


    setGlobal(new RibField(new Rib(new IntField(0),new RibField(symbolTable),procedure_tag)));
    setGlobal(new RibField(FALSE));
    setGlobal(new RibField(TRUE));
    setGlobal(new RibField(NIL));

    stack = new Rib(new IntField(0),new IntField(0),new RibField(new Rib(new IntField(halt_code),new IntField(0),new IntField(0))));
    int counter = 0;
    while (true){
      //System.out.println("while 4");
      //System.exit(0);
      Rib c2,s2,k;
      Field c;
      Field o = pc.getField1();
      int i = (int) pc.getField0().getField();
      counter++;
      if (i < 1){
        if (debug){
          System.out.println("--- call "+(isRib(pc.getField2().getField()) ? isRib(pc.getField2().getField())  : "--- jump" + showOpnd(0)));
          showStack();
        }
        o = getOpnd(o).getField0();
        c = ((Rib) o.getField()).getField0();
        if (isRib(c)){
          int nargs;
          c2 = new Rib(new IntField(0),o,new IntField(0));
          s2 = c2;
          nargs = (int) ((Rib)c.getField()).getField0().getField();
          while (nargs > 0){
            s2 = new Rib(pop(),new RibField(s2),new IntField(0));
            nargs-=1;
          }
          if (isRib(pc.getField2().getField())){
            c2.setField0(new RibField(stack));
            c2.setField2(pc.getField2());
          }else{
            k = getCont();
            c2.setField0(k.getField0());
            c2.setField2(k.getField2());
          }
          stack = s2;
        }else{
          primitives((int)c.getField());
          if (isRib(pc.getField2().getField())){
            c = new RibField(pc);
          }else {
            c = new RibField(getCont());
            stack.setField1(((Rib) c.getField()).getField0());
          }
        }
        pc = (Rib) ((Rib)c.getField()).getField2().getField();
      }else if (i < 2){
        if (debug) {
          System.out.println("--- set " + showOpnd(o));
          showStack();
        }
          Field x = pop();
          Rib temp = getOpnd(o);
          temp.setField0(x);
          pc = (Rib) pc.getField2().getField();
        }
      else if (i < 3){
        if (debug){
          System.out.println("--- get "+showOpnd(o));
          showStack();
        }
        push(getOpnd(o).getField0());
        pc=(Rib) pc.getField2().getField();
      }else if (i < 4){
        if (debug){
          System.out.println("--- const "+ o.toString());
          showStack();
        }
        push(o);
        pc=(Rib) pc.getField2().getField();
      }else if (i < 5){
        if (debug) {
          System.out.println("--- if");
          showStack();
        }
        Field temp = pop();
        if(!temp.isInteger() && temp.getField().equals(FALSE))
        {
          pc = (Rib) pc.getField2().getField();
        }
        else
          pc = (Rib) pc.getField1().getField();
      }else break;
    }

  }



  private void primitives(int c){
    Field x,y,z;
    switch (c) {
      case 0 : //rib
        z = pop();
        y = pop();
        x = pop();
        push(new RibField(new Rib(x, y, z)));
        break;
      case 1 : //id
        x = pop();
        push(x);
        break;
      case 2 : //arg1
        pop();
        break;
      case 3 : //arg2
        y = pop();
        pop();
        push(y);
        break;
      case 4 : //close
        x = ((Rib)pop().getField()).getField0();
        push(new RibField(new Rib(x, new RibField(stack), procedure_tag)));
        break;
      case 5 : //rib?
        x = pop();
        push(new RibField(toBool(isRib(x))));
        break;
      case 6 : //field0
        x = pop();
        push(((Rib) x.getField()).getField0());
        break;
      case 7 : //field1
        x = pop();
        push(((Rib) x.getField()).getField1());
        break;
      case 8 : //field2
        x = pop();
        push(((Rib) x.getField()).getField2());
        break;
      case 9 : //field0-set!
        y = pop();
        x = pop();
        ((Rib) x.getField()).setField0(y);
        push(y);
        break;
      case 10 : //field1-set!
        y = pop();
        x = pop();
        ((Rib) x.getField()).setField1(y);
        push(y);
        break;
      case 11 : //field2-set!
        y = pop();
        x = pop();
        ((Rib) x.getField()).setField2(y);
        push(y);
        break;
      case 12 : //eqv?
        y = pop();
        x = pop();
        push(new RibField(toBool(x.equals(y))));
        break;
      case 13 : //<
        y = pop();
        x = pop();
        push(new RibField(toBool((x.isInteger() && y.isInteger()) && (int) x.getField() < (int) y.getField())));
        break;
      case 14 : //+
        y = pop();
        x = pop();
        push(new IntField((int) x.getField() + (int) y.getField()));
        break;
      case 15 : //-
        y = pop();
        x = pop();
        push(new IntField((int) x.getField() - (int) y.getField()));
        break;
      case 16 : //*
        y = pop();
        x = pop();
        push(new IntField((int) x.getField() * (int) y.getField()));
        break;
      case 17 : //quotient
        y = pop();
        x = pop();
        push(new IntField((int) x.getField() / (int) y.getField()));
        break;
      case 18 : //getchar
        push(new IntField((int) getchar()));
        break;
      case 19 : //putchar
        x = pop();
        int field = (int) x.getField();
        putchar((char) field);
        push(x);
        break;
    }
  }

}
