with Ada.IO_Exceptions,Ada.Text_IO.Text_Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with GNAT.OS_Lib;

procedure Rvm is
   
   type RibCore;
   type Rib is access RibCore;
   
   type RibKind is (Leaf, Branch);
   
   type RibCore(kind : RibKind) is record
      case kind is
         when Leaf =>
            Value : Integer;
         when Branch =>
            First : Rib;
            Second : Rib;
            Third : Rib;
      end case;
   end record;
   
   function is_rib(element : Rib) return Boolean is
   begin
      return element.Kind = Branch;
   end is_rib;

   function Truthy_Rib(element : Rib) return Boolean is
   begin
      if element.kind = Branch then
         return True;
      else
         return element.Value /= 0;
      end if;
   end Truthy_Rib;

   function RIB_CONS(first : Rib; second : Rib; third : Rib)
                     return Rib is
   begin
      return new RibCore'(Kind => Branch, First => first, Second => second, Third => third);
   end RIB_CONS;

   function RIB_INT(value : Integer)
                    return Rib is
   begin
      return new RibCore'(Kind => Leaf, Value => value);
   end RIB_INT;

   function UNRIB_INT(const : Rib)
                    return Integer is
   begin
      return const.Value;
   end UNRIB_INT;

   RIB_TAG_PAIR : Rib := RIB_INT(0);
   RIB_TAG_PROC : Rib := RIB_INT(1);
   RIB_TAG_SYMBOL : Rib := RIB_INT(2);
   RIB_TAG_STRING : Rib := RIB_INT(3);
   RIB_TAG_VECTOR : Rib := RIB_INT(4);
   RIB_TAG_SPECIAL : Rib := RIB_INT(5);
   
   RIB_ZERO : Rib := RIB_INT(0);
   RIB_NIL : Rib := RIB_CONS(First => RIB_ZERO, Second => RIB_ZERO, Third => RIB_TAG_SPECIAL);
   RIB_FALSE : Rib := RIB_CONS(First => RIB_ZERO, Second => RIB_ZERO, Third => RIB_TAG_SPECIAL);
   RIB_TRUE : Rib := RIB_CONS(First => RIB_ZERO, Second => RIB_ZERO, Third => RIB_TAG_SPECIAL);
   
   RIB_EMPTY : Rib := RIB_CONS(First => RIB_NIL, Second => RIB_ZERO, Third => RIB_TAG_STRING);
   
   function RIB_BOOL(b : Boolean) return Rib is
   begin
      return (if b then RIB_TRUE else RIB_FALSE);
   end RIB_BOOL;
   
   function GetByte(input : String; pos : In Out Integer)
                    return Integer is
      sym : Character;
   begin
      sym := input(pos);
      pos := pos + 1;
      return Character'Pos(sym);
   end;

   function GetCode(input : String; pos : In Out Integer)
                    return Integer is
      item : Integer;
   begin
      item := GetByte(input, pos) - 35;
      if item < 0 then
         item := 57;
      end if;
      return item;
   end;

   function GetInt(input : String; pos : In Out Integer; acc : Integer := 0)
                   return Integer is
      number : Integer := acc;
      digit : Integer;
   begin
      loop
         number := number * 46;
         digit := GetCode(input, pos);

         if digit < 46 then
            return number + digit;
         end if;
         number := number + digit - 46;
      end loop;
   end GetInt;

   function Pop(stack : In Out Rib) return Rib is
      x : Rib := stack.First;
   begin
      stack := stack.Second;
      return x;
   end Pop;
   
   procedure Push(stack : In Out Rib; value : Rib) is
   begin
      stack := RIB_CONS(First => value, Second => stack, Third => RIB_TAG_PAIR);
   end Push;
   
   function Put_char (c : Integer) return Integer is
      Output_Stream : Access Ada.Streams.Root_Stream_Type'Class:= Ada.Text_IO.Text_Streams.Stream( File => Standard_Output );
   begin
      Character'Write(Output_Stream, Character'Val(c));
      return c;
   end Put_char;

   --retourne le nombre correspondant au caractere lu ou -1 si on a atteint le eof
   function Get_char return Integer is
      new_char : Character;
      Input_File : File_Type:= Standard_Input;
      Input_Stream : Access Ada.Streams.Root_Stream_Type'Class:= Ada.Text_IO.Text_Streams.Stream( File => Input_File );
   begin 
      
      Character'Read( Input_Stream, new_char );
      return Character'Pos(new_char);
   Exception
      When ADA.IO_EXCEPTIONS.END_ERROR => return -1;
   end Get_char;

   --retourne le n-ieme element de la liste
   function List_get(list : Rib; n : Integer) return Rib is
      node : Rib := list;
      i : Integer := n;
   begin
      while (i > 0) loop
         node := node.Second;
         i := i - 1;
      end loop; 
      return node;
   end List_get;

   function DecodeGlobalSymbolTable(input : String; pos : In Out Integer) return Rib is
      item : Integer;
      symTableN : Integer;
      symToAppend : Rib := null;
      symName : Rib := RIB_NIL;
      symLen : Integer := 0;
      symTable : Rib := RIB_NIL;
   begin
      -- Read the empty global symbols
      
      symTableN := GetInt(input, pos);   
      for i in 1 .. symTableN loop
         symTable := RIB_CONS(First => RIB_CONS(First => RIB_FALSE, Second => RIB_EMPTY, Third => RIB_TAG_SYMBOL), Second => symTable, Third => RIB_TAG_PAIR);
      end loop;
   
      -- Read the global symbol names
   
      loop 
         item := GetByte(input, pos);
      
         if item = Character'Pos(',') or item = Character'Pos(';') then
            symToAppend := RIB_CONS(First => RIB_FALSE,
                                    Second => RIB_CONS(First => symName, Second => RIB_INT(symLen), Third => RIB_TAG_STRING),
                                    Third => RIB_TAG_SYMBOL);
            symTable := RIB_CONS(First => symToAppend, Second => symTable, Third => RIB_TAG_PAIR);
            symName := RIB_NIL;
            symLen := 0;
         
            if item = Character'Pos(';') then
               exit;
            end if;
         else
            symName := RIB_CONS(First => RIB_INT(item), Second => symName, Third => RIB_TAG_PAIR);
            symLen := symLen + 1;
         end if;
      end loop;
      
      return symTable;
   end DecodeGlobalSymbolTable;

   function DecodeBytecodeStream(input : String; pos : In Out Integer; symTable : Rib) return Rib is
      x : Integer;
      n : Integer;
      d : Integer;
      op : Integer;
      opArg : Rib;
      opShorts : array(0 .. 5) of Integer := (20, 30, 0, 10, 11, 4);
      opStack : Rib := null;
   begin
      loop
         x := GetCode(input, pos);
         n := x;
         d := 0;
         op := 0;
      
         loop
            d := opShorts(op);
            if n <= (2 + d) then
               exit;
            end if;
            n := n - (d + 3);
            op := op + 1;
         end loop;
      
         if x > 90 then
            opArg := Pop(opStack);
         else
            if op = 0 then
               Push(opStack, RIB_ZERO);
               op := op + 1;
            end if;
         
            if n = d then
               opArg := RIB_INT(GetInt(input, pos));
            elsif n >= d then
               opArg := List_get(symTable, GetInt(input, pos, n-d-1)).First;
            elsif op < 3 then
               opArg := List_get(symTable, n).First;
            else
               opArg := RIB_INT(n);
            end if;
         
            if 4 < op then
               opArg := RIB_CONS(First => RIB_CONS(First => opArg,
                                                   Second => RIB_ZERO, -- Unused
                                                   Third => Pop(opStack)),
                                 Second => RIB_NIL,
                                 Third => RIB_TAG_PROC);
               
               if opStack = null then
                  return opArg.First.Third;
               end if;
               
               op := 4;
            end if;
         end if;
      
         opStack.First := RIB_CONS(First => RIB_INT(op-1), Second => opArg, Third => opStack.First);
      end loop;
   end;
   
   function PRIM_RIB(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_CONS(First => x, Second => y, Third => z);
   end;
   
   function PRIM_ID(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return x;
   end;
   
   function PRIM_FIRST(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      -- Raccourci (comme dans rvm.py): On ne pop que l'argument 2, et on laisse le premier à sa place.
      -- On doit utiliser ce spaghetti, sinon le programme corrompt le stack par réflexion.
      return null;
   end;
   
   function PRIM_SECOND(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return y;
   end;
   
   function PRIM_CLOSE(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_CONS(First => x.First, Second => stack, Third => RIB_TAG_PROC); 
   end;
   
   function PRIM_IS_RIB(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_BOOL(Is_Rib(x));
   end;
   
   function PRIM_FIELD0(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return x.First;
   end;
   
   function PRIM_FIELD1(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return x.Second;
   end;
   
   function PRIM_FIELD2(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return x.Third;
   end;
   
   function PRIM_FIELD0_SET(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      x.First := y;
      return y;
   end;
   
   function PRIM_FIELD1_SET(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      x.Second := y;
      return y;
   end;
   
   function PRIM_FIELD2_SET(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      x.Third := y;
      return y;
   end;
   
   function PRIM_EQV(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      if Is_Rib(x) or Is_Rib(y) then
         return RIB_BOOL(x = y);
      else
         return RIB_BOOL(UNRIB_INT(x) = UNRIB_INT(y));
      end if;
   end;
   
   function PRIM_LESS_THAN(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_BOOL(UNRIB_INT(x) < UNRIB_INT(y));
   end;
   
   function PRIM_PLUS(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_INT(UNRIB_INT(x) + UNRIB_INT(y));
   end;
   
   function PRIM_MINUS(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_INT(UNRIB_INT(x) - UNRIB_INT(y));
   end;
   
   function PRIM_TIMES(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_INT(UNRIB_INT(x) * UNRIB_INT(y));
   end;
   
   function PRIM_QUOTIENT(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_INT(UNRIB_INT(x) / UNRIB_INT(y));
   end;
   
   function PRIM_GET_CHAR(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_INT(Get_char);
   end;
   
   function PRIM_PUT_CHAR(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      return RIB_INT(Put_char(UNRIB_INT(x)));
   end;
   
   function PRIM_EXIT(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib is
   begin
      GNAT.OS_Lib.OS_Exit(UNRIB_INT(x));
      return null;
   end;
   
   type PrimitiveFunction is access function(stack : Rib; x : Rib; y : Rib; z : Rib) return Rib;
     
   type Primitive is record
      Arity : Integer;
      Execute : PrimitiveFunction;
   end record;
   
   function ExecutePrimitive(stack : In Out Rib; code : Integer) return Rib is
      arity : Integer;
      z : Rib := null;
      y : Rib := null;
      x : Rib := null;
      r : Rib;
      
      primitives : array(0 .. 21) of Primitive
        := (
            Primitive'(3, PRIM_RIB'Access),        -- (primitive (%%rib a b c))
            Primitive'(1, PRIM_ID'Access),         -- (primitive (%%id x))
            Primitive'(1, PRIM_FIRST'Access),      -- (primitive (%%arg1 x y))
            Primitive'(2, PRIM_SECOND'Access),     -- (primitive (%%arg2 x y))
            Primitive'(1, PRIM_CLOSE'Access),      -- (primitive (%%close rib))
            Primitive'(1, PRIM_IS_RIB'Access),     -- (primitive (%%rib? rib))
            Primitive'(1, PRIM_FIELD0'Access),     -- (primitive (%%field0 rib))
            Primitive'(1, PRIM_FIELD1'Access),     -- (primitive (%%field1 rib))
            Primitive'(1, PRIM_FIELD2'Access),     -- (primitive (%%field2 rib))
            Primitive'(2, PRIM_FIELD0_SET'Access), -- (primitive (%%field0-set! rib x))
            Primitive'(2, PRIM_FIELD1_SET'Access), -- (primitive (%%field1-set! rib x))
            Primitive'(2, PRIM_FIELD2_SET'Access), -- (primitive (%%field2-set! rib x))
            Primitive'(2, PRIM_EQV'Access),        -- (primitive (%%eqv? x y))
            Primitive'(2, PRIM_LESS_THAN'Access),  -- (primitive (%%< a b))
            Primitive'(2, PRIM_PLUS'Access),       -- (primitive (%%+ a b))
            Primitive'(2, PRIM_MINUS'Access),      -- (primitive (%%- a b))
            Primitive'(2, PRIM_TIMES'Access),      -- (primitive (%%* a b))
            Primitive'(2, PRIM_QUOTIENT'Access),   -- (primitive (%%quotient a b))
            Primitive'(0, PRIM_GET_CHAR'Access),   -- (primitive (%%getchar))
            Primitive'(1, PRIM_PUT_CHAR'Access),   -- (primitive (%%putchar c))
            Primitive'(1, PRIM_EXIT'Access),       -- (primitive (%%exit a))
            Primitive'(0, null)
           );
   begin
      -- Pop les arguments du stack dans l'ordre inverse
      arity := primitives(code).Arity;
      for iarg in 1 .. arity loop
         z := y;
         y := x;
         x := Pop(stack);
      end loop;
      
      -- On met le résultat sur le stack
      r := primitives(code).Execute(stack, x, y, z);
      if r /= null then
         Push(stack, r);
      end if;
      
      -- Ce résultat interne est toujours null, pour l'instant
      return null; 
   end ExecutePrimitive;
   
   function GetContinuation(stack : Rib) return Rib is
      s : Rib := stack;
   begin
      while not Truthy_Rib(s.Third) loop
         s := s.Second;
      end loop;
      return s;
   end GetContinuation;
   
   function GetOperand(stack : Rib; o : Rib) return Rib is
   begin
      if Is_Rib(o) then
         return o;
      end if;
               
      return List_Get(stack, UNRIB_INT(o));
   end GetOperand;
   
   procedure Execute(symTable : In Out Rib; pc : In Out Rib) is
      o : Rib;
      i : Integer;
      c : Rib;
      c2 : Rib;
      s2 : Rib;
      nparams : Integer;
      k : Rib;
      prim0 : Rib;
      
      Stack : Rib;
   begin
      prim0 := RIB_CONS(First => RIB_ZERO, Second => symTable, Third => RIB_TAG_PROC);
      Pop(symTable).First := prim0;
      Pop(symTable).First := RIB_FALSE;
      Pop(symTable).First := RIB_TRUE;
      Pop(symTable).First := RIB_NIL;
      
      -- Continuation frame (dark green).
      Stack := RIB_CONS(FIRST => RIB_ZERO,
                        SECOND => RIB_NIL,
                        THIRD => RIB_CONS(
                          First => RIB_INT(5),
                          Second => RIB_ZERO,
                          Third => RIB_ZERO));
      
      loop
         i := UNRIB_INT(pc.First);
         o := pc.Second;
         pc := pc.Third;
         
         case i is
            when 0 => -- jump/call
               o := GetOperand(Stack, o).First; -- Procedure
               
               loop
                  c := o.First; -- code pointer
                  
                  if Is_Rib(c) then -- o is Closure
                     -- New continuation frame
                     c2 := RIB_CONS(First => RIB_ZERO, Second => o, Third => RIB_ZERO);
                     s2 := c2;
                     
                     nparams := UNRIB_INT(c.First) / 2;
                     while nparams /= 0 loop
                        s2 := RIB_CONS(First => Pop(Stack), Second => s2, Third => RIB_TAG_PAIR);
                        nparams := nparams - 1;
                     end loop;
                     
                     if Truthy_Rib(pc) then -- call
                        c2.First := Stack;
                        c2.Third := pc;
                     else -- jump
                        k := GetContinuation(Stack);
                        c2.First := k.First;
                        c2.Third := k.Third;
                     end if;
                     
                     stack := s2;
                     pc := c.Third;
                     
                     exit;
                  else -- o is Primitive
                     o := ExecutePrimitive(Stack, UNRIB_INT(c));
                     
                     if o = null then
                        if not Truthy_Rib(pc) then -- jump
                           c := GetContinuation(Stack);
                           
                           Stack.Second := c.First;
                           pc := c.Third;
                        end if;
                           
                        exit;
                     end if;
                  end if;
               end loop;
            when 1 => -- set
               o := GetOperand(Stack, o);
               o.First := Pop(Stack);
            when 2 => -- get
               o := GetOperand(Stack, o);
               Push(Stack, o.First);
            when 3 => -- const
               Push(Stack, o);
            when 4 => -- if
               if Pop(Stack) /= RIB_FALSE then
                  pc := o;
               end if;
            when others => -- halt
               exit;
         end case;
      end loop;
   end Execute;

   -- @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
   input : String :=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y";
   -- )@@
   
   pos : Integer := input'First;
   symTable : Rib;
   pc : Rib;
begin
   symTable := DecodeGlobalSymbolTable(input, pos);
   pc := DecodeBytecodeStream(input, pos, symTable);
   Execute(symTable, pc);
end Rvm;
 
