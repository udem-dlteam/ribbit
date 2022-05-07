(* RVM code that prints HELLO! *)
let input = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,enilwen,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8L!L8L@YJ@YGZ$^8J~YN^YC@PvCvR3y]$7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw\'k]@\'_*Z@aC_G^~F^{!>\'^8>YHlbC`^\'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYPecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YM_|!94_@K^{!J4uy]?\'i$9?C_@K^G^~F^z]I\'i$\'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YK^8EZ)^~YM^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z%^z!G8E^4vE@Z?YD^@KvE~YK^z]O9O8@~?u^\'^~Ik^Dy!@8@@D\'^9O~?vR0^~I_vC\'iS0~YN^YFy!?*V^@D\'i&~OOIvD`*V^@D\'i&~OO^~^?vL_*V^@D\'i&~O^~^?vK^YFy]M*ZM^YC\'i&@D~?vL^Wy!C9*`\'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC\'i%@D~?vS;^\'i$@D~?vS-^YF@D~?vF^9M@D~?vK^\'^~Ik^Wy!F\'^!S-^Dy]H\'^!S-iS.\'^~?iS0^!S-^z!-9H^9HYS#~?iS.^\'^~?iS0^iS-y!S-iS.!N(iS0^z]27%Z>\'_@YS&Jc^@YS\'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-9#`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];9#`H^{]<i+!Di1!B#nS_^z!KQn]F\'_\'i$\'i$9FLLvR%`YObuC_~IvR/^~I_vR$G^~F^{]G9Fk^\'i$~T^z!S%\'i$5_k~^ZG^9GC^~?vPG^\'i$~T^YD^z]E\'^9E_`~IakAb^YHLYOu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P\'^!S,AiS,^YS$^9PBa_\'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!MQm]J\'`9JAca`Ll^~I_k|]L9Ji&`^{]A\'^9ALl`C^~I`k{]N9\'aZA`^|]#0ZA`^{!<\'k8HSC_l~F^z!=(i&^z!P87B^z!76B^z]/+B^z!61B^z]9iS)]\'iS\'!,i+!0i1!*#k`^{!/Qk!A\'i$\'i$\'i$\'i$8AHaH_~YABaB_~YAJaJ_~R`\'i$~?pJ_~R_\'^~^?`^{]%(i$^z!:9>\'i$(bJ^~R^zz!S.Lmk!S0Llk!\':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S\':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!O:lkv3]&:lkv4!S#:lkv5!4:lkv6y"

let debug = Sys.getenv_opt "RIBBIT_DEBUG" |> Option.is_some
let tracing = ref false
let step_count = ref 0
let start_tracing = ref 0
let next_stamp = ref 0

let _ = if debug then Printexc.record_backtrace true

module Rib = struct
  (* Rib (car, cdr, tag) *)
  type t = Rib of t ref * t ref * t ref | Integer of int

  let make_rib_of_ints a b c =
    Rib (ref (Integer a), ref (Integer b), ref (Integer c))
  let make_rib a b c = Rib (ref a, ref b, ref c)

  let is_rib = function Rib _ -> true | _ -> false
  let is_int = function Integer _ -> true | _ -> false

  let int_val = function Integer i -> i
                     | _ -> invalid_arg "int_val expects an Integer"
  let int_val_orelse i def =
    match i with
    |  Integer i -> i
    | _ -> def

  let rib_eq a b =
    match a, b with
    | Integer x, Integer y -> x = y
    | Rib _, Rib _ -> a == b
    | _ -> false

  let get_car = function Rib (car,_,_) -> !car
                       | _ -> invalid_arg "get_car expects a rib"
  let set_car rib newval =
    match rib with
    | Rib (car,_,_) -> car := newval
    | _ -> invalid_arg "set_car expects a rib"
  let get_cdr = function Rib (_,cdr,_) -> !cdr
                       | _ -> invalid_arg "get_cdr expects a rib"
  let set_cdr rib newval =
    match rib with
    | Rib (_,cdr,_) -> cdr := newval
    | _ -> invalid_arg "set_cdr expects a rib"
  let get_tag = function Rib (_,_,tag) -> !tag
                       | _ -> invalid_arg "get_tag expects a rib"
  let set_tag rib newval =
    match rib with
    | Rib (_,_,tag) -> tag := newval
    | _ -> invalid_arg "set_tag expects a rib"

  let false_rib = make_rib_of_ints 0 0 5
  let true_rib = make_rib_of_ints 0 0 5
  let nil_rib = make_rib_of_ints 0 0 5

  let to_bool = function true -> true_rib | false -> false_rib

  let write_to_buffer rib buf =
    let rec helper r =
      match r with
      | Integer i -> Printf.bprintf buf "%d" i
      | Rib _ when r == true_rib -> Buffer.add_string buf "#t"
      | Rib _ when r == false_rib -> Buffer.add_string buf "#f"
      | Rib _ when r == nil_rib -> Buffer.add_string buf "()"
      | Rib (car, cdr, tag) ->
         let obj = ref r in
         let typ = int_val_orelse !tag (-1) in
         if typ = 4 then begin
             Buffer.add_char buf '#';
             helper !car
           end else if typ = 0 then begin
             let n = ref 0 in
             Buffer.add_char buf '(';
             helper !car;
             let obj = ref !cdr in
             while is_rib !obj && int_val_orelse (get_tag !obj) (-1) = 0 do
               if !n > 4 then begin
                   Buffer.add_string buf " ...";
                   obj := nil_rib
                 end else begin
                   Buffer.add_char buf ' ';
                   helper (get_car !obj);
                   obj := get_cdr !obj;
                   incr n
                 end
             done;
             if not (!obj == nil_rib) then begin
                 Buffer.add_string buf " . ";
                 helper !obj
               end;
             Buffer.add_char buf ')'
           end else if typ = 1 then begin
             if is_rib !car then
               Printf.bprintf buf "#<procedure params=%d>" (int_val (get_car !car))
             else
               Printf.bprintf buf "#<primitive %d>" (int_val !car)
           end else if typ = 2 then begin
             obj := get_cdr !obj;
             if is_rib !obj && int_val (get_tag !obj) = 3 &&
                  int_val (get_cdr !obj) > 0 then begin
                 obj := get_car !obj;
                 while is_rib !obj && int_val (get_tag !obj) = 0 do
                   Buffer.add_char buf (get_car !obj |> int_val |> char_of_int);
                   obj := get_cdr !obj
                 done;
               end else begin
                 Buffer.add_string buf "#<symbol ";
                 helper !obj;
                 Buffer.add_char buf '>'
               end
           end else if typ = 3 then begin
             Buffer.add_char buf '"';
             obj := !car;
             while is_rib !obj && int_val (get_tag !obj) = 0 do
               begin
                 match (get_car !obj |> int_val |> char_of_int) with
                 | '\n' -> Buffer.add_string buf "\\n"
                 | '\r' -> Buffer.add_string buf "\\r"
                 | '\t' -> Buffer.add_string buf "\\t"
                 | '\\' -> Buffer.add_string buf "\\\\"
                 | '"' -> Buffer.add_string buf "\\\""
                 | c -> Buffer.add_char buf c
               end;
               obj := get_cdr !obj
             done;
             Buffer.add_char buf '"'
           end else begin
             Buffer.add_char buf '[';
             helper !car;
             Buffer.add_char buf ',';
             helper !cdr;
             Buffer.add_char  buf ',';
             helper !tag;
             Buffer.add_char buf ']'
           end in
    helper rib;
    buf

  let print_rib ?(out=stdout) rib =
    Buffer.create 16 |> write_to_buffer rib |> Buffer.output_buffer out
end

open Rib

let stack = ref (Integer 0)

let start_step () =
  incr step_count;
  if !step_count >= !start_tracing then tracing := true;
  if not !tracing then begin
      if !step_count >= !next_stamp then begin
          next_stamp := (float_of_int !next_stamp) *. 1.01 +. 1.0 |>  int_of_float;
          Printf.printf "@%d\n" !step_count;
        end
    end else begin
      let s = ref !stack in
      let buf = Buffer.create 80 in
      let sep = ref "" in
      Printf.bprintf buf "@%d STACK = (" !step_count;
      while is_rib !s && int_val_orelse (get_tag !s) (-1) = 0 do
        Buffer.add_string buf !sep;
        write_to_buffer (get_car !s) buf |> ignore;
        sep := " ";
        s := get_cdr !s
      done;
      Buffer.add_char buf ')';
      Buffer.output_buffer stdout buf;
      print_newline ();
      flush stdout
    end

let push x = stack := make_rib x !stack (Integer 0)
let pop () =
  match !stack with
  | Rib (car,cdr,_) ->
     stack := !cdr;
     !car
  | _ -> invalid_arg "Top of stack is not a rib"

module type PRIMITIVES = sig
  val primitives: (unit -> unit) array
end

module Primitives : PRIMITIVES = struct
  let prim0 f = function () -> f () |> push
  let prim1 f = function () -> pop () |> f |> push
  let prim2 f = function () ->
                  let x = pop () in
                  let y = pop () in
                  f x y |> push
  let prim3 f = function () ->
                  let x = pop () in
                  let y = pop () in
                  let z = pop () in
                  f x y z |> push

  let getchar () =
    try
      input_char stdin |> int_of_char
    with End_of_file -> -1

  let putchar c =
    char_of_int c |> print_char;
    flush stdout;
    c

  let primitives = [|
      prim3 (fun z y x -> make_rib x y z);
      prim1 (function x -> x);
      (function () -> pop () |> ignore);
      prim2 (fun y x -> y);
      prim1 (function x -> make_rib (get_car x) !stack (Integer 1));
      prim1 (function Rib _ -> true_rib | _ -> false_rib);
      prim1 get_car;
      prim1 get_cdr;
      prim1 get_tag;
      prim2 (fun y x -> set_car x y; y);
      prim2 (fun y x -> set_cdr x y; y);
      prim2 (fun y x -> set_tag x y; y);
      prim2 (fun y x -> to_bool (rib_eq x y));
      prim2 (fun y x -> match x, y with
                        | Integer a, Integer b -> to_bool (a < b)
                        | _ -> invalid_arg "< arguments must be Integers");
      prim2 (fun y x -> match x, y with
                        | Integer a, Integer b -> Integer (a + b)
                        | _ -> invalid_arg "+ arguments must be Integers");
      prim2 (fun y x -> match x, y with
                        | Integer a, Integer b -> Integer (a - b)
                        | _ -> invalid_arg "- arguments must be Integers");
      prim2 (fun y x -> match x, y with
                        | Integer a, Integer b -> Integer (a * b)
                        | _ -> invalid_arg "* arguments must be Integers");
      prim2 (fun y x -> match x, y with
                        | Integer a, Integer b -> Integer (a / b)
                        | _ -> invalid_arg "quotient arguments must be Integers");
      prim0 (function () -> Integer (getchar ()));
      prim1 (function Integer ch -> Integer (putchar ch) | _ -> invalid_arg "putchar argument must be Integer");
      prim1 (function Integer status -> exit status | _ -> invalid_arg "exit argument must be Integer")
    |]
end

let get_byte =
  let input_stream = Stream.of_string input in
  function () -> (Stream.next input_stream |> int_of_char)

let get_code () =
  let x = (get_byte ()) - 35 in if x < 0 then 57 else x

let rec get_int n =
  let x = get_code () in
  let n = n * 46 in
  if x < 46 then n + x else get_int (n + x - 46)

let rec list_tail lst i =
  if i = 0 then lst else list_tail (get_cdr lst) (i - 1)

(* Build the initial symbol table *)
let symtbl = ref nil_rib
let _ =
  for n = get_int 0 downto 1 do
    symtbl := make_rib (make_rib false_rib
                          (make_rib nil_rib (Integer 0) (Integer 3))
                          (Integer 2))
                !symtbl (Integer 0)
  done;
  let accum = ref nil_rib
  and n = ref 0
  and in_loop = ref true in
  while !in_loop do
    let c = get_byte () in
    if c = 44 then begin
        let r1 = make_rib !accum (Integer !n) (Integer 3) in
        let r2 = make_rib false_rib r1 (Integer 2) in
        symtbl := make_rib r2 !symtbl (Integer 0);
        accum := nil_rib;
        n := 0
      end
    else if c = 59 then
      in_loop := false
    else begin
        accum := make_rib (Integer c) !accum (Integer 0);
        incr n
      end
  done;
  symtbl := make_rib (make_rib false_rib
                        (make_rib !accum (Integer !n) (Integer 3))
                        (Integer 2))
              !symtbl (Integer 0)

let symbol_ref n = list_tail !symtbl n |> get_car

let pc = ref nil_rib

(* Decode the RVM instructions *)
let _ =
  let codes = [| 20; 30; 0; 10; 11; 4 |]
  and in_loop = ref true
  and n = ref nil_rib in
  while !in_loop do
    let x = get_code ()
    and d = ref 0
    and op = ref 0
    and in_loop2 = ref true in
    n := Integer x;
    while !in_loop2 do
      d := codes.(!op);
      if int_val !n <= 2 + !d then
        in_loop2 := false
      else begin
          n := Integer ((int_val !n) - (!d + 3));
          incr op
        end
    done;
    if x > 90 then
      n := pop ()
    else begin
        if !op = 0 then begin
            stack := make_rib (Integer 0) !stack (Integer 0);
            incr op
          end;
        n := if int_val !n = !d then
               Integer (get_int 0)
             else if int_val !n >= !d then
               get_int ((int_val !n) - !d - 1) |> symbol_ref
             else if !op < 3 then
               int_val !n |> symbol_ref
             else
               !n;
        if 4 < !op then begin
            n := make_rib (make_rib !n (Integer 0) (pop ())) nil_rib (Integer 1);
            if not (is_rib !stack) then
              in_loop := false
            else
              op := 4
          end;
      end;
    if !in_loop then
      set_car !stack (make_rib (Integer (!op - 1)) !n (get_car !stack))
  done;
  pc := get_car !n |> get_tag

let get_opnd o =
  match o with
  | Rib _ -> o
  | Integer i -> list_tail !stack i

let get_cont () =
  let s = ref !stack in
  while not (is_rib (get_tag !s)) do
    s := get_cdr !s
  done;
  !s

let set_global v =
  set_car (get_car !symtbl) v;
  symtbl := (get_cdr !symtbl)

(* Execute the program *)
let _ =
  set_global (make_rib (Integer 0) !symtbl (Integer 1));
  set_global false_rib;
  set_global true_rib;
  set_global nil_rib;
  stack := make_rib (Integer 0) (Integer 0) (make_rib_of_ints 5 0 0);
  while true do
    if debug then start_step ();
    let o = ref (get_cdr !pc)
    and i = int_val (get_car !pc) in
    if i < 1 then begin (* jump/call *)
        if !tracing then begin
            (if is_rib (get_tag !pc) then "call " else "jump ") |> prerr_string;
            print_rib ~out:stderr !o;
            prerr_newline ()
          end;
        o := get_opnd !o |> get_car;
        let c = ref (get_car !o) in
        if is_rib !c then begin
            let c2 = ref (make_rib (Integer 0) !o (Integer 0)) in
            let s2 = ref !c2
            and nargs = int_val (get_car !c) in
            for narg = nargs downto 1 do
              s2 := make_rib (pop ()) !s2 (Integer 0)
            done;
            if is_rib (get_tag !pc) then begin (* call *)
                set_car !c2 !stack;
                set_tag !c2 (get_tag !pc)
              end else begin (* jump *)
                let k = get_cont () in
                set_car !c2 (get_car k);
                set_tag !c2 (get_tag k)
              end;
            stack := !s2
          end else begin
            Primitives.primitives.(int_val !c) ();
            if is_rib (get_tag !pc) then (* call *)
              c := !pc
            else begin (* jump *)
                c := get_cont ();
                set_cdr !stack (get_car !c)
              end
          end;
        pc := get_tag !c
      end else if i < 2 then begin (* set *)
        if !tracing then begin
            prerr_string "set ";
            print_rib ~out:stderr !o;
            prerr_newline ()
          end;
        let opnd = get_opnd !o in
        set_car opnd (get_car !stack);
        stack := get_cdr !stack;
        pc := get_tag !pc
      end else if i < 3 then begin (* get *)
        if !tracing then begin
            prerr_string "get ";
            print_rib ~out:stderr !o;
            prerr_newline ()
          end;
        let opnd = get_opnd !o in
        push (get_car opnd);
        pc := get_tag !pc
      end else if i < 4 then begin (* const *)
        if !tracing then begin
            prerr_string "const ";
            print_rib ~out:stderr !o;
            prerr_newline ()
          end;
        push !o;
        pc := get_tag !pc
      end else if i < 5 then begin (* if *)
        if !tracing then prerr_endline "if";
        pc := if pop () == false_rib then get_tag !pc else get_cdr !pc
      end else begin (* halt *)
        if !tracing then prerr_endline "halt";
        exit 0
      end
  done
