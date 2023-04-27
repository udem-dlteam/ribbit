pub mod rvm {
    use std::fmt::{Display, Formatter};
    use std::cmp::Ordering;
    use std::cmp::Ordering::Equal;
    use std::collections::HashMap;
    use std::io::*;
    use std::ops::{Add, Div, Mul, Sub};
    use std::process;


    // Data representation as Rib
//
// Pair: car,cdr,0  (Rib,Rib,0)
// Closure Procedure: code,env,1 (Rib,Rib,1)
//       code: nparams,0,start (int,0,Rib: Operation)
// Primitive Procedure: code,dontcare,1 ([0..19],0,1)
// Symbol: value,name,2 (Rib, Rib: String,2)
// String: chars,length,3 (Rib,int,3)
// Vector: elems,length,4 (Rib,int,4)
// #t,#f,(): dontcare,dontcare,5 (0,0,5)
    const PAIR: i32 = 0;
    const PROCEDURE: i32 = 1;
    const SYMBOL: i32 = 2;
    const STRING: i32 = 3;
    const VECTOR: i32 = 4;
    const SPECIAL: i32 = 5;


    // Operation representation as Rib
//
// jump: 0,slot/global,0 (0,Rib: Symbol, 0)
// call: 0,slot/global,next (0,Rib: Symbol,Rib: Operation)
// set: 1,slot/global,next(1,Rib: Symbol, Rib: Operation)
// get: 2,slot/global,next (2,Rib: Symbol, Rib: Operation)
// const: 3,object,next (3,Rib, Rib: Operation)
// if: 4,then,next (4,Rib: Operation, Rib: Operation)

    const CALL: i32 = 0;
    const SET: i32 = 1;
    const GET: i32 = 2;
    const CNST: i32 = 3;
    const IF: i32 = 4;
    const HALT: i32 = 5;




    // putchar

    fn putchar(c: char) {
        let mut stdo = stdout();
        let binding = c.to_string();
        let c_buffer =binding.as_bytes();
        stdo.write(c_buffer)
            .expect("Failed to write to stdo buffer");
        stdo.flush()
            .expect("Failed to flush stdo buffer");
    }

    fn decode_char_to_u32(c: Option<char>) -> u32 {
        match c {
            Some(ch) => ch as u32,
            None => panic!("Unexpected end of input"),
        }
    }


    //VM

    use std::ops::{Index, IndexMut};
    use std::str::{Chars, from_utf8};

    #[derive(Copy,Clone,PartialEq,Eq)]
    struct Rib {
        first: RibField,
        middle: RibField,
        last: RibField,
    }

    impl Display for Rib {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f,"[f:{},m:{},l:{}]",self.first.to_string(),
                   self.middle.to_string(),
                   self.last.to_string())
        }
    }





    #[derive(Copy,Clone,Eq)]
    enum RibField {
        Rib(usize),
        Number(i32),
    }

    impl Display for RibField {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match *self {
                RibField::Rib(ref inner) => write!(f,"r{}",*inner),
                RibField::Number(ref n) => write!(f,"n{}",*n),
            }
        }
    }

    impl PartialEq for RibField {
        fn eq(&self, other: &Self) -> bool {
            match self {
                RibField::Rib(ref inner) =>
                    match other {
                        RibField::Rib(ref other_inner) =>
                            inner == other_inner,
                        RibField::Number(_) => false,
                    },
                RibField::Number(ref n) =>
                    match other {
                        RibField::Rib(_) => false,
                        RibField::Number(ref other_n) => other_n == n,
                    }
            }
        }
    }

    impl PartialOrd for RibField {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            if self == other {Some(Equal)} else {
                match self {
                    RibField::Rib(_) => {
                        Option::None
                    },
                    RibField::Number(ref n) => {
                        match other {
                            RibField::Rib(_) => Option::None,
                            RibField::Number(ref other_n) => {
                                n.partial_cmp(other_n)
                            },
                        }
                    },
                }
            }

        }
    }

    impl Add for RibField{
        type Output = Option<RibField>;

        fn add(self, rhs: Self) -> Self::Output {
            match self {
                RibField::Number(ref n) => {
                    match rhs {
                        RibField::Number(ref m) => {
                            Some(RibField::Number(n + m))
                        },
                        _ => Option::None,
                    }
                },
                _ => Option::None,
            }
        }
    }

    impl Sub for RibField{
        type Output = Option<RibField>;

        fn sub(self, rhs: Self) -> Self::Output {
            match self {
                RibField::Number(ref n) => {
                    match rhs {
                        RibField::Number(ref m) => {
                            Some(RibField::Number(n - m))
                        },
                        _ => Option::None,
                    }
                },
                _ => Option::None,
            }
        }
    }

    impl Mul for RibField{
        type Output = Option<RibField>;

        fn mul(self, rhs: Self) -> Self::Output {
            match self {
                RibField::Number(ref n) => {
                    match rhs {
                        RibField::Number(ref m) => {
                            Some(RibField::Number(n * m))
                        },
                        _ => Option::None,
                    }
                },
                _ => Option::None,
            }
        }
    }

    impl Div for RibField {
        type Output = Option<RibField>;

        fn div(self, rhs: Self) -> Self::Output {
            match self {
                RibField::Number(ref n) => {
                    match rhs {
                        RibField::Number(ref m) => {
                            Some(RibField::Number(n / m))
                        },
                        _ => Option::None,
                    }
                },
                _ => Option::None,
            }
        }
    }

    impl RibField {
        fn get_rib(&self, holder: &mut RibHeap) -> Rib {
            match self {
                RibField::Rib(ref inner) => holder.get(inner),
                RibField::Number(n) =>
                    panic!("Expected a rib reference but got the number {}",n),
            }
        }

        fn get_number(&self) -> i32 {
            match self {
                RibField::Rib(ref inner) =>
                    {panic!("Expected a number but got the rib index {}",inner)},
                RibField::Number(ref n) => *n,
            }
        }

        fn get_rib_ref(&self) -> usize {
            match self {
                RibField::Rib(ref inner) => *inner,
                RibField::Number(ref n) =>
                    panic!("Expected a rib reference but got the number {}",n),
            }
        }

    }



    struct RibHeap {
        heap:Vec<Rib>,
    }

    impl RibHeap {
        fn push_rib(&mut self, data:Rib) -> usize {
            let index = self.heap.len(); // len() is how many ribs are before the pushed one
            self.heap.push(data);
            index
        }



        fn with_capacity(capacity: usize) -> Self {
            RibHeap{
                heap: Vec::with_capacity(capacity)
            }
        }

        fn set(&mut self, i:&usize, r:Rib) {
            self[*i] = r;
        }

        fn get(&mut self, i:&usize) -> Rib {
            self[*i]
        }

        fn garbage_collect(&mut self, stack: &mut usize, pc: &mut usize,symtbl: &mut usize) -> usize {

            let mut new_heap = Vec::with_capacity(self.heap.capacity());

            new_heap.push(self.get(&0)); //FALSE
            new_heap.push(self.get(&1)); //TRUE
            new_heap.push(self.get(&2)); //NIL

            //Put every Rib referenced by a Rib in the stack, by a Rib in pc or by a Rib in the
            //symbol table in new_heap
            //Then, iterate through new_heap and change all RibField::Rib to their new index, as
            //recorded in index_correspondence


            if *symtbl<self.heap.len() {
                self.stop_and_copy(symtbl, &mut new_heap);
            }

            if *pc<self.heap.len() {
                self.stop_and_copy(pc, &mut new_heap);
            }

            if *stack<self.heap.len() {
                self.stop_and_copy(stack, &mut new_heap);
            }

            self.heap = new_heap;
            self.heap.len()
        }

        fn stop_and_copy(&mut self, root: &mut usize, new_heap: &mut Vec<Rib>) {

            let broken_heart = RibField::Rib(self.heap.len() + 1);

            // FR: Si le Rib référencé par root est déjà dans le new_heap alors, par récursion,
            // les Ribs auxquels il est connexe sont déjà copiés et il n'est pas nécessaire de poursuivre le copiage.
            // ENG: If the Rib referenced by root is already copied then, by recursion, the Ribs to which it is
            // connected are already copied and the copying doesn't need to take place.

            if self.get(&root).first == broken_heart
            {
                return;
            }

            // FR: Initialisation des pointeurs scan et copy. ENG: Initialization of the scan and copy pointers.

            let mut scan: usize = new_heap.len();
            let mut copy: usize = new_heap.len();

            let mut old_start = self.get(&root);
            let mut copied_rib = old_start.clone();

            // FR: Le marqueur va être écrit dans le champ first, l'adresse de sa copie dans le champ middle
            // ENG: The mark will be written in the first field, the address of its copy in the middle field
            old_start.first = broken_heart;
            old_start.middle = RibField::Rib(new_heap.len());
            self.heap[*root] = old_start;

            new_heap.push(copied_rib);
            copy += 1;

            // FR: Mise à jour du pointeur root. ENG: Updating the root pointer.
            *root = scan;

            while scan != copy
            {
                copied_rib = new_heap[scan];

                let mut is_changed = false;
                if is_rib(&copied_rib.first)
                {
                    is_changed = true;
                    let mut past_rib = copied_rib.first.get_rib(self);
                    if past_rib.first == broken_heart
                    {
                        copied_rib.first = past_rib.middle;
                    } else {
                        let past_rib_ref = copied_rib.first.get_rib_ref();

                        copied_rib.first = RibField::Rib(new_heap.len());

                        new_heap.push(past_rib.clone());
                        copy += 1;

                        past_rib.first = broken_heart;
                        past_rib.middle = RibField::Rib(new_heap.len()-1);

                        self.heap[past_rib_ref]= past_rib;
                    }
                }

                if is_rib(&copied_rib.middle)
                {
                    is_changed = true;
                    let mut past_rib = copied_rib.middle.get_rib(self);
                    if past_rib.first == broken_heart
                    {
                        copied_rib.middle = past_rib.middle;
                    } else {
                        let past_rib_ref = copied_rib.middle.get_rib_ref();
                        copied_rib.middle = RibField::Rib(new_heap.len());

                        new_heap.push(past_rib.clone());
                        copy += 1;

                        past_rib.first = broken_heart;
                        past_rib.middle = RibField::Rib(new_heap.len()-1);

                        self.heap[past_rib_ref]= past_rib;
                    }
                }

                if is_rib(&copied_rib.last)
                {
                    is_changed = true;
                    let mut past_rib = copied_rib.last.get_rib(self);
                    if past_rib.first == broken_heart
                    {
                        copied_rib.last = past_rib.middle;
                    } else {
                        let past_rib_ref = copied_rib.last.get_rib_ref();
                        copied_rib.last = RibField::Rib(new_heap.len());

                        new_heap.push(past_rib.clone());
                        copy += 1;

                        past_rib.first = broken_heart;
                        past_rib.middle = RibField::Rib(new_heap.len()-1);

                        self.heap[past_rib_ref]= past_rib;
                    }
                }
                if is_changed
                {
                    new_heap[scan] = copied_rib;
                }
                scan +=1;
            }

        }
    }

    impl Display for RibHeap{
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let mut record: String = String::new();

            let mut it = self.heap.iter();
            let mut current = it.next();
            let mut index: usize = 0;


            while current.is_some() {
                let current_str = *current.unwrap();
                let current_str = current_str.to_string();
                let mut entry_str = index.to_string();
                entry_str.push(':');
                entry_str.push_str(current_str.as_str());
                record.push_str(entry_str.as_str());
                record.push('\n');
                current = it.next();
                index += 1;
            }

            write!(f,"{}",record)
        }
    }

    impl Index<usize> for RibHeap{
        type Output = Rib;

        fn index(&self, index: usize) -> &Self::Output {
            &self.heap[index]
        }
    }

    impl IndexMut<usize> for RibHeap {
        fn index_mut(&mut self, index: usize) -> &mut Self::Output {
            &mut self.heap[index]
        }
    }


    const NIL: Rib = Rib {
        first: RibField::Number(0),
        middle: RibField::Number(0),
        last: RibField::Number(SPECIAL),
    };

    const NIL_REF:usize = 2;


    const TRUE: Rib = Rib {
        first: RibField::Number(0),
        middle: RibField::Number(0),
        last: RibField::Number(SPECIAL),
    };
    const TRUE_REF: usize = 1;

    const FALSE: Rib = Rib {
        first: RibField::Number(0),
        middle: RibField::Number(0),
        last: RibField::Number(SPECIAL),
    };

    const FALSE_REF: usize =0;

    fn make_rib(first: RibField, middle: RibField, last: RibField) -> Rib {
        Rib {
            first,
            middle,
            last
        }
    }

    fn make_data_rib(first: RibField, middle: RibField, last: i32) -> Rib {
        make_rib(first, middle, RibField::Number(last))
    }

    fn make_op_rib(first: i32, middle: RibField, last: RibField) -> Rib {
        make_rib(RibField::Number(first), middle, last)
    }






    fn show(o: &RibField, holder: &mut RibHeap) -> String{
        if !is_rib(o) {o.get_number().to_string()}
        else {
            let mut rib_o = o.get_rib(holder);
            let kind = rib_o.last;
            let mut result = String::new();
            match kind {
                RibField::Number(ref n) => match *n {
                    VECTOR => {result = String::from("#");
                        result.push_str(show(&rib_o.first,holder).as_str());
                    },
                    PAIR => { // Could also be tail call
                        let mut n =1;
                        result.push('(');
                        result.push_str(show(&rib_o.first, holder).as_str());
                        let mut o_middle = rib_o.middle;
                        while is_rib(&o_middle) &&
                            (!is_rib(&o_middle.get_rib(holder).last) &&
                                o_middle.get_rib(holder).last.get_number() == 0)
                        {
                            rib_o = o_middle.get_rib(holder);
                            if n > 4 {
                                result.push_str(" ...");
                                o_middle = RibField::Rib(NIL_REF);
                                break;
                            }
                            result.push(' ');
                            result.push_str(show(&rib_o.first, holder).as_str());
                            o_middle = rib_o.middle;
                            n += 1;
                        }
                        if o_middle != RibField::Rib(NIL_REF)
                        {
                            result.push_str(" . ");
                            result.push_str(show(&o_middle, holder).as_str());
                        }
                        result.push(')');
                    },
                    PROCEDURE => {
                        if is_rib(&rib_o.first) {
                            let rib_o_first = rib_o.first.get_rib(holder);
                            result.push_str("#<procedure nparams=");
                            result.push_str(rib_o_first.first.get_number().to_string().as_str());
                            result.push('>');
                        } else {
                            result.push_str("#<primitive ");
                            result.push_str(rib_o.first.get_number().to_string().as_str());
                            result.push('>');
                        }
                    },
                    SYMBOL => {
                        let mut field_o = rib_o.middle;
                        let mut cond = is_rib(&field_o);
                        if cond {
                            rib_o =field_o.get_rib(holder);
                            if (!is_rib(&rib_o.last) && rib_o.last.get_number() ==2) &&
                                (!is_rib(&rib_o.middle) && rib_o.middle.get_number() > 0)
                            {
                                field_o = rib_o.first;
                                while is_rib(&field_o) &&
                                    !is_rib(&field_o.get_rib(holder).last) &&
                                    field_o.get_rib(holder).last.get_number() == 0
                                {
                                    rib_o =field_o.get_rib(holder);
                                    let n =rib_o.first.get_number() as u32;
                                    let c = std::char::from_u32(n).unwrap();
                                    result.push(c);
                                    field_o = rib_o.middle;
                                }
                            }
                            else
                            { cond = false; }
                        }
                        if cond == false {
                            result.push_str("#<symbol ");
                            result.push_str(show(&field_o, holder).as_str());

                            result.push('>');
                        }
                    },
                    STRING => {
                        result.push('"');
                        let mut field_o = rib_o.first;

                        while is_rib(&field_o) && !is_rib(&field_o.get_rib(holder).last)
                            && field_o.get_rib(holder).last.get_number() == 0
                        {
                            rib_o = field_o.get_rib(holder);
                            let n = rib_o.first.get_number() as u32;
                            let mut c =std::char::from_u32(n).unwrap();
                            if c == '\n' {
                                c = 'n';
                                result.push_str("\\");
                            } else if c == '\r' {
                                c = 'r';
                                result.push('\\');
                            } else if c == '\t' {
                                c = 't';
                                result.push('\\');
                            } else if c == '\\' || c == '"' {
                                result.push('\\');
                            }
                            result.push(c);
                            field_o = rib_o.middle;
                        }
                        result.push('"');

                    },
                    SPECIAL => {
                        match o {
                            RibField::Rib(FALSE_REF) => result.push_str("#f"),
                            RibField::Rib(TRUE_REF) => result.push_str("#t"),
                            RibField::Rib(NIL_REF) => result.push_str("()"),
                            _ => {
                                result.push('[');
                                result.push_str(show(&rib_o.first, holder).as_str());
                                result.push(',');
                                result.push_str(show(&rib_o.middle, holder).as_str());
                                result.push(',');
                                result.push_str(show(&rib_o.last, holder).as_str());
                                result.push(']');
                            }
                        }
                    },
                    _ => {
                        result.push('[');
                        result.push_str(show(&rib_o.first, holder).as_str());
                        result.push(',');
                        result.push_str(show(&rib_o.middle, holder).as_str());
                        result.push(',');
                        result.push_str(show(&rib_o.last, holder).as_str());
                        result.push(']');
                    }
                },
                RibField::Rib(_) => {
                    result.push('[');
                    result.push_str(show(&rib_o.first, holder).as_str());
                    result.push(',');
                    result.push_str(show(&rib_o.middle, holder).as_str());
                    result.push(',');
                    result.push_str(show(&rib_o.last, holder).as_str());
                    result.push(']');
                }
            };
            result
        }
    }

    fn start_step(step_count: &mut u32, tracing: &mut bool, next_stamp: &mut u32,
                  start_tracing: &u32, stack: &usize, holder: &mut RibHeap) {
        *step_count += 1;
        if *step_count >= *start_tracing {
            *tracing = true;
        }
        if !*tracing {
            if *step_count >= *next_stamp
            {
                *next_stamp = f32::floor((*next_stamp as f32) *1.01 + 1.0) as u32;
                eprintln!("@{}",step_count.to_string());
            }
            return
        }
        let mut s = RibField::Rib(*stack);
        let mut rib_s = s.get_rib(holder);
        let mut result = String::new();
        result.push('@');
        result.push_str(step_count.to_string().as_str());
        result.push_str(" STACK = (");
        while !is_rib(&rib_s.last) && rib_s.last.get_number() == 0
        {
            result.push(' ');
            result.push_str(show(&rib_s.first,holder).as_str());
            s = rib_s.middle;
            if !is_rib(&s) {break;}
            rib_s = s.get_rib(holder);
        }
        result.push(')');
        eprintln!("{}",result);

    }



    fn is_rib(obj: &RibField) -> bool {
        match obj {
            RibField::Rib(_) => true,
            _ => false,
        }
    }




    fn to_bool<E>(expr: E) -> RibField where E: FnOnce() -> bool{
        if expr() { RibField::Rib(TRUE_REF)} else { RibField::Rib(FALSE_REF) }
    }
    // )@@





    //functions involving the stack

    fn push_stack(x: RibField, stack: &mut usize, holder:&mut RibHeap){
        *stack = holder.push_rib(make_data_rib(x,
                                               RibField::Rib(*stack),
                                               PAIR));
    }

    fn pop_stack(stack: &mut usize, holder: &mut RibHeap) ->RibField{
        let r = holder.get(&stack).first;
        *stack = holder.get(&stack).middle.get_rib_ref();
        r
    }

    fn rvm_getchar(stack: &mut usize, holder: &mut RibHeap) {
        let mut buf: [u8; 1] = [0; 1];
        stdin()
            .read(&mut buf)
            .expect("Failed to read character in standard input");
        let n = from_utf8(&buf).unwrap();
        let c =n.chars().next().unwrap();
        if c as i32 == 0
        {
            push_stack(RibField::Number(-1), stack, holder);
        } else {
        push_stack(RibField::Number(c as i32), stack, holder);
        }
    }


    fn rvm_prim1<F>(
        mut f: F,stack: &mut usize, holder: &mut RibHeap)
        where F: FnMut(RibField,&mut RibHeap) -> RibField{
        let x =pop_stack(stack, holder);
        let r = f(x, holder);
        push_stack(
            r,
            stack, holder
        );
    }

    fn rvm_prim2<G>(
                     mut f: G,stack: &mut usize, holder: &mut RibHeap)
        where G: FnMut(RibField,RibField, &mut RibHeap) -> RibField{
        let x = pop_stack(stack, holder);
        let y = pop_stack(stack, holder);
        let r =f(x, y, holder);
        push_stack(r,
                   stack, holder
        );
    }

    fn rvm_prim3<H>(
                    mut f: H,stack: &mut usize, holder: &mut RibHeap)
        where H: FnMut(RibField, RibField, RibField, &mut RibHeap) -> RibField{
        let x = pop_stack(stack, holder);
        let y = pop_stack(stack, holder);
        let z = pop_stack(stack, holder);
        let r = f(x,y,z, holder);
        push_stack(r,
                   stack, holder
        );
    }

    fn rvm_arg2(stack: &mut usize, holder: &mut RibHeap){
        let x = pop_stack(stack, holder);
        pop_stack(stack, holder);
        push_stack(x, stack, holder);
    }

    fn rvm_close(stack: &mut usize, holder: &mut RibHeap){
        let f = pop_stack(stack,holder).get_rib(holder).first;
        let m = RibField::Rib(*stack);

        let closure = holder.push_rib(
            make_data_rib(f,
                          m,
                          PROCEDURE)
        );

        push_stack(RibField::Rib(closure),
                   stack, holder);
    }

    fn list_tail(list: &usize, i:u32, holder: &mut RibHeap) ->usize{
        if i==0 {*list} else {
            list_tail(&holder.get(list).middle.get_rib_ref(),
                      i-1, holder)
        }
    }
    // End of functions involving the stack

    fn get_byte(iter: &mut Chars)-> u32 {
        decode_char_to_u32(iter.next())
    }

    fn get_code(iter: &mut Chars)-> i32 {
        // donne un nombre entre 0 et 92
        // Le bytecode de Ribbit n'utilise pas ' ' (ASCII 32), '"' (ASCII 34), et '/' (ASCII 47)
        let x= get_byte(iter) as i32 -35 /*35: ASCII pour '#'*/ ;
        if x<0 {57 /*57: ASCII pour '9'*/} else {x}
    }

    fn get_int(mut n:i32,iter:&mut Chars) -> i32 {

        let x=get_code(iter); // x entre 0 et 92 inclusif
        n *= 46; /* 46= 92/2, ASCII pour '.' */
        if x<46 {
            n+x // n*46 + [0..45]
        } else {
            get_int(n+x-46,iter) // passe n*46 + [0..46] à get_int
        }
    }



    fn symbol_ref(n: u32, symtbl:&usize, holder: &mut RibHeap)-> usize {
        let tail_ref = list_tail(symtbl, n, holder);
        holder.get(&tail_ref).first.get_rib_ref()
    }

    fn get_opnd_ref(o: &RibField, stack: &usize , holder: &mut RibHeap) -> usize {
        match o {
            RibField::Rib(ref r) => *r,
            RibField::Number(ref n) => list_tail(stack, *n as u32, holder),
        }
    }

    fn get_opnd(o: &RibField, stack: &usize , holder: &mut RibHeap) -> Rib {
        let index = get_opnd_ref(o, stack, holder);
        holder.get(&index)
    }

    fn get_cont(stack: &usize, holder: &mut RibHeap) -> usize {
        let mut s = *stack;
        let mut s_last = holder.get(&s).last;
        while !is_rib(&s_last) {
            let s_middle =holder.get(&s).middle;
            s = s_middle.get_rib_ref();
            s_last = holder.get(&s).last;
        }
        s
    }

    fn set_global(val_ref:usize,symtbl:&mut usize,holder: &mut RibHeap) {
        let sym_top = holder.get(symtbl);
        let mut top_first = sym_top.first.get_rib(holder);
        top_first.first = RibField::Rib(val_ref);
        holder.set(&sym_top.first.get_rib_ref(), top_first);
        *symtbl = sym_top.middle.get_rib_ref();
    }


    pub fn run_rvm() {

        let mut step_count:u32 =0;
        let start_tracing:u32 = 0;
        let mut next_stamp:u32 =0;
        let mut tracing = true;
        let heap_tracing = true;
        let mut debug = true;

        // tracing = false;
        // debug = false;
        // )@@

        let rvm_code: String = "RGgubed,llac/pmuj,htgnel-rotcev,?=<gnirts,trats-tni-llac,trohs-corp-tsnoc,raaaac,trohs-tni-teg,erutaef-enifed,esu,neg,sevitimirp-tluafed,epyt-gnirts,po-tsnoc,?=>rahc,raaadc,_,dne,dna,*tsil>-gnirts,po-tes,?naeloob,trats-tni-tsnoc,trats-tni-teg,tes,rdaadc,serutaef-denifed,!llif-rotcev,mcl,etouq,dnapxe-dnoc,xam,oludom,trats-corp-tsnoc,radddc,dnoc,yrarbil-daer,di@@,rts-ot-tuptuo-htiw,po-teg,epyt-riap,denifederp,epyt-erudecorp,cc/llac,adbmal,rdaddc,rdadac,dne-trats,raadac,trats-llac,tel,evitimirp-enifed,?>rahc,rotaremun,?tsil,rddddc,nruter-pp,roolf,trats-teg,epyt-notelgnis,yllamron-margorp-tixe,ytpme,margorp-daer,trohs-tni-tsnoc,ypoc-gnirts,enifed,yllamronba-margorp-tixe,?evitagen,kcehc-ytira,2/be,liat,gniliec,epyt-rotcev,?=<rahc,tegrat,daeh,trats-mys-teg,trats-tni-tes,trats-mys-llac,dnuor,elif-led,dbt,nim,rotanimoned,tropxe,trats-tes,trats-mys-pmuj,kcehc-ytira-erutaef##,etacnurt,radaac,slobmys-denretninu,noitacol@@,raddac,epyt-lobmys,rdaaac,trohs-mys-pmuj,tsoh,trats-mys-tsnoc,ro,trohs-mys-llac,xedni,reffub,fer-rav-labolg,foe,po-fi,cca,raaddc,trats-tsnoc,trats,teg,rddadc,*tel,tsnoc,nigeb,be,edoc-tegrat-etirw,trats-mys-tes,!tes,marap-tser,vne-erudecorp,trats-tni-pmuj,?=>gnirts,certel,trats-pmuj,trats-fi,enon,!llif-gnirts,tibbir,radadc,dmc-llehs,lbtmys,!tes-rav-labolg,?<rahc,ydob,?evitisop,?orez,rahc-etirw,po-llac/pmuj,sedoc>-gnirts,fi,sevitimirp-desu,sfed-teser,tsil>-elbat,edoc-etareneg,tros-tsil,enil-dmc,?ddo,rahcteg,tpxe,elif-tupni-htiw-llac,dcg,sevitimirp,elif-etareneg,lper,relipmoc-enilepip,orcam-tceted,stropxe-xtc,htap-elbatucexe,elif-tpircs,ssenevil,?htiw-trats-gnirts,?=rahc,gnirts-ekam,noisnetxe-htap-csr,htgnel-elbat,gnirts-ot-tuptuo-htiw,enil-txen,dnibnu-neg,!tros-tsil,riap-retlif,lobmys-denretninu>-gnirts,!tes-gnirts,rts,rebmun>-gnirts,margorp-elipmoc,vssa,?neve,?erudecorp,elipmoc,lave,elif-ot-tuptuo-htiw,?>gnirts,redniamer,edocne,?<gnirts,!tes-evil-xtc,rddaac,stropxe-tcartxe,?llac-si,xua-lobmys>-gnirts,tsil-dnapxe,yrotcerid-htap-csr,xua-rebmun>-gnirts,xua-sisylana-ssenevil,noitacol,fer-tsil,raadc,?regetni,repo,rebmem,serutaef-tcartxe,xua-gnirtsbus,2xua-rebmun>-gnirts,!tes-tsil,xua-pmc-gnirts,radac,elbat-ekam,xua-rahc-daer,edoc-erudecorp,?=gnirts,*dnib-pmoc,?tnatsnoc,!tes-elbat,radc,dnif,raaac,tnemmoc-piks,tsil-etirw,tsil-daer,?ni,tsila>-stropxe,erutaef,xua-tsil-ekam,rdadc,=<,xua-esrever,raddc,erutaef-esu,ecalper,xua-?tsil,evil>-stropxe,elif-morf-gnirts,liat-tsil,tixe,xua-dcg,evil-xtc,?2erudecorp,tnatsnoc-dnapxe,lla-daer,ydob-dnapxe,lave-dnapxe-dnoc,pmc-gnirts,elif-morf-daer,>,1tsil,xua-gnirts>-rebmun,*?htiw-trats-gnirts,raac,htgnel-reporpmi,3tsil,ylppa,xua-euqinu,rotcev>-tsil,2tsil,?rotcev,serutaef-evil-xtc,tsil-ekam,sba,enilwen,ecapsetihw-non-rahc-keep,rahc>-regetni,lobmys-daer,hguorht-epip,ecalper-gnirts,lobmys-denretninu>-rts,sisylana-ssenevil,tsil>-rotcev,fer-elbat,gnirtsbus,enil-daer,meti-tsal,evitimirp-dnif,!tes-2dleif,evitimirp,serutaef-desu,serutaef-tceted,enod-ydob-dnapxe,2rahctup,rid-toor,rdddac,poon-neg,ngissa-neg,!rac-tes,cossa,llac-pmoc,?ecnatsni,tsil>-tsil-reporpmi,euqinu,dnapxe-htap,srahc-daer,esac,lla-daer%,etanetacnoc-gnirts,esle,rts>-lobmys,rahc-keep,fi-dliub,regetni>-rahc,?evil,?tcejbo-foe,gnirts>-lobmys,sesualc-dnapxe-dnoc-dnapxe,elif-tsoh-esrap,nigeb-dnapxe,dnib-pmoc,etirw,*nigeb-dnapxe,!tes-1dleif,hcae-rof,dnpo,fer-gnirts,dlof,retlif,gnirts>-maerts,tcartxe,rdddc,txen,qssa,!rdc-tes,tneitouq,setyb-ot-edoc-mvr,pukool,evil-dda,srahc-etirw,?lauqe,gnirts>-rebmun,cossa-tfos,gnirts-tupni-nepo,rdaac,htgnel-gnirts,=,redro-evitimirp-tes,selbairav-erutaef-dda,gnirts>-tsil,?gnirts,daer,xeh-daer,erutaef-lave,!tes-0dleif,lobmys-esu,?rebmun,tes-etc-xtc,esrever,lobmys>-gnirts,bir-yalpsid,dnetxe,tsil>-gnirts,rahctup,?llun,?lobmys,lave-ecalper,llac-neg,nigeb-pmoc,vmem,rpxe-dnapxe,*gnirts>-tsil,htgnel,rddac,?qe,*,rorre,rahc-daer,etc-xtc,qmem,dneppa,<,0dleif,2dleif,rddc,1dleif,dneppa-gnirts,?bir,-,+,pam,rotcev-ekam,rdac,ton,?riap,esolc,rdc,xtc-ekam,?vqe,erudecorp-ekam,sgra-bn-dda,rac,pmoc,2gra,snoc,!tes-rotcev,pp,=>,fer-rotcev,yalpsid,di,1gra,lin,eurt,eslaf,bir;8V2!X&i[$!V2)YV=YULYT:i$EEi&wW(wX,i$ki$i$i$i$kiZ6y!W+8UFN)by^)_~Z@iZD^}'!SO7&i&kkF^[$N8S2iZIZ.EaYSJZ;h-_f7$UlfF^[$N7/fTldb7'Ul^~YV+ZN`h1ZNT`dh/70EEh.YSJh.gh3h4_^Th/c~YEk^{i$~YTHZ;gTf_})i$Z;aZ;_})]D8S2`SZ*_N9?YS-^{}'!V=(^YSP_YS6YS@WWiZ)fiYL(^^~FZ@iZ9f(^YSP_YS6YS@WWiZ)fiYL(^^~F^~^Ph.YV4bbcah.(^YSP_YS6YS@WWiZ)fiYL(^^~FZ@iZ9f(^YSP_YS6YS@WWiZ)fiYL(^^~F^~^Ph.X$vS#~Z@i[6dN(^>i$(^>@iZM>@Z;^>@iZB~Blh-Wi[)(^>i$(^>@iZM>@Z;^>@iZB~Blh-WYTAh.~h.YJ_i[,(^>i$(^>@iZM>@Z;^>@iZB~Blh-Wi[)(^>i$(^>@iZM>@Z;^>@iZB~Blh-WYTAh.~h.YUCeeee~KvS#^{Aoh-AngAmfAleAkd}1!TA(^(iZ4~YS+^YV7N8SIi$^{^{!V49JiZ?`^[$N8S4YS1gFX$FFFWYSOaaX2iZHh/Z&h8YMh-^8S4YS1gFX$FFFWYSOaaX2iZHh/Z&h8YMh-YS*^~Z'^QfYTDFX$FFWZJi[&`h2h-ZLh5N(i$(i$93h8^~YLdQ_~_Q_Z>_wWC{QeYU8FX$WX-i[-eYSEFX$WX,i[7dYTEFX$FFWYT.Sh4`i@eN7$h-F^[$N(iYK8@X(M_i$8@X(M_Qb~YLwX%H^8@X(M_c~YLwVGH^8@X(M_Z?f8@X(M_Z?Hf~Of~YLwW:H^8@X(M_H^~Z6H^~O^{i$Z>awX%ZJiZ*Ei&`h4i$7$h-F^[$N(iYK8@X(M_i$8@X(M_Qb~YLwX%H^8@X(M_c~YLwVGH^8@X(M_Z?f8@X(M_Z?Hf~Of~YLwW:H^8@X(M_H^~Z6H^~O^{i$Z>awX%ZJiZ*Ei&`h4YJaiZ5~P^YSGh7_Q_H^{MZ>cwY8X.FX$Fc8S4YS1gFX$FFFWYSOaaX2iZHh/Z&h8YMh-^8S4YS1gFX$FFFWYSOaaX2iZHh/Z&h8YMh-YS*^~Z'^QfYTDFX$FFWZJi[&`h2h-ZLh5N(i$(i$93h8^~YLdQ_~_Q_Z>_wWC{QeYU8FX$WX-i[-eYSEFX$WX,i[7dYTEFX$FFWYT.Sh4`i@eN7$h-F^[$N(iYK8@X(M_i$8@X(M_Qb~YLwX%H^8@X(M_c~YLwVGH^8@X(M_Z?f8@X(M_Z?Hf~Of~YLwW:H^8@X(M_H^~Z6H^~O^{i$Z>awX%ZJiZ*Ei&`h4i$7$h-F^[$N(iYK8@X(M_i$8@X(M_Qb~YLwX%H^8@X(M_c~YLwVGH^8@X(M_Z?f8@X(M_Z?Hf~Of~YLwW:H^8@X(M_H^~Z6H^~O^{i$Z>awX%ZJiZ*Ei&`h4YJaiZ5~P^YSGh7_Q_H^{MZ>cwY8X.FX$FWX+iZAc~Z3h/^QaYTKFX$WQaaYUNH^})i$}-]&8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~KnYN^~YLH_xD~O^7%Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~KnYN^~YLH_xD~O^7%Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~i$8J_iZN(^~Z0^(^~Z6^9DZ&aYM_Z&`Q^~KnYN^~YLH_xD~O^7%Z&`Q^~KmYN^~YLH_wUC~O^}'!SG(i$8SGM`^1_~i$8SGM`^1_~YLZ<H`^~YLwSEHH_~O_}'!SD7%c_F^[$N(b7-`YS7YFc^~O`ZMai&N(`8FZLM`N89YGh;^{`~^Z>_wY9}'M_H^YV#`N8S4YS1YJiZCFX$Z3eQaX%FX$YGdZ<`YSEH^{}'i$YFZLYFSbj<dN89YGh-^{Ei&w#ZL_N8Gg_(^~^YGe^Z<^{ZL_N8LwSEH^{}+!V#7&i&i&`F^[$N.a_7+EcH``M^7+bEaH_M^~X*H^~O^})i$}']389PYG`^89PZ#SM`N93d^{i%~i$89PYG`^89PZ#SM`N93d^{i%~KwW<H^~O^89Z#SM`N93d^{i$~i$89PYG`^89PZ#SM`N93d^{i%~i$89PYG`^89PZ#SM`N93d^{i%~KwW<H^~O^89Z#SM`N93d^{i$~KwY0H^~O^89Z3`Q^~i$89PYG`^89PZ#SM`N93d^{i%~i$89PYG`^89PZ#SM`N93d^{i%~KwW<H^~O^89Z#SM`N93d^{i$~i$89PYG`^89PZ#SM`N93d^{i%~i$89PYG`^89PZ#SM`N93d^{i%~KwW<H^~O^89Z#SM`N93d^{i$~KwY0H^~O^89Z3`Q^~Kw9H^~O^}'!S78T-i&^{!T-(_8T-EaH_M^8T-`M^~YG`H^~O^}'!S(7'bki&_F^[$N9.E`EEi&YObbwUN8JiZ77;h2kEgYFEEi&b`bh/EEi&`wX%EEi&Ei&EEi&`wUNwVGYOgh.Z5Z=^YOUo_YB_M`H_~KwXA`7=dkEh.YFEEi&aadaEEi&`wVGEEi&YOh0h2wX%M_H^YS(gZ5Z=Wi[*^YOUm_YB_M`H_~KwW2`74h0Th/d_c~KwVO`.Z._c~KwY1`e8JiZ77;h2kEgYFEEi&b`bh/EEi&`wX%EEi&Ei&EEi&`wUNwVGYOgh.Z5Z=^YOUo_YB_M`H_~KwXA`7=dkEh.YFEEi&aadaEEi&`wVGEEi&YOh0h2wX%M_H^YS(gZ5Z=Wi[*^YOUm_YB_M`H_~KwW2`74h0Th/d_c~KwVO`.Z._c~KwY1`EfEEi&YOh-h-wUN~FKwXA`8JiZ77;h2kEgYFEEi&b`bh/EEi&`wX%EEi&Ei&EEi&`wUNwVGYOgh.Z5Z=^YOUo_YB_M`H_~KwXA`7=dkEh.YFEEi&aadaEEi&`wVGEEi&YOh0h2wX%M_H^YS(gZ5Z=Wi[*^YOUm_YB_M`H_~KwW2`74h0Th/d_c~KwVO`.Z._c~KwY1`e8JiZ77;h2kEgYFEEi&b`bh/EEi&`wX%EEi&Ei&EEi&`wUNwVGYOgh.Z5Z=^YOUo_YB_M`H_~KwXA`7=dkEh.YFEEi&aadaEEi&`wVGEEi&YOh0h2wX%M_H^YS(gZ5Z=Wi[*^YOUm_YB_M`H_~KwW2`74h0Th/d_c~KwVO`.Z._c~KwY1`EfEEi&YOh-h-wUN~F^~^KwW2_8JiZ77;h2kEgYFEEi&b`bh/EEi&`wX%EEi&Ei&EEi&`wUNwVGYOgh.Z5Z=^YOUo_YB_M`H_~KwXA`7=dkEh.YFEEi&aadaEEi&`wVGEEi&YOh0h2wX%M_H^YS(gZ5Z=Wi[*^YOUm_YB_M`H_~KwW2`74h0Th/d_c~KwVO`.Z._c~KwY1`e~Kkf8JiZ77;h2kEgYFEEi&b`bh/EEi&`wX%EEi&Ei&EEi&`wUNwVGYOgh.Z5Z=^YOUo_YB_M`H_~KwXA`7=dkEh.YFEEi&aadaEEi&`wVGEEi&YOh0h2wX%M_H^YS(gZ5Z=Wi[*^YOUm_YB_M`H_~KwW2`74h0Th/d_c~KwVO`.Z._c~KwY1`EfEEi&YOTh.ch-wUN~KwY1_M_H^YV1_aM_H^YV&^~O^}+i${!Y/8<Z*_iS-{!O7%k`F^[$N(c7)Tl`M^>YUOYT$H``c~YEf_}'i$YV*YT$vR%_}'!V17'ki$a_F^[$N7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~KvR5Q^~KvR5Q^~KvLH^7-n`Un`ZI^8JiZO~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~KvR5Q^~KvR5Q^~KvLH^7-n`Un`ZI^8JiZO~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~i$7-d7-Tld~cbUl`M^.i&wY1.ETdnawXA~`~KvR5Q^~KvR5Q^~KvLH^7-n`Un`ZI^8JiZO~`~KvKYM^~KvR5Q^~KvR5H^.i&wVO.ETdlawW2~`~YTHm_}+i$}'!V&7%k_F^[$N7)Tl`M^.Tal.`~PO_i$.Tal.`~PO_M^~O^~FKuH_7)Tl`M^.Tal.`~PO_i$.Tal.`~PO_M^~O^~F^~^PO^}'i${]J9Mbb^[$N7.`caN(^9MQc_g~a{Z>M`wVGH^}'i$})!U29Ji&_N8S4YS1cFX$EdbYTKFX$EcaYSEFX$FYFc^X(i&YV5H^}){!X8(_>X$_}']>8U$`N(i$8LbH^~O^{}'!U$(i$8U$M`^1_~X$H_~O_}'!X28FYT6`8FYT:~Z@iYG`YT.S`iXLiF}'!XL8T6^8T6YS6YS6YS@i[3WiZ1^~Z@iZ%YV)^{!T68S3^>i$8S3^>YSI^~i$8S3^>i$8S3^>YSI^~i$8S3^>i$8S3^>YSI^~KvDYS-ZNl_~KvFYS-ZNk_~YT5lZ;_Z=^YTA^{!T:.YT:^(i&~YS+^Z5y!S3iT:!S@8U;FYV/8U;F^~^YV.y!VB8<Z*_iS-{]K97S_N8T$^8T$vD~Z:vS#^TvF^{{!UC7&i&`kF^[$N7&ca_F^[$N7&cg_F^[$N7$aF^[$N8@WWZKaiZKYS2iZ>SaN97Z.Z*^YS0^{ZKXAi&UYN`YNeX>i&hR%7'M^~i$8@WWZKaiZKYS2iZ>SaN97Z.Z*^YS0^{ZKXAi&UYN`YNeX>i&hR%7'M^~YU)iYDYS0H^~O^{i$70Eh-aMeTlc>Z2d^MaYSNiYO7.eMca~^ZGhR%^H^H_~O_})i$7.eMca70Eh-aMeTlc>Z2d^MaM^~^ZGhM^H^H_~O_})i$70Eh-`MeTlc>Z2d`YSNi['70Eh-`MeTlc>Z2d`M^~^ZGhH_M_H^H_~i$7&ca_F^[$N7&cg_F^[$N7$aF^[$N8@WWZKaiZKYS2iZ>SaN97Z.Z*^YS0^{ZKXAi&UYN`YNeX>i&hR%7'M^~i$8@WWZKaiZKYS2iZ>SaN97Z.Z*^YS0^{ZKXAi&UYN`YNeX>i&hR%7'M^~YU)iYDYS0H^~O^{i$70Eh-aMeTlc>Z2d^MaYSNiYO7.eMca~^ZGhR%^H^H_~O_})i$7.eMca70Eh-aMeTlc>Z2d^MaM^~^ZGhM^H^H_~O_})i$70Eh-`MeTlc>Z2d`YSNi['70Eh-`MeTlc>Z2d`M^~^ZGhH_M_H^H_~YEiW;^~O_})i$YV<YV>h<N8EX)_X(_}'>X4h<>X0h<>ZPi[1N73n^{[$N8D^M`8=YN_vS[rz0~^YU3i[1^H^{[%N8Ji&i[/8J_iZ@7+X,EciVPZO`ZH_~KiW6^7+FYJ_iZL7+FX.c^~YT<^7+FX2dX3_iW=~Z'^7+FX3d_iY,7+FEcT_iW3~YEv.^~Z0^ZO`ZH_~KiY5^7+FYJ_iZ'7+FX2dX3_iX$~Z'^7+FX3d_iY+7+FEcT_iX6~YEu^~Z0^ZO`ZH_~KiXI^7+FYJ_iZ27+FX2dX3_iW*~Z'^7+FX3d_iX#~Z0^ZO`ZH_~KiY.^7+FYJ_i[47+FFX3e_iWP7+FFEdT_iX?~YEiW;^X2^~Z'^7+FX3d_iY>~Z0^ZO`ZH_8J_iYJ72d_iWH.cT_k~YEv7^X1^~Z'^72c_iW&~Z0^ZO_~KZH`k~KiVC^YU4^~V^}'[&N7,X3d`ToiXO7,EcT`iXO~YEo_ZH_YD^YU*^}'['N71e_`(^~Z:k`EbTiX+^71e_`(^~Z:k`Eb^~KcaUYKiX+__ZEiX+^})[(N7-a`^}'[)N.``.M`Tl`~Z:TliX+^H^X.a_})[*N.X/b`^})[+N8D^YSKi$_h>{[,N(i$75n^~KiY._76l^>YS#_a>Z2iXIaX7^75m^~FFi$76l^>YS#_a>Z2iXIaX7^75m^~FFBk`~Z0`76l^>YS#_a>Z2iXIaX7^75m^~FF^~^YT<_76l^>YS#_a>Z2iXIaX7^75m^~F^~^Z'^~KiY5_75l^~KiXI_75k^~KiVC_72^~KiW6_ZO_YU4^{[-N(i$70ZH^>X/^~V^{[.N(i$74^~YT<^(i$8SFTYC`l^~Z:m`8S#TYA`l^~Z:l`92TYD`l^~Z:k`FF^>YU&`ahA:kkk(i$8SFTYC`l^~Z:m`8S#TYA`l^~Z:l`92TYD`l^~Z:k`F^~^YSKi$_h?~Z'^}'[/N(^>X3`^}'[0N70ZHYU*^{[1N(^>X3k^{[2N(^>X5n^>X>_`[8Tlh7Z-WZ?h7iYM8:^~^ZGh>^(w&~Ki&^(w%~Ki%^(w$~Ki$^{[3k[4N8SFX<X:YC_^YD^{[5N7%`h>F^[$N(_7-XD:gbiY._MbYB`Q_H^H^~O^}'i${[6N#X=bYC`YA_YD^(_~Kk^}'[7N7%aMhCF^[$N(_7)FFb7)FFX-c_~i$7)FFb7)FFX-c_~FFYEYCak7)FFb7)FFX-c_~FF^~^YEYA`k7)FFb7)FFX-c_~F^~^YEYD_k~^YSKi$_hFHH_M^~O^}'i$[$N(`#::XJ::h0XCnh.iY.XAkw#iVCnliY5kiY5_iY5~Z0^QZGhG^}'i${[8N(`[@Eh@EE`a_X>k^}'[9N8J_i[87?X@:XD:gX;kw#iVCnoiY5YN_^YSL^~YT*^7?X@:XD:gX;kw#iVCnniY5YN_^SZ*_iS-~Z6^7>X?:XC:fX:kw#iVCnkiY5M_H^~O^#a_iY5#:XC:fX:kw>iVCmUbkiY5kiY5~YEk^~Z0^#aX5m_iY5~Z'^#bX6l_iXIX6^~FZGh@_8J_i[87?X@:XD:gX;kw#iVCnoiY5YN_^YSL^~YT*^7?X@:XD:gX;kw#iVCnniY5YN_^SZ*_iS-~Z6^7>X?:XC:fX:kw#iVCnkiY5M_H^~O^#a_iY5#:XC:fX:kw>iVCmUbkiY5kiY5~YEk^~Z0^#aX5m_iY5~Z'^#bX6l_iXIX6^~F^~^Z#i[.^}'[:N(_#a_iY5~i$(_#a_iY5~YGhDwX,~hC}'[;i&[<YU,i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$i$}+!VPTTloiXO!XOTmiW=!W=TliY,!Y,Tv.iW3!W3TmiX$!X$TliY+!Y+TuiX6!X6TmiW*!W*TliX#!X#TkiWI!WITmiWP!WPTliY>!Y>TiW;iX?!X?TmiWH!WHTliW&!W&Tv7k!W#k!W;UTTTTv4v7uov.vS#!W?v7!Y=o!X1v.!Y;u!X+ZEmvS#!W,vS#!V-(c>X'i%i&b[$N(i$7)`M^>X+i$`H^~O^}'[%N(i$7+a_7.i$Z+c_YM`Q_~KwXD^7+aM_~KwW-^7.i$YFcS`i1YM`>X,bS_i:Q_~KwX>^7-i$aYS?_>X-i$aYM_>X-i$aQ_~KwVA^(i$70i$d_>ZFEM``^~^YS,h1_>i$(i$70i$d_>ZFEM``^~^YS,h1_>X/_~h17/i$c^~YTMc_YM`Q_~KwW)^7-^Q_~KwY%^H^~O^7,^(i$~YTM`^~Z'^})[&N(i$9PYSL_c~YT*^7)M^>X)H^~O^7*^~Z'^{['N(k[,ZBg^{i$i$i$i$})!TM89KZCka_^}'!U'(i$(i$(i$4HQ_wY%~OQ^~Z(YB^~OM^{!S,(i$8S,Ma_(^~KH__H_~O_}']B.a^Ei&^(_~YS,`^}'!V?7$_F^[$N(i$7'M^>ZFi&H^~O^{i${!TB(i&.YTBM_Ei&HH^~O^{!U97$ZBZBZBZBYTBFi&7$ZBZBZBZBYTBF^~^dw(w7w/w'F^[$N7(^(_~K`^YV-Pe_b>YV?^{i$}'!SM(^8U9a_~YS;_wVJYU9i&^}'!U<(i&.YU<M_YPH^~O^{!S)(_8S)aM_8S$aM^~FYT8H_8S)aM_8S$aM^~F^~^KH_wS1H^~O^}'!T84wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~i$4wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~KwW<H^~O^89Z#SM`iT8i$~i$4wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~i$4wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~KwW<H^~O^89Z#SM`iT8i$~KwY0H^~O^89YT8Q^~i$4wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~i$4wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~KwW<H^~O^89Z#SM`iT8i$~i$4wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~i$4wVM^4Z-i[$Q^~i$4wVM^4Z-i[$Q^~KwW>H^~O^89PZ#SM`iT8i%~KwW<H^~O^89Z#SM`iT8i$~KwY0H^~O^89YT8Q^~Kw9H^~O^{!S$(_._YP_8S)_M_~i$._YP_8S)_M_~KwY$H_~O_8S$_M_~i$._YP_8S)_M_~i$._YP_8S)_M_~KwY$H_~O_8S$_M_~KwW-H_~O_YS$aM_H^~O^}'!S'8T;k1^._wW-~OM^~O^YS$i&^{!SB8S'_8PEEaZ._wW$~O^}'!T97%i&_F^[$N8SBiZG_8SB``7+EcEYBa_M`7+EcEEi&EEYBcMawXDH_M`~O^Q^~i$8SB``7+EcEYBa_M`7+EcEEi&EEYBcMawXDH_M`~O^Q^~i$8SB``7+EcEYBa_M`7+EcEEi&EEYBcMawXDH_M`~O^Q^~OM^~KH_wX/~O^H^~O^}'i${!T;.Ei&_wY%{!P8T;^8U<_8Pi$8PEEEEi&EYBcwXMEMQbwW-HQ`wVA8PEMQ`wW-~KHQ`wS1~OM_~KwXM^8Pi$8PQ_8PEEEi&EEEEi&EYBewW<wY2wY2wVAEi&EEi&QawY2wX>~OYB_~OM_~KwW<^8Pi%8PQ_8PEEEEi&i$EYBbwY0Q`wVA~OYB_~OM_~KwY0^(i$>ZP_N(k!Y(YFEi&EEEEEi&EEi&Ei&EEi&QewUNwVGfEEi&HbwWCfwTKiY({YBb(i$>ZP_N(k!Y(YFEi&EEEEEi&EEi&Ei&EEi&QewUNwVGfEEi&HbwWCfwTKiY({ZIb~_i&(i$>ZP_N(k!Y(YFEi&EEEEEi&EEi&Ei&EEi&QewUNwVGfEEi&HbwWCfwTKiY({YBb(i$>ZP_N(k!Y(YFEi&EEEEEi&EEi&Ei&EEi&QewUNwVGfEEi&HbwWCfwTKiY({ZIb~_YMa~^YLwY9YU@`Q_8JiZ<~PiY(~KwY:H_.EEi&Eew#awW)!Y(YFEi&EYFYFEi&EEi&gwXKEi&EEi&Ei&EEi&cwUNwVGcwSEiY(YJiZ;.EEi&Eew#awW)!Y(YFEi&EYFYFEi&EEi&gwXKEi&EEi&Ei&EEi&cwUNwVGcwSEiY(H^~KlYN^ZLMcj6YT1^ZLMai8EEEi&EEi&lwY%EEi&kwY%wWM8JiZ/~PiY(~KwX=H_.EEi&YPYMb_wW).EEi&YPEEYBdMawXDH_wW)~O^Q_~KwX/^8S'M_~KwW-^8PEEYFYBcSaN.EEi&Q`H_wW){S`N.Ei&i$H^{wX>Q_~KwW$^8PEMa8PEEEi&EEYBdMawW/Ei&H_~i$8PEMa8PEEEi&EEYBdMawW/Ei&H_~OM_~O_wX>Q_~KwW/^8T9YBa.EEi&YT9YBcS`N.Ei&YPQ_H^{wX>~O^^8PFES`i:EEEi&aEi&EEi&EEZIeSbi1wXD`wW$YM`~Z'^Q_~KwX>^.EEi&YT9YBb_wXDQ_~KwXD^.EEEi&i$.EEEi&YPYS?b~OZIbYPYMaYPQ`wVA~KwVA^.EEi&YPYMb_wW)Q_~KwW)^8T;Q_~KwY%^H^~O^(^~Z'^{!Y(i&!UL(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^___(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^__YSDh4a_a~h/S`i1YS7YFh/YSC^YSMc_YFaiY(YS'ak!Y(i&i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^___(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^__YSDh4a_a~h/S`i1YS7YFh/YSC^YSMc_YFaiY(YS'ak!Y(i&YU2c~cYTLM`Ei&i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^___(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^__YSDh4a_a~h/S`i1YS7YFh/YSC^YSMc_YFaiY(YS'ak!Y(i&i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^___(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBi[%(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpFSeN._^H^{(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LFi&(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>i$(_>i$(_>CAn_>@iZ#~Bnh4>CAm_>@iYE~Bmh4>CAl_>@iYN~Bnh4>CAk_>@iZ+~Bmh4>Dh.o_>Den_>Dcm_>Dbl_>DJi&:GiX*gbkmk_!X*IiX*l^LF^~^eafi&RpF^~^h-~PYGawYBZ9d_~h1Z8eF^~^__YSDh4a_a~h/S`i1YS7YFh/YSC^YSMc_YFaiY(YS'ak!Y(i&YU2c~cYTLM`^~O^H^YU?b}-!SC9M`i&N(_(`.EbwX,wW(~i$(`.EbwX,wW(~i$(`.EbwX,wW(~Fi$(`.EbwX,wW(~FPYLi&YSHQ_~OQ_(`.EbwX,wW(~F^~^Z'Q^~YLwXDH^~O^Q^~i$(_(`.EbwX,wW(~i$(`.EbwX,wW(~i$(`.EbwX,wW(~Fi$(`.EbwX,wW(~FPYLi&YSHQ_~OQ_(`.EbwX,wW(~F^~^Z'Q^~YLwXDH^~O^Q^~OM^~O^}'{]8.YFMe_wW-ZMbi&N(_.`EEEi&i$_wW)~YV,dYS*^.`EEEi&i%_wW)~YGc^}'SaN9-WYS*_a{iZ0})!V,8T2Z*`Z*^}'!T289O_(i$8T2M`M^~KH`H^~i$89O_(i$8T2M`M^~KH`H^~O_~O^}']99MbiZ=N(_8FEi&EEi&eZ<``>i$8FEi&EEi&eZ<``>YS<EEi&ewY%Q^~^[)TlcZ>_wXK~i$(_8FEi&EEi&eZ<``>i$8FEi&EEi&eZ<``>YS<EEi&ewY%Q^~^[)TlcZ>_wXK~i$(_8FEi&EEi&eZ<``>i$8FEi&EEi&eZ<``>YS<EEi&ewY%Q^~^[)TlcZ>_wXK~PYLw#Z<^~YGdZ<^~YLwSEH^}'k}'!V@._w#ZMbi&N.`H^(_~FPYGdH_.`H^(_~F^~^YLw#H^}'S_iU@}'!TL(^8<_N.Q_H^._^~Z'^{~O^{!U?7&i$i&_F^[$N.aZ._7,cEb_M_7,YFFi&7,YFF^~^dM`aM_~i$7,cEb_M_7,YFFi&7,YFF^~^dM`aM_~KwWJH^~O^H^~O^})i${!X*:kw(iVC]+(_.Z+aM_H^~O^}']C(^9CTlbM`^(`~K_H_~O_})!S:7&^0YS:dMbZ/EYHbi$`H`^~O_})]$0b0I:Z$h-MfdZ1w'ciVCm`~OMaH`^})]1(_>YUAZBYT=``^}'!V%2:cZ1w/aiVCm^(_~KiX*_}'!2(`#b`iY5~YGYT)_wX,})!U(9$eca0YU(YV%h2gh/Z/EYHh/eh-MfMdZ/EYHdi$b_`HaH_~O_}/!S&8U(geab`^}-!S>#akiY58C_8CYC_~YGYT)_wX,~YU>aw'^}'!U>(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`VYC^~KbYA^~KiVCYD^~^i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`VYC^~KbYA^~KiVCYD^~^a~Va(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`VYC^~KbYA^~KiVCYD^~^i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`i$(^(i$(i$(i$89VYAc~KiY5YDc~Vc~^~`VYC^~KbYA^~KiVCYD^~^YCa~Va~^YGYT)_wX,})!S=#YS>c``iY.})]%#a_iVC#k_iVC~KiX*_}'!0#b`iY58S&fEi&EbwY2Ei&aiZJ`8S:N2Z%h0`2Z%h0Tl`~i$2Z%h0`2Z%h0Tl`~YGYT)bwX,~Z0`YNe_ZCkYH_d{_`~Z'_M`8S&gaSbi:Sai1aYBaQ`~KwX>^9$cMa_~KwW-^#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5`#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5YS8i&`~_YN_#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5`#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5YS8i&`~_YT0_~^FPYLi&YSH_#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5`#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5YS8i&`~_YN_#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5`#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTk#IZ%h/Z1w7gle#g~Z(YHeJi&:Z$iX*YBh-Z/Z+EEYHh/i$i$bfkTl~bYKamiY5YS8i&`~_YT0_~^F^~^Z'^Q`~KwXD^0`Qdb:a_iW6GdYMb`GcYS?a_~KwVA^0YS=h-ad`b8S>fc0YS=h.beac8S>fc~i$0YS=h.beac8S>fc~PYS;YV0da~YU'^~^YS,YT=c`~Ka^ZClYHb_YMaQ`~KwW)^#cQaiY5~KwY%^H_~O_#c_iXI#d`iXI#dQQ_iY5~i$#d`iXI#dQQ_iY5~YU'^~^YS,YT=``~Ka^ZCkYH__~Z'_})!S89.E`^8S8EaH_M^~O^}'!T0(k8=YT0M_l~O^{!SH(^8SHM^~O^{!UA92`YA^}']/#YC`YA__}'!V08C^{!T)8AYA^{!T=8DYA^{!H8D^{!5#b:i$c`^}+]H8C^{]O8A^{!U48D^{!T<iUI!W6wVA!Y5wW.!XIwW1!Y.wY*!VCwYA!Y7i[%!XGi[1],)i[2)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>@YA^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>Z,Ul`YA^~VYA^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>@^~PV^>@iZ&>@YD^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>@YA^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>Z,Ul`YA^~VYA^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>@^~PV^>@iZ&>Z,Ul`YD^~VYD^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>@YA^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>Z,Ul`YA^~VYA^)iYF>@YC^)iYF>Z,Ul`YC^~VYC^)iYF>@^~PV^>@iZ$>@^~PV^>@iZ&>@^~PV^>@iYH~YT5k_}'!S2(iZ87%Z*HaM`F^[$N97_7)YFYFbeZ*H_M^~O^}'i$Z*`Z.^~O^}'!V/(iZ,y!V.1YV;y!V<8V$YFi&`^}'!V$7(_c(i&~Z:_kYNb[$N(^>ZFb_(^>ZFa_7/fMdc`>ZFca7/Mfdd`>ZFda~X0`^HbH`~Oa~O`}+[%N(_(^7-dMba`7-Mdbba~X.`^H`H^~O_~O^}'[&N7%a_F^[$N71_X2eeX1Uef_7)M`Ul^~YT5k^}'i$ZEm_(^>ZFi&^~Z:l_}'i$i$i$}'!,8T&>YS%^{!SI7$i&F^[$N7(E`^97Z._~FKf_7(E`^97Z._~F^~^YS+^YIb{i$}'!S68@W`iZP_8@__~Z:vR$YS-ZNUlZ;`_(^~Z:kZ;_}'!U;7$UlZ;_F^[$N7'Ul^8SJTl`kb~Z:vR$YS-ZN_b(iZE~YEk^{i${!V)7$UlZ;_F^[$N7'Ul^8SJZ;d_b~Z:vR#YS-ZN_b(i[0~YEk^{i${!S08SKYS*`_iWD{!SN(^>YU&b_iWDZ-^WZ?YV(iWDi[({!WDYU,!V>1^{!V(8NH^{!U&8S<EHaEca_9Fb^~^YS;H__})!SK(a6^~^YS;H__})!U,.i&i&y!XJ8V'^{!X.8T?ly!X48T?ky]08U5^{!V;.i&iYPy!SP(_>@iYC>@^>@iZ(>@iZ3>@i[5}'!WN(i${!VK(i${!J8T?l>YT&>YS%_>@iYI>@^}']M(iW59MMbX&aHa^~O`})!V38V3>YT&>YS%YUG^8T&~YS+^Z5>YSAvCvR3y!UG7#YUH^{!UH3i&:`kkGiX*_i&{],)i[#>@YC^)i[#>Z,YC^~VYC^>@i[+>@YA^)i[#>@YC^)i[#>Z,YC^~VYC^>@i[+>Z,YA^~VYA^>@iZ:>@YD^)i[#>@YC^)i[#>Z,YC^~VYC^>@i[+>@YA^)i[#>@YC^)i[#>Z,YC^~VYC^>@i[+>Z,YA^~VYA^>@iZ:>Z,YD^~VYD^>@iZF{!X*I:kw(iVCl]+(_.Z+aM_H^~O^}']C(^9CTlbM`^(`~K_H_~O_})!S:2Z%a_2Z%aT`l~i$2Z%a_2Z%aT`l~iWG~YU5_cZCka_MbHa0YS:fdMbEai$H`^~O_}+!S>#`kiY58C^~i$#`kiY58C^~i$#`kiY58C^~i$#`kiY58C^~VYC^~Kw'YA^~KiVCYD^~V^{!S=#YS>a_iY.}']%#a_iVC#k_iVC~KiX*_}']$0b0I:Z$gMecw'iVCm~OMaH`^})!S&0Z$I:h.w/iVCm0Z$f~KiX*fdEaaa^}-!T4.i&^{!T+.YT4`^}'!T/.YT+b`^})!S..YT/ca_wVA})!0#b`iY58S&fEi&EbwY2awY2`8S:EfbYN`_`~Z'_M`0ci$0cYS.EYBdwXMEMQcwW-HQa0cEMQbwW-~KHQbwS1~OMa_~KwXM^0ci$0cF^0cFYT/YS.EYBgwW<wY2wY2YT4YT+`wY2wX>~OYBbQa~OMa_~KwW<^0ci%0cF^0cFYS.i$EYBdwY0^~OYBbQa~OMa_~KwY0^8S&fYBdQ`H_`HQ`~KwX>^9$cMa_~KwW-^#IZ%fw7l#d~Z(bJi&:Z$iX*YBeZ+EEfi$i$akYKYN`miY5Q`~KwXD^0:GgYS?ecGfYMdbiW6Qa_~KwVA^0YS=dZClbQbYMa_~FKwX/_8S&fEi&EbwY2awY2`8S:EfbYN`_`~Z'_M`0ci$0cYS.EYBdwXMEMQcwW-HQa0cEMQbwW-~KHQbwS1~OMa_~KwXM^0ci$0cF^0cFYT/YS.EYBgwW<wY2wY2YT4YT+`wY2wX>~OYBbQa~OMa_~KwW<^0ci%0cF^0cFYS.i$EYBdwY0^~OYBbQa~OMa_~KwY0^8S&fYBdQ`H_`HQ`~KwX>^9$cMa_~KwW-^#IZ%fw7l#d~Z(bJi&:Z$iX*YBeZ+EEfi$i$akYKYN`miY5Q`~KwXD^0:GgYS?ecGfYMdbiW6Qa_~KwVA^0YS=dZClbQbYMa_~F^~^KwW)^#cQaiY5~KwY%^H_~O_#bZCk``iXI~Z'_})!2(_#a_iY5~iWG}'!W6o!Y5n!XIm!Y.l!VCk!SA9)_>Z)^}'!T&9)uy!VDj)]A(i$9AaM_>Z)^9AaM_>Z)^>Z)vS#~FKvS#_9AaM_>Z)^9AaM_>Z)^>Z)vS#~F^~^KvE^9AaM_>Z)vS;>Z)vS#~Kt^9AaM_>Z)vS9>Z)vS#~Kv0^9AaM_>Z)vS5>Z)vS#~Ku^9AaM_>Z)^~P`H^~O^}'!TO(i$(i$8TOM^>YS%H^~O^>Z)vC~O^{!))Z?^8SAvS7vF~YUI^8S%YSL^>Z)vF~YT*^9Ai$Z*^~Z6^)YS*^~Z'^9)vL>YTOM^>YS%H^>Z)vK~O^8SAvLvK~Z(^8SAvS;vF~Ki%^8SAvS-vF~P^{!S%)^9)vE>ZAi%Z*^>Z)vE~Z6^{!TP8TP8T%~Ku^(^~YEk^YIy!T%8T%>YI(^8TP~KvR0^~YE_vC(iZ.~YS+^YS/y!S58S5E`^8S5Ea^8S5Eat~KvS;^8S5Eav0~KvS9^8S5Eau~KvS5^YI~KvS#^9._~KvE^(i&~YS+^YI{!T#.YT#^>YI(i&~FFYEvD`.YT#^>YI(i&~FF^~^KvL_.YT#^>YI(i&~F^~^KvK^YS/y!TN.YTN^Z5(i&>YI~KvL^YT%y]4(_94UUvRL_YK`v3>YI~i$(_94UUvRL_YK`v3>YI~YEvS.^~YE_vS'94UUvR,_YK`v3>YI~i$(_94UUvRL_YK`v3>YI~i$(_94UUvRL_YK`v3>YI~YEvS.^~YE_vS'94UUvR,_YK`v3>YI~YEvR<^~YE_vR594UUvR%_YK`v3>YI~i$(_94UUvRL_YK`v3>YI~i$(_94UUvRL_YK`v3>YI~YEvS.^~YE_vS'94UUvR,_YK`v3>YI~i$(_94UUvRL_YK`v3>YI~i$(_94UUvRL_YK`v3>YI~YEvS.^~YE_vS'94UUvR,_YK`v3>YI~YEvR<^~YE_vR594UUvR%_YK`v3>YI~YEvR/^~YE_vR$YS/{]59-`(^~^^YUM^Z7EYT#^>YI97YS5i&>YI~KvE^.Ei&Z5wY%>YI~KvJ^8T,Z58>Z4kk94k>YI~KvP^YS/>YI~FKvRM_8T,Z58>Z4kk94k>YI~KvP^YS/>YI~F^~^KvS?^(i%>YI~KvS;^(i$>YI~KvS-^YS/>YI~KvF^8TN>YI~KvK^(^~YEk^YT%y!S/(^!W9^YIy!U+(^!W9iZ-(^~KiZ.^!W9^{!I8U+^8U+YV9~KiZ-^(^~KiZ.^iW9y!W9iZ-!X3iZ-!S+4iZ.^{!W7iZ.!XE7%N(_>YSFYCc^>Z2YDc^YAYANi${YAYANi${]P(i$9PM`^>X$H_~O_}'!<(i&.SMa_X$H_~O_}'!W'iA!U*iD!3#l`^}'!UIYS9l!Y'92YT(aYA_^}'!;8T,YT(k^{!-8U/b`YD^})!*8U7`YD^}'!Y@iA!SLiD!T,#oYN_^{!T*YS9o!VN92YT(aYA_^}'!X097YFi&Z*^{!@97YFZ*`Z*^}'!U197a8U1EfZNbb`a_Ul`~YEa_}+!SJ8U1i&b`^})!U.(k(iZ.~O_(l8U.MbM`(l~YE`^(iZ.~YE__H`H^~O_~O^}'!T78U.Z*`Z*^}'!W%89YUB`^}'!Y?89YUE`^}'!UE8EYT7a_k}'!UB8EkYT7`^}'!U)4kYT7`^}'!V*97YT(vC^{!UO8U/b`YD^})]N8U7`YD^}'];iA]*iD]7#nYN_^{]6YS9n!T$i(!S-i(!Y4i+!X'iTH!X<iT5!VHiE!V+i4!U0(_(i$(i$8U0UUvR%`YKbuM_~YEvR/^~YE_vR$H^~O^}'!U:8U0k^(i$~Z(^{!UM(i$8>_k~^YU:^8U:M^~KvPH^(i$~Z(^Z*^{!T3(^8T3_`~YEakEb^TUYKu``vR%ZEu^}']?97YT3i&^97EYT3i&U`kvP~YEk^{!V8(^8K__~YV:`YV8ZEm`YK_^(l~Kk_}'!WOi(!WFi(!X)i(!X7i(!WK(l{!X;i(!Y&8K_ZEYV6``_YT'`YT'^(k~Kk_}'!T>8T>_YUD__(_~Kk^}'!V68T>`^8T>__~YE__YT'`YT'^}'!XP8=b^(^~KYEkbYEk`(k~Kk^UYK`a_ZE`^}'!UD8>YKZEb``^}'!T'(^8>_k~YEk^{!WL(_(^~YE`^}'!Y#(^(_~YE`^}'!V:89YUJ^{!UJ4YKZEm`m^{!X-8Ek^{!VF8E_k{!VE4k^{!+89YE`^}'!TH89YE__}'!T58E__}']:i4!U589V^{>YS#ki#!VJYAi#!U=(^!VJEiVJ^YUP^8U=YAa_(^~Z@`YA^YD_~O_}']-8U=iVJ^{!VIj2!W8iD!S*iA!UP#m_i${]'YS9m!TJ(`8TJEca`Ul^~YE_k})!T(8TJi&`^}'!S;(i$8S;Ma_(^~Z@H__H_~O_}']GiUK!UK(i$8UKMa_(^~KH__H_~O_}'!U3(i$8U3M`^(_~Z@H`^~O_}'!Gj#]#(i$9#M`^(_~KH`^~O_}'!T@(^8T@Ul`M^~YE`k}'!U/8S<aYT@`^})!U71YT@`^}'!TG(_8TGEaH_M^~O^}'].8TGi&^{!F(_.YFaM_H^~O^}'!N(k8=YNM_l~O^{!TC9(^9(^8TCMaM^~O^(i$~YLa^M^~O^}'!X:8TC_^{](4i&^{!X99IYA^{!XN9IYD^{!XC8TFYA^{!W48TFYD^{!W08TIYA^{!VL8TIYD^{!Y)8U6YA^{!Y38U6YD^{!WB8MYD^{!XB8U-YA^{!X@8U-YD^{!U@9<YA^{!WE9<YD^{!W@8U#YA^{!Y<8U#YD^{]I8BYA^{!TF8BYD^{!TI8U%YA^{!U68U%YD^{!U-8:YD^{]<8T1YA^{!U#8T1YD^{!U%8AYD^{!T18DYD^{!S?8MYA^{!M8:YA^{!B8AYA^{!:8DYA^{]FiS#!S<j2!6iA!1iD!.#k`^}'!8YS9k]@(i$(i$(i$(i$9@YDaYD_~Z@YAaYA_~Z@YCaYC_~V`(i$~KpYC_~V_(^~^K`^}'!Li4!Y-89_(^~^Ki%^{!94i$^{!S987(i$4bYC^~V^{{!X5p!X(o!Y6n!WAm!XFl!XHk!WGi$!Z6:nn:k:k:ki&vS4vS=vS9!ZD:nl:ki&vP!ZI:nki&!Z9:nn:k:k:ki&vS4vS=vS9!Z):nr:k:k:k:k:k:k:ki&vS@vS-vS0vS5vS0vS4vR$!YL:np:k:k:k:k:ki&vR$vS;vS:vS6vS/![6:nn:k:k:ki&vS4vS=vS9![,:nvR#:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS:vS;vS0vS)vCvS-vS6vCvS9vS,vS)vS4vS<vS5vCvS:vS0vS/vS;vCvS/vS;vS0vS>vCvS4vS(vS9vS.vS6vS9vS7vCvS,vS+vS6vS*vS5vS,vCvS;vS6vS5vS5vS(vR8![):nki&!ZM:nr:k:k:k:k:k:k:ki&uvS:vS,vS;vS@vS)vC!ZB:nv8:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vCvR/vS/vS;vS.vS5vS,vS3vCvS,vS+vS6vS*vCvRBvRKvRGvCvMvMvM!Z4:nki&!Z?:nki&!ZH:nki&![&:nki&![-:nki&![7:nki&!YK:nki&!Z*:nki&!Z5:nvP:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vR/vCvS4vS(vS9vS.vS6vS9vS7vCvS,vS+vS0vS:vS5vS0vCvS,vS=vS0vS;vS0vS4vS0vS9vS7vCvS+vS,vS+vS,vS,vS5vCvS+vS5vS0vS-vCvS;vS6vS5vS5vS(vR8!ZA:nki&!ZN:nvH:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS,vS*vS(vS3vS7vS,vS9vCvS5vS0vCvS5vS6vS0vS:vS:vS,vS9vS7vS?vS,vCvS,vS;vS(vS<vS3vS(vS=vS,vCvS;vS6vS5vS5vS(vR8!ZC:nvR/:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS,vS9vS<vS;vS(vS,vS-vCvS(vCvS9vS6vCvS,vS=vS0vS;vS0vS4vS0vS9vS7vCvS(vCvS;vS6vS5vCvS:vS0vCvS;vS(vS/vS;vCvS,vS9vS<vS;vS(vS,vS-vCvS(vCvS,vS=vS(vS/vCvS;vS6vS5vS5vS(vR8!Z7:nv5:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS,vS7vS@vS;vPvS6vS9vS*vS(vS4vCvS5vS>vS6vS5vS2vS5vRJ![*:nl:ki&vL!ZO:nvG:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS,vS5vS0vS3vCvS,vS4vS(vS:vCvS,vS/vS;vCvS5vS6vCvKvS#vR5vR5vCvR'vCvS;vS9vS(vS;vS:vCvS;vS6vS5vS5vS(vS*!YG:nl:ki&vP![3:nn:k:k:ki&vS)vS0vS3!Z1:no:k:k:k:ki&vS4vS*vS:vR#!Z%:nki&!ZK:nl:ki&vR0!Z>:nl:ki&vO!YD:nki&!YO:nki&![':nki&![/:nv/:k:k:k:k:k:k:k:k:k:k:k:ki&vS+vS,vS;vS*vS,vS7vS?vS,vCvS)vS0vS9!Z@:nu:k:k:k:k:k:k:k:k:k:ki&vS7vS6vCvS5vS>vS6vS5vS2vS5vS<!ZL:nv5:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS:vS5vS6vS*vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!Z':nv3:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS,vS.vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!Z2:nv3:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS,vS:vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*![4:nv4:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS3vS3vS(vS*vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!YJ:nv4:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS7vS4vS<vS1vCvS,vS+vS6vS*vS5vS,vCvS;vJvS5vS(vS*!YM:nl:ki&vS&![8:nv7:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS5vS(vS;vS:vS5vS6vS*vCvS+vS3vS0vS<vS)vCvS;vJvS5vS(vS*![.:k:k:ki&i&i%i$![$:nn:k:k:ki&vS4vS=vS9!ZG:ki&k!Z<:nvR4:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS:vS6vS/vCvS,vS3vS)vS(vS0vS-vS0vS+vS6vS4vPvS5vS6vS5vCvS(vCvS.vS5vS0vS;vS,vS.vS9vS(vS;vCvS,vS3vS0vS/vS>vCvS,vS9vS<vS;vS(vS,vS-vPvS,vS5vS0vS-vS,vS+vCvS,vS:vS<vCvS;vS6vS5vS5vS(vR8!Z;:nvF:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS+vS,vS4vS9vS6vS-vCvS3vS3vS,vS>vCvS;vS6vS5vCvS:vS0vCvS,vS=vS0vS;vS0vS4vS0vS9vS7vPvS,vS5vS0vS-vS,vS+!Z/:nvR6:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vS;vS:vS6vS/vCvS,vS3vS)vS(vS0vS-vS0vS+vS6vS4vPvS5vS6vS5vCvS(vCvS.vS5vS0vS;vS,vS.vS9vS(vS;vCvS,vS3vS0vS/vS>vCvS,vS=vS0vS;vS0vS4vS0vS9vS7vPvS,vS5vS0vS-vS,vS+vCvS,vS:vS<vCvS;vS6vS5vS5vS(vR8!Z#:nv6:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR/vS:vS,vS9vS<vS;vS(vS,vS-vPvS,vS=vS0vS3vCvMvMvM!YE:nv8:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR/vS9vS,vS+vS9vS6vCvS,vS=vS0vS;vS0vS4vS0vS9vS7vCvMvMvM!YN:nv0:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR/vS:vS;vS9vS6vS7vS?vS,vCvMvMvM!Z+:nv1:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR/vS,vS+vS6vS*vCvRBvRKvRGvCvMvMvM!Z0:nu:k:k:k:k:k:k:k:k:k:ki&vPvS,vS9vS<vS;vS(vS,vS-vFvF!Z=:ki&:k:ki&kw#!ZJ:ki&wY2![%:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&:k:ki&v7wT?:k:ki&v6x):k:ki&v5wV9:k:ki&v4xE:k:ki&v3wK:k:ki&v2w>:k:ki&v1w=:k:ki&v0wE:k:ki&v/w4:k:ki&v.wSF:k:ki&uwS#:k:ki&tx2:k:ki&swC:k:ki&rwA:k:ki&qwD:k:ki&pw?:k:ki&ow7:k:ki&nw/:k:ki&mw':k:ki&lw(:k:ki&kw#![1:k:k:k:ki&w&w%w$w#![2:nn:k:k:ki&vR#vR#vR#!YF:nl:ki&vS$!Z$:nl:ki&vC!Z&:nl:ki&vC!YH:nl:ki&vRP!Z8:nki&!Z,:nki&!ZP:nl:ki&vR$!ZE:nm:k:ki&vR$vR#![0:nki&![(:nn:k:k:ki&vR5vR5vR5!YP:nki&!YC:nv.:k:k:k:k:k:k:k:k:k:k:ki&uvR#vS@vS3vS3vS(vS<vS5vS(vS4vC!Z(:nvE:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&vCvS.vS5vS0vS5vS5vS<vS9vCvS@vS9vS;vCvS6vS;vCvS;vS5vS(vS>vCvS;vS/vS.vS0vS4vCvS<vS6vRNvCvMvMvM!Z3:nvO:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvR#vS+vS,vS0vS-vS0vS5vS0vS4vCvS;vS6vS5vCvS:vS(vS>vCvS,vS+vS6vS*vCvS+vS,vS;vS(vS9vS,vS5vS,vS.vCvS,vS/vS;vCvS6vS:vCvMvMvM![5:nvR/:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:k:ki&uvS4vS,vS;vS:vS@vS:vCvS,vS4vS,vS/vS*vRHvCvS:vS0vS/vS;vCvS/vS;vS0vS>vCvS+vS,vS;vS9vS6vS7vS7vS<vS:vCvS;vS6vS5vCvS:vS0vCvS5vS6vS0vS;vS(vS*vS0vS-vS0vS5vS0vRBvCvMvMvM!YI:nl:ki&vC![#:nl:ki&vS$![+:nl:ki&vC!Z::nl:ki&vC!ZF:nl:ki&vRP!Z-Umk!Z.Ulk!T?:lkl]):lkm!V9:lkn]E:lko!K:lkp!>:lkq!=:lkr!E:lks!4:lkt!SF:lku!S#:lkv.]2:lkv/!C:lkv0!A:lkv1!D:lkv2!?:lkv3!7:lkv4!/:lkv5!':lkv6!(:lkv7{".to_string();
        // )@@

        let mut pos = rvm_code.chars();

        let mut rib_heap: RibHeap = RibHeap::with_capacity(rvm_code.len());

        rib_heap.push_rib(FALSE);

        rib_heap.push_rib(TRUE);

        rib_heap.push_rib(NIL);

        let mut stack: usize;


        fn primitives(code:u8,
                      mut stack: &mut usize, mut rib_heap: &mut RibHeap) {
            match code {
0 =>                     {
                        rvm_prim3(
                            |z, y, x, h| -> RibField
                            {
                                RibField::Rib(
                                    h.push_rib(
                                        make_rib(x, y, z)
                                    ))
                            },
                                  &mut stack, &mut rib_heap)
                    },// )@@
1 =>                     rvm_prim1(
                              |code, _h| {
                    match code {
                        RibField::Number(value) => process::exit(value),
                        RibField::Rib(_) => process::exit(0x0100),
                    }
                },
                                &mut stack, &mut rib_heap), // )@@
2 =>                     rvm_prim1(
                               |x, _h| {
                    let n_to_push = x.get_number() as u32;
                    let c_to_write = std::char::from_u32(n_to_push)
                        .expect(format!("expected representable character, got {}",n_to_push)
                            .as_str());
                    putchar(c_to_write);
                    RibField::Number(n_to_push as i32)
                },
                                &mut stack, &mut rib_heap), // )@@
3 =>                     {
                    rvm_getchar(&mut stack, &mut rib_heap)
                }, // )@@
4 =>                     rvm_prim2(
                               |y, x, _h|
                                    { match y {
                                        RibField::Number(0) => {println!("Division by zero");process::exit(1)}
                                        _ => ()
                                    };
                                        (x/y)
                                        .expect("Division operands should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
5 =>                     rvm_prim2(
                        |y, x, _h|
                                    { (x*y)
                                        .expect("Factors should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
6 =>                     rvm_prim2(
                               |y, x, _h|
                                    { (x-y)
                                        .expect("Subtraction operands should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
7 =>                     rvm_prim2(
                               |y, x, _h|
                                    { (x+y)
                                        .expect("Addition operands should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
8 =>                     rvm_prim2(
                               |y, x,_h|
                                    { to_bool(||x<y)
                                    },
                                &mut stack, &mut rib_heap), // )@@
9 =>                     rvm_prim2(
                               |y, x,_h|
                                    { to_bool(||x==y)
                                    }, &mut stack, &mut rib_heap), // )@@
10 =>                     rvm_prim2(
                               |y,x,h|
                                    {let mut new_rib = x.get_rib(h);
                                        let x_index = x.get_rib_ref();
                                        new_rib.last=y;
                                        h.set(&x_index,new_rib);
                                        y},
                                &mut stack, &mut rib_heap), // )@@
11 =>                     rvm_prim2(
                              |y,x, h|
                                    {let mut new_rib = x.get_rib(h);
                                        let x_index = x.get_rib_ref();
                                        new_rib.middle=y;
                                        h.set(&x_index,new_rib);
                                        y},
                                &mut stack, &mut rib_heap), // )@@
12 =>                     rvm_prim2(
                               |y,x, h|
                                   {let mut new_rib = x.get_rib(h);
                                       let x_index = x.get_rib_ref();
                                       new_rib.first=y;
                                       h.set(&x_index,new_rib);
                                       y},
                               &mut stack, &mut rib_heap), // )@@
13 =>                     rvm_prim1(
                               |x,h|x.get_rib(h).last,
                               &mut stack, &mut rib_heap), // )@@
14 =>                     rvm_prim1(
                               |x, h|x.get_rib(h).middle,
                               &mut stack, &mut rib_heap), // )@@
15 =>                     rvm_prim1(
                               |x, h|x.get_rib(h).first,
                               &mut stack, &mut rib_heap), // )@@
16 =>                     rvm_prim1(
                               |x, _h|
                                   to_bool(||is_rib(&x)),
                               &mut stack, &mut rib_heap), // )@@
17 =>                     {
                        rvm_close(&mut stack, &mut rib_heap)
                }, // )@@
18 =>                     {
                        rvm_arg2(&mut stack, &mut rib_heap)}, // )@@
19 =>                     {
                        (||->(){ pop_stack(&mut stack, &mut rib_heap);})();}, // )@@
20 =>                     { rvm_prim1(
                                 |x,_h|x,&mut stack,&mut rib_heap) }, // )@@
                n => panic!("Unexpected code for primitive call {}",n),
            }
        }

        // Build the initial symbol table

        let mut symtbl = NIL_REF;
        let mut n = get_int(0,&mut pos);
        // n = rvm_code[0]>=35?(rvm_code[0] -35), 57
        while n>0 /*si rvm_code[0]=='#', la boucle est skipped*/
        {
            //Ceci alloue des structures SYMBOL vides (noms= "", value= FALSE
            n -= 1;
            let inner = rib_heap.push_rib(make_data_rib(
                RibField::Rib(NIL_REF),
                RibField::Number(0),
                STRING));
            let outer = rib_heap.push_rib(make_data_rib(
                RibField::Rib(FALSE_REF),
                RibField::Rib(inner),
                SYMBOL,
            ));
            symtbl = rib_heap.push_rib(make_data_rib(
                RibField::Rib(outer),
                RibField::Rib(symtbl),
                PAIR
            ));
        };


        let mut accum = NIL_REF;
        let mut n=0;
        loop{
            let c = get_byte(&mut pos); // 1e iteration: c = rvm_code[1]
            if c==44 /*44: ASCII pour ','*/ {
                let inner = rib_heap.push_rib(make_data_rib(
                    RibField::Rib(accum),
                    RibField::Number(n),
                    STRING
                ));
                let outer = rib_heap.push_rib(make_data_rib(
                    RibField::Rib(FALSE_REF),
                    RibField::Rib(inner),
                    SYMBOL
                ));
                symtbl = rib_heap.push_rib(make_data_rib(
                    RibField::Rib(outer),
                    RibField::Rib(symtbl),
                    PAIR
                ));
                accum=NIL_REF;
                n=0;
            } else {
                if c==59 /*ASCII pour ';'*/ {break};
                let ch = c as i32;
                push_stack(RibField::Number(ch),&mut accum,&mut rib_heap);
                n+=1;
            }
        }

        let inner = rib_heap.push_rib(make_data_rib(
            RibField::Rib(accum),
            RibField::Number(n),
            STRING
        ));
        let outer = rib_heap.push_rib(make_data_rib(
            RibField::Rib(FALSE_REF),
            RibField::Rib(inner),
            SYMBOL
        ));
        symtbl = rib_heap.push_rib(make_data_rib(
            RibField::Rib(outer),
            RibField::Rib(symtbl),
            PAIR
        ));



        // Les procédures n'ont pas encore été construites ni assignées aux entrées de la symtbl

        // Decode the RVM instructions

        let mut n_field:RibField;

        stack = rib_heap.push_rib(make_data_rib(RibField::Number(6),RibField::Number(6),6));

        loop {
            let x = get_code(&mut pos); //1e iteration: 1e char après ';' dans rvm_code
            let mut n = x; // 0<=n<=92
            let mut d ;
            let mut op = CALL;
            loop{
                //
                // x<=22:op=CALL,  ??23=<x<=55:op=SET,
                // ??56=<x<=57:op=GET, ??58=<x<=60:op=CNST,
                // ??61<=x<=74:op=IF, ??75=<x<=81:op=HALT
                // 82<=x<=92 ???
                d = match op {
                    CALL => 20,
                    SET=> 30,
                    GET=> 0,
                    CNST=> 10,
                    IF=> 11,
                    HALT=> 4,
                    _ => panic!("Unexpected op value {}",op)
                };
                if n<= d+2 {break};
                n-=d+3;
                op+=1;

            };
            if x>90 {
                n_field=pop_stack(&mut stack,&mut rib_heap);
            } else {
                if op==CALL {
                    push_stack(RibField::Number(0),&mut stack, &mut rib_heap);
                    op+=1;
                };
                if n>=d { //n= d+2, d+1, ou d
                    if n==d {
                        n_field = RibField::Number(get_int(0,&mut pos));
                    } else {
                        n_field = RibField::Rib(symbol_ref(get_int(n-d-1,&mut pos) as u32, // n-d-1= 1, 0
                                                           &symtbl,&mut rib_heap));
                    }
                } else { // n < d
                    if op<CNST { //CALL, SET, GET
                        n_field = RibField::Rib(symbol_ref(n as u32,&symtbl,&mut rib_heap));
                    } else { //CNST, IF, HALT
                        n_field = RibField::Number(n);

                    }
                };
                if op>IF {
                    let popped = pop_stack(&mut stack,&mut rib_heap);
                    let inner = rib_heap.push_rib(make_rib(
                        n_field,
                        RibField::Number(0),
                        popped
                    ));
                    n_field = RibField::Rib(rib_heap.push_rib(make_data_rib(
                        RibField::Rib(inner),
                        RibField::Rib(NIL_REF),
                        PROCEDURE
                    )));
                    if !is_rib(&rib_heap.get(&stack).middle) {break};
                    op = IF;
                };
            };

            // Il ne fait que push des n0, ils sont modifiés ici
            let stack_first= rib_heap.get(&stack).first;
            let new_rib_ref = rib_heap.push_rib(
                make_op_rib(
                    op-1 as i32,
                    n_field,
                    stack_first
                ));
            let mut top_stack = rib_heap.get(&stack);
            top_stack.first = RibField::Rib(new_rib_ref);
            rib_heap.set(&stack, top_stack); // <- Là, spécifiquement
        };


        let n_first = n_field.get_rib(&mut rib_heap).first;
        let mut pc: RibField = n_first.get_rib(&mut rib_heap).last;


        set_global(rib_heap.push_rib(make_data_rib(RibField::Number(0),
                                                   RibField::Rib(symtbl),
                                                   PROCEDURE)),
                   &mut symtbl, &mut rib_heap);
        set_global(FALSE_REF,
                   &mut symtbl, &mut rib_heap);
        set_global(TRUE_REF,
                   &mut symtbl, &mut rib_heap);
        set_global(NIL_REF,
                   &mut symtbl, &mut rib_heap);




        let halt_instr = rib_heap.push_rib(make_op_rib(HALT,
                                                       RibField::Number(0),
                                                       RibField::Number(0)));

        let primordial_cont = make_op_rib(CALL,
                                          RibField::Number(0),
                                          RibField::Rib(halt_instr));

        stack = rib_heap.push_rib(primordial_cont);

        let pc_before_gc = show(&pc,&mut rib_heap);

        if tracing {
            eprintln!("{}",pc_before_gc);
        }

        // let mut pc_trace = show(&pc, &mut rib_heap);
        // let mut stack_trace = show_stack(&stack, &mut rib_heap);

        let mut size_of_heap =rib_heap.heap.len();
        if heap_tracing {
            eprintln!("Heap size before first gc: {}", size_of_heap);
        }

        let mut pc_ref = pc.get_rib_ref();
        size_of_heap = rib_heap.garbage_collect(&mut stack, &mut pc_ref, &mut symtbl);
        pc = RibField::Rib(pc_ref);

        if heap_tracing {
            eprintln!("Heap size after first gc: {}", size_of_heap);
        }

        let pc_after_gc = show(&pc,&mut rib_heap);

        if tracing {
            eprintln!("{}",pc_after_gc);
        }

        if pc_before_gc != pc_after_gc
        {
            panic!("pc differ in functionality! {} \n {}",pc_before_gc,pc_after_gc)
        }

        let mut gc_count: u32 = 1;

        loop{
            if debug {
                start_step(&mut step_count, &mut tracing, &mut next_stamp, &start_tracing , &stack, &mut rib_heap);
            } else {
                step_count += 1;
            }
            let mut o = pc.get_rib(&mut rib_heap).middle;
            let pc_instr = pc.get_rib(&mut rib_heap).first.get_number();
            match pc_instr {
                HALT => { if tracing {eprintln!("halt");}
                    return},
                // jump/call
                CALL => {
                    if tracing { if is_rib(&pc.get_rib(&mut rib_heap).last) {
                        eprintln!("call {}",show(&o,&mut rib_heap));
                    } else {eprintln!("jump {}",show(&o,&mut rib_heap));}
                    }
                    let opnd_ref =get_opnd(&o, &stack, &mut rib_heap);
                    o = opnd_ref.first;
                    let mut c = o.get_rib(&mut rib_heap).first;


                    if is_rib(&c){ // c: code
                        let mut nparams = c.get_rib(&mut rib_heap)
                            .first.get_number();


                        nparams = nparams >>1;


                        let mut c2 = make_rib(RibField::Number(0),
                                              RibField::Rib(o.get_rib_ref()),
                                              RibField::Number(PAIR));
                        let mut s2 = rib_heap.push_rib(c2);
                        let c2_ref = s2;


                        while nparams >0{
                            let popped =pop_stack(&mut stack,&mut rib_heap);
                            push_stack(popped,&mut s2,&mut rib_heap);
                            nparams -=1;
                        };
                        if is_rib(&pc.get_rib(&mut rib_heap).last) {
                            //It's a call
                            c2.first=RibField::Rib(stack);
                            c2.last=pc.get_rib(&mut rib_heap).last;
                            rib_heap.set(&c2_ref,c2);
                        } else {
                            //It's a jump
                            let k = get_cont(&stack, &mut rib_heap);
                            c2.first=rib_heap.get(&k).first;
                            c2.last=rib_heap.get(&k).last;
                            rib_heap.set(&c2_ref,c2);
                        };

                        stack = s2;

                    } else {
                        primitives(c.get_number() as u8,
                                   &mut stack, &mut rib_heap);
                        if is_rib(&pc.get_rib(&mut rib_heap).last)
                            || pc.get_rib(&mut rib_heap).last.get_number() !=0 {
                            //It's a call
                            c = pc;
                        } else {
                            //It's a jump
                            c= RibField::Rib(get_cont(&stack, &mut rib_heap));
                            let mut top_stack = rib_heap.get(&stack);
                            top_stack.middle = c.get_rib(&mut rib_heap).first;
                            rib_heap.set(&stack,top_stack);
                        }
                    }
                    pc = c.get_rib(&mut rib_heap).last;
                },
                SET => {
                    if tracing {eprintln!("set {}",show(&o, &mut rib_heap));}
                    let set_rib_index = get_opnd_ref(&o,&stack,&mut rib_heap);
                    let mut set_rib = rib_heap.get(&set_rib_index);
                    let top =pop_stack(&mut stack,&mut rib_heap);
                    set_rib.first = top;
                    rib_heap.set(&set_rib_index,set_rib);
                    pc = pc.get_rib(&mut rib_heap).last;
                },
                GET => {
                    if tracing {eprintln!("get {}",show(&o, &mut rib_heap));}
                    let opnd_ref =get_opnd(&o,&stack,&mut rib_heap);
                    let gotten_element =
                        opnd_ref.first;
                    push_stack(gotten_element,&mut stack, &mut rib_heap);
                    pc = pc.get_rib(&mut rib_heap).last;
                },
                CNST => {
                    if tracing {eprintln!("const {}",show(&o, &mut rib_heap));}
                    push_stack(o,&mut stack,&mut rib_heap);
                    pc = pc.get_rib(&mut rib_heap).last;
                },
                IF => {

                    let bool_expr = pop_stack(&mut stack, &mut rib_heap);
                    if tracing {eprintln!("if ({})",show(&bool_expr, &mut rib_heap));
                    }
                    if is_rib(&bool_expr) && bool_expr.get_rib_ref() == FALSE_REF
                    {
                        pc = pc.get_rib(&mut rib_heap).last;
                    } else {
                        pc = pc.get_rib(&mut rib_heap).middle;
                    };
                },
                _ => panic!("Unimplemented instruction number {}",pc_instr),
            };


            if 2*size_of_heap < rib_heap.heap.len() {
                gc_count += 1;
                if heap_tracing {
                    eprintln!("Heap size before {}th gc: {}", gc_count, size_of_heap);
                }
                pc_ref = pc.get_rib_ref();
                size_of_heap = rib_heap.garbage_collect(&mut stack,&mut pc_ref, &mut symtbl);
                pc = RibField::Rib(pc_ref);
                if heap_tracing {
                    eprintln!("Heap size after {}th gc: {}", gc_count, size_of_heap);
                }
            }
        }
    }
}

use self::rvm::run_rvm;

fn main() {
    run_rvm();
}



/*
                    */