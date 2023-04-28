pub mod rvm {
    use std::fmt::{Display, Formatter};
    use std::cmp::Ordering;
    use std::cmp::Ordering::Equal;
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

            let broken_rib = RibField::Rib(self.heap.len()+1);


            //let mut old_heap_record = self.heap.clone();//DEBUG


            let mut new_heap = Vec::with_capacity(self.heap.len());

            new_heap.push(FALSE); //FALSE
            let broken_false = make_data_rib(broken_rib,RibField::Rib(0),SPECIAL);
            self.set(&0,broken_false);

            new_heap.push(TRUE); //TRUE
            let broken_true = make_data_rib(broken_rib,RibField::Rib(1),SPECIAL);
            self.set(&1,broken_true);

            new_heap.push(NIL); //NIL
            let broken_nil = make_data_rib(broken_rib,RibField::Rib(2),SPECIAL);
            self.set(&2, broken_nil);

            self.stop_and_copy(symtbl, &mut new_heap);

            self.stop_and_copy(pc, &mut new_heap);

            self.stop_and_copy(stack, &mut new_heap);

            self.heap = new_heap;
            self.heap.len()
        }

        fn stop_and_copy(&mut self, root: &mut usize, new_heap: &mut Vec<Rib>) {

            let broken_rib = RibField::Rib(self.heap.len() + 1);

            // FR: Si le Rib référencé par root est déjà dans le new_heap alors, par récursion,
            // les Ribs auxquels il est connexe sont déjà copiés et il n'est pas nécessaire de poursuivre le copiage.
            // ENG: If the Rib referenced by root is already copied then, by recursion, the Ribs to which it is
            // connected are already copied and the copying doesn't need to take place.

            if self.get(root).first == broken_rib
            {
                let new_root = self.get(root).middle;
                *root = new_root.get_rib_ref();
                return;
            }

            // FR: Initialisation des pointeurs scan et copy. ENG: Initialization of the scan and copy pointers.

            let mut scan: usize = new_heap.len();
            let mut copy: usize = new_heap.len();

            let mut old_start = self.get(root);
            let mut copied_rib = old_start.clone();

            // FR: Le marqueur va être écrit dans le champ first, l'adresse de sa copie dans le champ middle
            // ENG: The mark will be written in the first field, the address of its copy in the middle field
            old_start.first = broken_rib;
            old_start.middle = RibField::Rib(copy);
            self.set(root,old_start);

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
                    if past_rib.first == broken_rib
                    {
                        copied_rib.first = past_rib.middle;
                    } else
                    {
                        let past_rib_ref = copied_rib.first.get_rib_ref();

                        copied_rib.first = RibField::Rib(copy);

                        new_heap.push(past_rib.clone());

                        past_rib.first = broken_rib;
                        past_rib.middle = RibField::Rib(copy);

                        copy += 1;

                        self.set(&past_rib_ref,past_rib);
                    }
                }

                if is_rib(&copied_rib.middle)
                {
                    is_changed = true;
                    let mut past_rib = copied_rib.middle.get_rib(self);
                    if past_rib.first == broken_rib
                    {
                        copied_rib.middle = past_rib.middle;
                    } else
                    {
                        let past_rib_ref = copied_rib.middle.get_rib_ref();
                        copied_rib.middle = RibField::Rib(copy);

                        new_heap.push(past_rib.clone());

                        past_rib.first = broken_rib;
                        past_rib.middle = RibField::Rib(copy);

                        copy += 1;

                        self.set(&past_rib_ref,past_rib);
                    }
                }

                if is_rib(&copied_rib.last)
                {
                    is_changed = true;
                    let mut past_rib = copied_rib.last.get_rib(self);
                    if past_rib.first == broken_rib
                    {
                        copied_rib.last = past_rib.middle;
                    } else
                    {
                        let past_rib_ref = copied_rib.last.get_rib_ref();
                        copied_rib.last = RibField::Rib(copy);

                        new_heap.push(past_rib.clone());

                        past_rib.first = broken_rib;
                        past_rib.middle = RibField::Rib(copy);

                        copy += 1;
                        self.set(&past_rib_ref,past_rib);
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





    // @@(feature debug
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
                            let sh =show(&field_o, holder);
                            result.push_str(sh.as_str());

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
                                let sh =show(&rib_o.first, holder);
                                result.push_str(sh.as_str());
                                result.push(',');
                                let sh = show(&rib_o.middle, holder);
                                result.push_str(sh.as_str());
                                result.push(',');
                                let sh = show(&rib_o.last, holder);
                                result.push_str(sh.as_str());
                                result.push(']');
                            }
                        }
                    },
                    _ => {
                        result.push('[');
                        let sh =show(&rib_o.first, holder);
                        result.push_str(sh.as_str());
                        result.push(',');
                        let sh =show(&rib_o.middle, holder);
                        result.push_str(sh.as_str());
                        result.push(',');
                        let sh =show(&rib_o.last, holder);
                        result.push_str(sh.as_str());
                        result.push(']');
                    }
                },
                RibField::Rib(_) => {
                    result.push('[');
                    let sh =show(&rib_o.first, holder);
                    result.push_str(sh.as_str());
                    result.push(',');
                    let sh = show(&rib_o.middle, holder);
                    result.push_str(sh.as_str());
                    result.push(',');
                    let sh =show(&rib_o.last, holder);
                    result.push_str(sh.as_str());
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
            let sh = show(&rib_s.first,holder);
            result.push_str(sh.as_str());
            s = rib_s.middle;
            if !is_rib(&s) {break;}
            rib_s = s.get_rib(holder);
        }
        result.push(')');
        eprintln!("{}",result);

    }
    // )@@


    fn is_rib(obj: &RibField) -> bool {
        match obj {
            RibField::Rib(_) => true,
            _ => false,
        }
    }




    // @@(feature bool2scm
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
        // @@(feature arity-check
        expected_nargs: u32,
        // )@@
        mut f: F,stack: &mut usize, holder: &mut RibHeap)
        where F: FnMut(RibField,&mut RibHeap) -> RibField{
        // @@(feature arity-check
        if expected_nargs != 1
        {
            incoherent_nargs_stop(expected_nargs,1,false)
        }
        // )@@
        let x =pop_stack(stack, holder);
        let r = f(x, holder);
        push_stack(
            r,
            stack, holder
        );
    }

    fn rvm_prim2<G>(
        // @@(feature arity-check
        expected_nargs: u32,
                     // )@@
                     mut f: G,stack: &mut usize, holder: &mut RibHeap)
        where G: FnMut(RibField,RibField, &mut RibHeap) -> RibField{
        // @@(feature arity-check
        if expected_nargs != 2
        {
            incoherent_nargs_stop(expected_nargs,2,false)
        }
        // )@@
        let x = pop_stack(stack, holder);
        let y = pop_stack(stack, holder);
        let r =f(x, y, holder);
        push_stack(r,
                   stack, holder
        );
    }

    fn rvm_prim3<H>(
        // @@(feature arity-check
                    expected_nargs: u32,
                    // )@@
                    mut f: H,stack: &mut usize, holder: &mut RibHeap)
        where H: FnMut(RibField, RibField, RibField, &mut RibHeap) -> RibField{
        // @@(feature arity-check
        if expected_nargs != 3
        {
            incoherent_nargs_stop(expected_nargs,3,false)
        }
        // )@@
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

    // @@(feature arity-check
    fn incoherent_nargs_stop(nargs:u32,expected_nargs:u32, variadic:bool) {
        //TODO: Reformulate error message
        if variadic {
            eprintln!("Insufficient number of arguments. This function requires a minimum of {} arguments, got {}", expected_nargs, nargs);
            println!("Insufficient number of arguments. This function requires a minimum of {} arguments, got {}", expected_nargs, nargs);
        }
        else {
            eprintln!("Incorrect number of arguments. This function takes {} arguments, got {}", expected_nargs, nargs);
            println!("Incorrect number of arguments. This function takes {} arguments, got {}", expected_nargs, nargs);
        }
        process::exit(0x0100)
    }
    // )@@

    pub fn run_rvm() {

        let mut step_count:u32 =0;
        let start_tracing:u32 = 0;
        let mut next_stamp:u32 =0;
        let mut tracing = true;
        let heap_tracing = false;
        let mut debug = true;


        // @@(feature (not debug)
        tracing = !tracing; // Pour enlever les warnings de rustc
        debug = !debug;
        // )@@

        // @@(replace ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{" (encode 92)
        let rvm_code: String = ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{".to_string();
        // )@@

        let mut pos = rvm_code.chars();

        let mut rib_heap: RibHeap = RibHeap::with_capacity(rvm_code.len());

        rib_heap.push_rib(FALSE);

        rib_heap.push_rib(TRUE);

        rib_heap.push_rib(NIL);

        let mut stack: usize;


        fn primitives(code:u8,
                      // @@(feature arity-check
                      expected_nargs: u32,
                      // )@@
                      mut stack: &mut usize, mut rib_heap: &mut RibHeap) {
            match code {
                // @@(primitives (gen index " => " body)
                0 => // @@(primitive (rib a b c)
                    {
                        rvm_prim3(
                            // @@(feature arity-check
                            expected_nargs,
                            // )@@
                            |z, y, x, h| -> RibField
                            {
                                RibField::Rib(
                                    h.push_rib(
                                        make_rib(x, y, z)
                                    ))
                            },
                                  &mut stack, &mut rib_heap)
                    },// )@@
                1 => // @@(primitive (id x)
                    { rvm_prim1(
                        // @@(feature arity-check
                        expected_nargs,
                                 // )@@
                                 |x,_h|x,&mut stack,&mut rib_heap) }, // )@@
                2 => // @@(primitive (arg1 x y)
                    {
                        // @@(feature arity-check
                        if expected_nargs != 2 {incoherent_nargs_stop(expected_nargs,2,false)};
                        // )@@
                        (||->(){ pop_stack(&mut stack, &mut rib_heap);})();}, // )@@
                3 => // @@(primitive (arg2 x y)
                    {
                        // @@(feature arity-check
                        if expected_nargs != 2 {incoherent_nargs_stop(expected_nargs,2,false)};
                        // )@@
                        rvm_arg2(&mut stack, &mut rib_heap)}, // )@@
                4 => // @@(primitive (close rib)
                    {
                        // @@(feature arity-check
                    if expected_nargs != 1 {incoherent_nargs_stop(expected_nargs, 1, false) };
                        // )@@
                        rvm_close(&mut stack, &mut rib_heap)
                }, // )@@
                5 => // @@(primitive (rib? rib) (use bool2scm)
                    rvm_prim1(
                        // @@(feature arity-check
                        expected_nargs,
                               // )@@
                               |x, _h|
                                   to_bool(||is_rib(&x)),
                               &mut stack, &mut rib_heap), // )@@
                6 => // @@(primitive (field0 rib)
                    rvm_prim1(
                        // @@(feature arity-check
                        expected_nargs,
                               // )@@
                               |x, h|x.get_rib(h).first,
                               &mut stack, &mut rib_heap), // )@@
                7 => // @@(primitive (field1 rib)
                    rvm_prim1(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |x, h|x.get_rib(h).middle,
                               &mut stack, &mut rib_heap), // )@@
                8 => // @@(primitive (field2 rib)
                    rvm_prim1(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |x,h|x.get_rib(h).last,
                               &mut stack, &mut rib_heap), // )@@
                9 =>// @@(primitive (field0-set! rib)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |y,x, h|
                                   {let mut new_rib = x.get_rib(h);
                                       let x_index = x.get_rib_ref();
                                       new_rib.first=y;
                                       h.set(&x_index,new_rib);
                                       y},
                               &mut stack, &mut rib_heap), // )@@
                10 => // @@(primitive (field1-set! rib)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                              |y,x, h|
                                    {let mut new_rib = x.get_rib(h);
                                        let x_index = x.get_rib_ref();
                                        new_rib.middle=y;
                                        h.set(&x_index,new_rib);
                                        y},
                                &mut stack, &mut rib_heap), // )@@
                11 => // @@(primitive (field2-set! rib)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |y,x,h|
                                    {let mut new_rib = x.get_rib(h);
                                        let x_index = x.get_rib_ref();
                                        new_rib.last=y;
                                        h.set(&x_index,new_rib);
                                        y},
                                &mut stack, &mut rib_heap), // )@@
                12 => // @@(primitive (eqv? rib1 rib2) (use bool2scm)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |y, x,_h|
                                    { to_bool(||x==y)
                                    }, &mut stack, &mut rib_heap), // )@@
                13 => // @@(primitive (< x y) (use bool2scm)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |y, x,_h|
                                    { to_bool(||x<y)
                                    },
                                &mut stack, &mut rib_heap), // )@@
                14 => // @@(primitive (+ x y)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |y, x, _h|
                                    { (x+y)
                                        .expect("Addition operands should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
                15 => // @@(primitive (- x y)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |y, x, _h|
                                    { (x-y)
                                        .expect("Subtraction operands should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
                16 => // @@(primitive (* x y)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                        |y, x, _h|
                                    { (x*y)
                                        .expect("Factors should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
                17 => // @@(primitive (quotient x y)
                    rvm_prim2(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |y, x, _h|
                                    { match y {
                                        RibField::Number(0) => {println!("Division by zero");process::exit(1)}
                                        _ => ()
                                    };
                                        (x/y)
                                        .expect("Division operands should both be numbers")
                                    },
                                &mut stack, &mut rib_heap), // )@@
                18 => // @@(primitive (getchar)
                    {
                    rvm_getchar(&mut stack, &mut rib_heap)
                }, // )@@
                19 => // @@(primitive (putchar c)
                    rvm_prim1(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                               |x, _h| {
                    let n_to_push = x.get_number() as u32;
                    let c_to_write = std::char::from_u32(n_to_push)
                        .expect(format!("expected representable character, got {}",n_to_push)
                            .as_str());
                    putchar(c_to_write);
                    RibField::Number(n_to_push as i32)
                },
                                &mut stack, &mut rib_heap), // )@@
                20 =>  // @@(primitive (exit n)
                    rvm_prim1(
                        // @@(feature arity-check
                        expected_nargs,
                        // )@@
                              |code, _h| {
                    match code {
                        RibField::Number(value) => process::exit(value),
                        RibField::Rib(_) => process::exit(0x0100),
                    }
                },
                                &mut stack, &mut rib_heap), // )@@
                // @@(feature arity-check
                21 => {
                    let mut n_elems = expected_nargs;
                    let mut elems = Vec::new();
                    while n_elems > 0 {
                        if !is_rib(&rib_heap.get(&stack).last) &&
                            rib_heap.get(&stack).last.get_number() == 0
                        {
                            elems.push(pop_stack(&mut stack, &mut rib_heap));
                            n_elems -= 1;
                        }
                        else
                        {
                            eprintln!("Expected {} elements in the list but stack had {} elements",
                            expected_nargs, elems.len());
                            println!("Expected {} elements in the list but stack had {} elements",
                            expected_nargs, elems.len());
                            process::exit(0x0100)
                        }
                    }

                    let mut new_list = NIL_REF;
                    for e in elems {
                        push_stack(e, &mut new_list, &mut rib_heap);
                    };
                    let new_vector = rib_heap.push_rib(make_data_rib(
                        RibField::Rib(new_list),
                        RibField::Number(expected_nargs as i32),
                        4)
                    );
                    push_stack(RibField::Rib(new_vector),&mut stack, &mut rib_heap);
                }, // )@@
                // )@@
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

        // Il faut assigner le symbole "list" à la primitive list, si elle est présente



        let halt_instr = rib_heap.push_rib(make_op_rib(HALT,
                                                       RibField::Number(0),
                                                       RibField::Number(0)));

        let primordial_cont = make_op_rib(CALL,
                                          RibField::Number(0),
                                          RibField::Rib(halt_instr));

        stack = rib_heap.push_rib(primordial_cont);


        // @@(feature debug
        if tracing {
            eprintln!("{}",show(&pc,&mut rib_heap));
        }
        // )@@

        // let mut pc_trace = show(&pc, &mut rib_heap);
        // let mut stack_trace = show_stack(&stack, &mut rib_heap);
        // let mut getchar_calls = 0;

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

        let mut gc_count: u32 = 1;

        loop{
            // @@(feature debug
            if debug {
                start_step(&mut step_count, &mut tracing, &mut next_stamp, &start_tracing , &stack, &mut rib_heap);
            }
            // )@@
            let mut o = pc.get_rib(&mut rib_heap).middle;
            let pc_instr = pc.get_rib(&mut rib_heap).first.get_number();
            match pc_instr {
                HALT => {
                    if tracing {eprintln!("halt");} // @@(feature debug)@@
                    return},
                // jump/call
                CALL => {
                    // @@(feature debug
                    if tracing {
                        if is_rib(&pc.get_rib(&mut rib_heap).last) {
                            eprintln!("call {}",show(&o,&mut rib_heap));
                        } else {
                            eprintln!("jump {}",show(&o,&mut rib_heap));
                        }
                    }
                    // )@@
                    // @@(feature arity-check
                    let pre_o =o;
                    let mut nargs = -1;
                    if is_rib(&pre_o) {
                        nargs = pop_stack(&mut stack, &mut rib_heap).get_number();
                    }
                        //)@@
                    let opnd_ref =get_opnd(&o, &stack, &mut rib_heap);
                    o = opnd_ref.first;
                    let mut c = o.get_rib(&mut rib_heap).first;

                    // @@(feature arity-check
                    if !is_rib(&pre_o) {
                        nargs = pop_stack(&mut stack, &mut rib_heap).get_number();
                    }
                    //)@@

                    if is_rib(&c){ // c: code
                        let mut nparams = c.get_rib(&mut rib_heap)
                            .first.get_number();

                        // @@(feature arity-check
                        let variadic = nparams % 2==1;
                        // )@@

                        nparams = nparams >>1;

                        // @@(feature arity-check
                        if !variadic && nparams != nargs || variadic && nparams > nargs
                        {
                            incoherent_nargs_stop(nargs as u32, nparams as u32, variadic);
                        }
                        // )@@

                        let mut c2 = make_rib(RibField::Number(0),
                                              RibField::Rib(o.get_rib_ref()),
                                              RibField::Number(PAIR));
                        let mut s2 = rib_heap.push_rib(c2);
                        let c2_ref = s2;

                        // @@(feature rest-param (use arity-check)
                        nargs -= nparams;
                        if variadic
                        {
                            let mut rest = NIL_REF;
                            let mut i =0;
                            while i < nargs {
                                let arg =pop_stack(&mut stack, &mut rib_heap);
                                push_stack(arg, &mut rest, &mut rib_heap);
                                i -= 1;
                            }
                            push_stack(RibField::Rib(rest), &mut s2, &mut rib_heap);
                        }
                        // )@@

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
                                   // @@(feature arity-check
                                   nargs as u32,
                                   // )@@
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
                    if tracing {eprintln!("set {}",show(&o, &mut rib_heap));}  // @@(feature debug)@@
                    let set_rib_index = get_opnd_ref(&o,&stack,&mut rib_heap);
                    let mut set_rib = rib_heap.get(&set_rib_index);
                    let top =pop_stack(&mut stack,&mut rib_heap);
                    set_rib.first = top;
                    rib_heap.set(&set_rib_index,set_rib);
                    pc = pc.get_rib(&mut rib_heap).last;
                },
                GET => {
                    if tracing {eprintln!("get {}",show(&o, &mut rib_heap));} // @@(feature debug)@@
                    let opnd_ref =get_opnd(&o,&stack,&mut rib_heap);
                    let gotten_element =
                        opnd_ref.first;
                    push_stack(gotten_element,&mut stack, &mut rib_heap);
                    pc = pc.get_rib(&mut rib_heap).last;
                },
                CNST => {
                    if tracing {eprintln!("const {}",show(&o, &mut rib_heap));} //@@(feature debug)@@
                    push_stack(o,&mut stack,&mut rib_heap);
                    pc = pc.get_rib(&mut rib_heap).last;
                },
                IF => {

                    let bool_expr = pop_stack(&mut stack, &mut rib_heap);
                    if tracing {eprintln!("if"); }                                  //@@(feature debug)@@
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

                // @@(feature debug
                if heap_tracing {
                    size_of_heap = rib_heap.heap.len();
                    eprintln!("Heap size before {}th gc: {}", gc_count, size_of_heap);
                }
                // )@@
                pc_ref = pc.get_rib_ref();
                size_of_heap = rib_heap.garbage_collect(&mut stack,&mut pc_ref, &mut symtbl);
                pc = RibField::Rib(pc_ref);
                // @@(feature debug
                if heap_tracing {
                    eprintln!("Heap size after {}th gc: {}", gc_count, size_of_heap);
                }
                // )@@
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