use std::io;
use std::io::Read;
use std::process;
use std::convert::TryInto;

const RIB_NB_FIELDS: usize = 3;
const MAX_NB_OBJS: u32 = 30_000;
const SPACE_SZ: u32 = MAX_NB_OBJS * RIB_NB_FIELDS as u32;
const HEAP_SIZE: usize = SPACE_SZ as usize * 2;
const HEAP_BOT: usize = 0;
const HEAP_MID: usize = HEAP_SIZE / 2;
const HEAP_TOP: usize = HEAP_SIZE - 1; // Last valid index

const NUM_0: Obj = tag_num(0);
const PAIR_TAG: Obj = tag_num(0);
const CLOSURE_TAG: Obj = tag_num(1);
const SYMBOL_TAG: Obj = tag_num(2);
const STRING_TAG: Obj = tag_num(3);
const SINGLETON_TAG: Obj = tag_num(5);

const INSTR_AP: i64 = 0;
const INSTR_SET: i64 = 1;
const INSTR_GET: i64 = 2;
const INSTR_CONST: i64 = 3;
const INSTR_IF: i64 = 4;
const INSTR_HALT: i64 = 5;

const EXIT_ILLEGAL_INSTR: i32 = 6;
const EXIT_NO_MEMORY: i32 = 7;

fn exit_vm(code: i32)
{
    process::exit(code);
}

const fn tag_num(num: i64) -> Obj
{
    Obj::Num(num as u64)
}

const fn tag_rib(num: u64) -> Obj
{
    Obj::Rib(num)
}

#[derive(Copy, Clone)]
enum Obj
{
    Num(u64),
    Rib(u64)
}

const fn unwrap_obj(obj: &Obj) -> u64
{
    match obj
    {
        Obj::Num(num) =>
        {
            *num
        }

        Obj::Rib(num) =>
        {
            *num
        }
    }
}

const fn is_rib(obj: &Obj) -> bool
{
    match obj
    {
        Obj::Num(_) =>
        {
            false
        }

        Obj::Rib(_) =>
        {
            true
        }
    }
}

struct Rib<'a>
{
    fields: &'a [Obj],
}

struct Environment<'a>
{
    // Roots
    stack: Obj,
    pc: Obj,
    FALSE: Obj,
    
    pos: usize,
    input: &'a [u8],

    heap: &'a mut [Obj; HEAP_SIZE],
    symbol_table: Obj,

    alloc: usize,
    alloc_limit: usize,
    scan: *const Obj,
}

fn advance_pc(env: &mut Environment)
{
    env.pc = get_tag(env, env.pc);
}

fn list_tail(env: &mut Environment, lst: usize, i: Obj) -> usize
{
    if unwrap_obj(&i) == 0
    {
	lst
    }
    else
    {
	let rib = get_rib_at(env, Obj::Num(lst as u64));
	let cdr = unwrap_obj(&rib.fields[1]);
	list_tail(env, cdr as usize, Obj::Num(unwrap_obj(&i) - 1))
    }
}

fn symbol_ref(env: &mut Environment, n: Obj) -> usize
{
    let sym_table_idx = unwrap_obj(&env.symbol_table) as usize;
    list_tail(env, sym_table_idx, n)
}

fn get_opnd(env: &mut Environment, obj: Obj) -> Obj
{
    let rib_obj = if !is_rib(&obj)
    {
	Obj::Rib(list_tail(env, unwrap_obj(&env.stack) as usize, obj) as u64)
    }
    else
    {
	obj
    };

    let rib = get_rib_at(env, rib_obj);
    rib.fields[0]
}

fn proc (env: &mut Environment) -> Obj
{
    let cdr = get_cdr(env, env.pc);
    get_opnd(env, cdr)
}

fn code (env: &mut Environment) -> Obj
{
    let proc_obj = proc(env);
    get_car(env, proc_obj)
}

fn get_cont(env: &mut Environment) -> Obj
{
    let mut s = env.stack;

    while unwrap_obj(&get_tag(env, s)) != 0
    {
	s = get_cdr(env, s);
    }

    s
}

fn get_int(env: &mut Environment, n:i64) -> i64
{
    let x = get_code(env);
    let n = n * 46;
    if x < 46 { n + x } else { get_int(env, n + x - 46) }
}

fn get_code(env: &mut Environment) -> i64
{
    let x: i64 = i64::from(get_byte(env)) - 35;
    if x < 0 { 57 } else { x }
}

fn get_byte(env: &mut Environment) -> u8
{
    let byte = env.input[env.pos];
    env.pos += 1;
    byte
}

fn get_car_index(index: Obj) -> usize
{
    unwrap_obj(&index).try_into().unwrap() // TODO: Check this conversion
}

fn get_cdr_index(index: Obj) -> usize
{
    (unwrap_obj(&index) + 1).try_into().unwrap() // TODO: Check this conversion
}

fn get_tag_index(index: Obj) -> usize
{
    (unwrap_obj(&index) + 2).try_into().unwrap() // TODO: Check this conversion
}

fn get_tos_index(env: &mut Environment) -> usize
{
    get_car_index(env.stack)
}

fn get_car(env: &mut Environment, index: Obj) -> Obj
{
    let rib = get_rib_at(env, index);
    rib.fields[0]
}

fn get_cdr(env: &mut Environment, index: Obj) -> Obj
{
    let rib = get_rib_at(env, index);
    rib.fields[1]
}

fn get_tag(env: &mut Environment, index: Obj) -> Obj
{
    let rib = get_rib_at(env, index);
    rib.fields[2]
}

fn get_true(env: &mut Environment) -> Obj
{
    get_car(env, env.FALSE)
}

fn get_nil(env: &mut Environment) -> Obj
{
    get_cdr(env, env.FALSE)
}

fn get_boolean(env: &mut Environment, cond: bool) -> Obj
{
    if cond { get_true(env) } else { env.FALSE }
}

fn get_tos(env: &mut Environment) -> Obj
{
    get_car(env, env.stack)
}

fn get_rib_at<'a>(env: &'a mut Environment, index: Obj) -> Rib<'a>
{
    let start_index = unwrap_obj(&index) as usize;
    let end_index = start_index + RIB_NB_FIELDS;

    Rib
    {
        fields: &env.heap[start_index..end_index],
    }
}

fn main()
{
    init();
}

fn init()
{
    let input = String::from(");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y");
    let mut heap: [Obj; HEAP_SIZE] = [Obj::Num(0); HEAP_SIZE];
    let scan = &heap[0] as *const Obj; 

    let mut env = Environment
    {
        stack: NUM_0,
        pc: NUM_0,
        FALSE: NUM_0,
        
        pos: 0,
        input: &input.as_bytes(),
        heap: &mut heap,
        symbol_table: NUM_0,

        alloc: HEAP_BOT,
        alloc_limit: HEAP_MID,
        scan: scan,
    };

    let init_0 = alloc_rib(&mut env, NUM_0, NUM_0, SINGLETON_TAG);
    env.FALSE = alloc_rib(&mut env, init_0, init_0, SINGLETON_TAG);

    build_sym_table(&mut env);
    decode(&mut env);

    let sym_table = env.symbol_table;
    let rib = alloc_rib(&mut env, NUM_0, sym_table, CLOSURE_TAG);
    let fals = env.FALSE;
    let tru = get_true(&mut env);
    let nil = get_nil(&mut env);
    
    set_global(&mut env, rib);
    set_global(&mut env, fals);
    set_global(&mut env, tru);
    set_global(&mut env, nil);

    setup_stack(&mut env);
    run(&mut env);
}

fn build_sym_table(env: &mut Environment)
{
    let mut n = get_int(env, 0);

    while n > 0
    {
        n -= 1;
        let nil = get_nil(env);
        env.symbol_table = create_sym(env, nil);
    }

    let mut accum = get_nil(env);

    loop
    {
        let c = get_byte(env);

        if c == 44
        {
            env.symbol_table = create_sym(env, accum);
            accum = get_nil(env);
            continue;
        }

        if c == 59
        {
            break;
        }

        accum = alloc_rib(env, tag_num(c as i64), accum, PAIR_TAG); 
    }

    env.symbol_table = create_sym(env, accum);
}

fn set_global(env: &mut Environment, c: Obj)
{
    let car_index = Obj::Num(get_car_index(env.symbol_table) as u64);
    env.heap[get_car_index(car_index)] = c;
    env.symbol_table = get_cdr(env, env.symbol_table);
}

fn decode(env: &mut Environment)
{
    let weights = [20, 30, 0, 10, 11, 4];

    let mut n = Obj::Num(0);
    let mut d = 0;
    let mut op: i64 = -1;

    loop
    {
	let x = get_code(env);
	n = tag_num(x);
	op = -1;

	while unwrap_obj(&n) > { op +=1; d = weights[op as usize]; d + 2 }
	{
	    n = Obj::Num(unwrap_obj(&n) - (d + 3));
	}
	
	if x > 90
	{
	    op = INSTR_IF;
	    n = pop(env);
	}
	else
	{
	    if op == 0
	    {
		push2(env, NUM_0, NUM_0);
	    }

	    if unwrap_obj(&n) >= d
	    {
		n = if unwrap_obj(&n) == d
		{
		    tag_num(get_int(env, 0))
		}
		else
		{
		    let num = (unwrap_obj(&n) - d - 1) as i64;
		    let int = get_int(env, num);
		    Obj::Rib(symbol_ref(env, tag_num(int)) as u64)
		}
	    }
	    else
	    {
		n = if op < 3
		{
		    Obj::Rib(symbol_ref(env, n) as u64)
		}
		else
		{
		    n
		}
	    }

	    if op > 4
	    {
		let obj = pop(env);
		let rib2 = alloc_rib2(env, n, NUM_0, obj);
		let nil = get_nil(env);
		n = alloc_rib(env, rib2, nil, CLOSURE_TAG);
		
		if unwrap_obj(&env.stack) == unwrap_obj(&NUM_0)
		{
		    break;
		}
	    }
	    else if op > 0
	    {
		op -= 1;
	    }
	    else
	    {
		op = 0;
	    }
	}

	let c = alloc_rib(env, Obj::Num(op as u64), n, Obj::Num(0));
	env.heap[get_cdr_index(c)] = env.heap[get_tos_index(env)];
	env.heap[get_tos_index(env)] = c;
    }

    let car = get_car(env, n);
    let tag = get_tag(env, car);
    env.pc = get_tag(env, tag);
}

fn setup_stack(env: &mut Environment)
{
    push2(env, NUM_0, PAIR_TAG);
    push2(env, NUM_0, PAIR_TAG);

    let first = get_cdr(env, env.stack);
    env.heap[get_cdr_index(env.stack)] = NUM_0;
    env.heap[get_tag_index(env.stack)] = first;

    env.heap[get_car_index(first)] = tag_num(INSTR_HALT);
    env.heap[get_cdr_index(first)] = NUM_0;
    env.heap[get_tag_index(first)] = PAIR_TAG;
}

fn run(env: &mut Environment)
{
    loop
    {
	let instr = get_car(env, env.pc);
    print!("{}\n", unwrap_obj(&instr) as i64);
    advance_pc(env);
    let instr = get_car(env, env.pc);
    print!("{}\n", unwrap_obj(&instr) as i64);
    
    match unwrap_obj(&instr) as i64
	{
	    INSTR_HALT =>
	    {               
        exit_vm(0);
	    }

	    INSTR_AP =>
	    {
		let pc_tag = unwrap_obj(&get_tag(env, env.pc));
		let jump = pc_tag == unwrap_obj(&NUM_0);

		if !is_rib(&code(env))
		{
		    let code_obj = code(env);
		    prim(env, unwrap_obj(&code_obj) as i64);
		    if jump
		    {
			env.pc = get_cont(env);
			env.heap[get_cdr_index(env.stack)] = get_car(env, env.pc);
		    }

		    advance_pc(env);
		}
		else
		{
		    let code_obj = code(env);
		    let argc = get_car(env, code_obj);
		    env.heap[get_car_index(env.pc)] = code(env);

		    let proc_obj = proc(env);
		    let mut s2 = alloc_rib(env, NUM_0, proc_obj, PAIR_TAG);

		    for _ in 0..unwrap_obj(&argc)
		    {
			let pop_obj = pop(env);
			s2 = alloc_rib(env, pop_obj, s2, PAIR_TAG);
		    }

		    let c2 = Obj::Num(list_tail(env, unwrap_obj(&s2) as usize, argc) as u64);

		    if jump
		    {
			let k = get_cont(env);
			env.heap[get_car_index(c2)] = get_car(env, k);
			env.heap[get_tag_index(c2)] = get_tag(env, k);
		    }
		    else
		    {
			env.heap[get_car_index(c2)] = env.stack;
			env.heap[get_tag_index(c2)] = get_tag(env, env.pc);
		    }

		    env.stack = s2;

		    let new_pc = get_car(env, env.pc);
		    env.heap[get_car_index(env.pc)] = instr;
		    env.pc = get_tag(env, new_pc);
		}
	    }

	    INSTR_SET =>
	    {
		let x = pop(env);

		let rib = if !is_rib(&get_cdr(env, env.pc))
		{
		    let cdr_obj = get_cdr(env, env.pc);
		    let stack = unwrap_obj(&env.stack) as usize;
		    Obj::Rib(list_tail(env, stack, cdr_obj) as u64)
		}
		else
		{
		    get_cdr(env, env.pc)
		};

		env.heap[get_car_index(rib)] = x;
		
		advance_pc(env);
	    }

	    INSTR_GET =>
	    {
		let proc_obj = proc(env);
		push2(env, proc_obj, PAIR_TAG);
		advance_pc(env);
	    }

	    INSTR_CONST =>
	    {
		let cdr_obj = get_cdr(env, env.pc);
		push2(env, cdr_obj, PAIR_TAG);
		advance_pc(env);
	    }

	    INSTR_IF =>
	    {
		let p = unwrap_obj(&pop(env));
		let false_unwraped = unwrap_obj(&env.FALSE);
		if p != false_unwraped
		{
		    env.pc = get_cdr(env, env.pc);
		}
		else
		{
		    env.pc = get_tag(env, env.pc);
		}
	    }

	    _ =>
	    {
		exit_vm(EXIT_ILLEGAL_INSTR);
	    }
	}
    }
}

fn create_sym(env: &mut Environment, name: Obj) -> Obj
{
    let list_length = list_lenght(env, name);
    let list: Obj = alloc_rib(env, name, list_length, STRING_TAG);
    let sym = alloc_rib(env, env.FALSE, list, SYMBOL_TAG);
    alloc_rib(env, sym, env.symbol_table, PAIR_TAG)
}

fn alloc_rib(env: &mut Environment, car: Obj, cdr: Obj, tag: Obj) -> Obj
{
    push2(env, car, cdr);
    let old_stack = get_cdr(env, env.stack);
    let allocated = env.stack;

    env.heap[get_cdr_index(allocated)] = env.heap[get_tag_index(allocated)];
    env.heap[get_tag_index(allocated)] = tag;

    env.stack = old_stack;

    Obj::Rib(unwrap_obj(&allocated))
}

fn alloc_rib2(env: &mut Environment, car: Obj, cdr: Obj, tag: Obj) -> Obj
{
    push2(env, car, tag);
    let old_stack = get_cdr(env, env.stack);
    let allocated = env.stack;

    env.heap[get_cdr_index(allocated)] = cdr;

    env.stack = old_stack;

    Obj::Rib(unwrap_obj(&allocated))
}

fn pop(env: &mut Environment) -> Obj
{
    let x = get_car(env, env.stack);
    env.stack = get_cdr(env, env.stack);
    x
}

fn push2(env: &mut Environment, car: Obj, tag: Obj)
{
    env.heap[env.alloc] = car;
    env.alloc += 1;

    env.heap[env.alloc] = env.stack;
    env.alloc += 1;

    env.heap[env.alloc] = tag;
    env.alloc += 1;

    env.stack = tag_rib((env.alloc - RIB_NB_FIELDS) as u64);

    if env.alloc == env.alloc_limit
    {
        // TODO: GC
    }
}

fn list_lenght(env: &mut Environment, list: Obj) -> Obj
{
    let mut len: i64 = 0;
    let mut list = list;

    while is_rib(&list) && unwrap_obj(&get_tag(env, list)) == 0
    {
        len += 1;
        list = get_cdr(env, list)
    }

    tag_num(len)
}

// TODO: Finish GC
fn gc(env: &mut Environment)
{
    let to_space = if env.alloc_limit == HEAP_MID { HEAP_MID } else { HEAP_BOT };
    env.alloc_limit = to_space + SPACE_SZ as usize;

    env.alloc = to_space;
}

fn prim(env: &mut Environment, no:i64)
{
    match no
    {
        // rib
        0 =>
        {
	    let new_rib = alloc_rib(env, NUM_0, NUM_0, NUM_0);
	    env.heap[get_car_index(new_rib)] = pop(env);
	    env.heap[get_cdr_index(new_rib)] = pop(env);
	    env.heap[get_tag_index(new_rib)] = pop(env);
	    push2(env, new_rib, PAIR_TAG);
        }

        // id
        1 =>
        {
	    let x = pop(env);
	    push2(env, x, PAIR_TAG);
        }

        // pop
        2 =>
        {
	    pop(env);
	    // TODO: Check what is the meaning of true?
        }

        // skip
        3 =>
        {
	    let x = pop(env);
	    pop(env);
	    push2(env, x, PAIR_TAG);
        }

        // close
        4 =>
        {
	    let mut tos_index = get_tos_index(env);
	    let x = get_car(env, Obj::Num(tos_index as u64));
	    let y = get_cdr(env, env.stack);
	    tos_index = get_tos_index(env);
	    env.heap[tos_index] = alloc_rib(env, x, y, CLOSURE_TAG);
        }

        // is rib?
        5 =>
        {
	    let x = pop(env);
	    let cond = is_rib(&x);
	    let boolean = get_boolean(env, cond);
	    push2(env, boolean, PAIR_TAG);
        }

        // field0
        6 =>
        {
	    let x = pop(env);
	    let car = get_car(env, x);
	    push2(env, car, PAIR_TAG);
        }

        // field1
        7 =>
        {
	    let x = pop(env);
	    let cdr = get_cdr(env, x);
	    push2(env, cdr, PAIR_TAG);
        }

        // field2
        8 =>
        {
	    let x = pop(env);
	    let tag = get_tag(env, x);
	    push2(env, tag, PAIR_TAG)
        }

        // set field0
        9 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    env.heap[get_car_index(x)] = y;
	    push2(env, y, PAIR_TAG);
        }

        // set field1
        10 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    env.heap[get_cdr_index(x)] = y;
	    push2(env, y, PAIR_TAG);
        }

        // set field2
        11 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    env.heap[get_tag_index(x)] = y;
	    push2(env, y, PAIR_TAG);
        }

        // eq
        12 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    let cond = unwrap_obj(&x) == unwrap_obj(&y);
	    let boolean = get_boolean(env, cond);
	    push2(env, boolean, PAIR_TAG);
        }

        // lt
        13 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    let cond = unwrap_obj(&x) < unwrap_obj(&y);
	    let boolean = get_boolean(env, cond);
	    push2(env, boolean, PAIR_TAG);
        }

        // add
        14 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    let num_x = unwrap_obj(&x);
	    let num_y = unwrap_obj(&y);
	    let add = Obj::Num(num_x + num_y);
	    push2(env, add, PAIR_TAG);
        }

        // sub
        15 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    let num_x = unwrap_obj(&x);
	    let num_y = unwrap_obj(&y);
	    let sub = Obj::Num(num_x - num_y);
	    push2(env, sub, PAIR_TAG);
        }

        // mul
        16 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    let num_x = unwrap_obj(&x);
	    let num_y = unwrap_obj(&y);
	    let mul = Obj::Num(num_x * num_y);
	    push2(env, mul, PAIR_TAG);
        }

        // div
        17 =>
        {
	    let x = pop(env);
	    let y = pop(env);
	    let num_x = unwrap_obj(&x);
	    let num_y = unwrap_obj(&y);
	    let div = Obj::Num(num_x / num_y);
	    push2(env, div, PAIR_TAG);
        }

        // getc
        18 =>
        {
	    //
	    let mut buff: [u8; 1] = [0];
	    io::stdin().read_exact(&mut buff);
	    let read = buff[0];
        }

        // putc
        19 =>
        {
	    let x = pop(env);
	    let chr = unwrap_obj(&x) as u8 as char;
	    print!("{}", chr);
        }

        // default ERROR
        _ =>
        {
	    exit_vm(EXIT_ILLEGAL_INSTR);
        }
    }
}


