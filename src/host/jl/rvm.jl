# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
input = raw");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"  # RVM code that prints HELLO!
# )@@

function putChar(c)
    write(stdout, Char(c))
    flush(stdout)
    return c
end

function getChar()
    if !eof(stdin)
        c = read(stdin, Char)
        push(Int(c))
    else
        push(-1)
    end
end

#----- DEBUG SECTION -----#
debug = false
tracing = false
step_count = 0
start_tracing = 0
next_stamp = 0

function show_trace(obj)
    if !is_rib(obj)
        return string(obj)
    end
    type = obj[3]
    if type == 4
        return string("#", show_trace(obj[1]))
    end
    result = ""
    if type == 0
        n = 1
        result *= "(" * show_trace(obj[1])
        obj = obj[2]
        while is_rib(obj) && obj[3] == 0
            if n > 4
                result *= " ..."
                obj = NIL
                break
            end
            result *= " " * show_trace(obj[1])
            obj = obj[2]
            n += 1
        end
        if obj == NIL
            result *= " . " * show_trace(obj)
        end
        result *= ")"
    elseif type == 1
        if is_rib(obj[1])
            result *= "#<procedure nparams=" * string(obj[1][1]) * ">"
        else
            result *= "#<primitive " * string(obj[1]) * ">"
        end
    elseif type == 2
        obj = obj[2]
        if (is_rib(obj)) && (obj[3] == 3) && (obj[2] > 0)
            obj = obj[1]
            while is_rib(obj) && obj[3] == 0
                result *= Char(obj[1])
                obj = obj[2]
            end
        else
            result *= "#<symbol " * show_trace(obj) * ">"
        end
    elseif type == 3
        result *= "\""
        obj = obj[1]
        while is_rib(obj) && obj[3] == 0
            c = Char(obj[1])
            if c == "\n"
                c = "n"
                result *= "\\"
            elseif c == "\r"
                c = "r"
                result *= "\\"
            elseif c == "\t"
                c = "t"
                result *= "\\"
            elseif c == "\\" || c == "\""
                result *= "\\"
            end
            result *= c
            obj = obj[2]
        end
        result *= "\""
    elseif type == 5
        if obj === FALSE
            result *= "#f"
        elseif obj === TRUE
            result *= "#t"
        elseif obj === NIL
            result *= "()"
        else
            result *= "[" * show_trace(obj[1]) * "," * show_trace(obj[2]) * "," * show_trace(obj[3]) * "]"
        end
    else
        result *= "[" * show_trace(obj[1]) * "," * show_trace(obj[2]) * "," * show_trace(obj[3]) * "]"
    end
    return result

end

function start_step()
    global step_count, tracing, next_stamp
    step_count += 1
    if step_count >= start_tracing
        tracing = true
    end
    if !tracing
        if step_count >= next_stamp
            next_stamp = int(next_stamp * 1.01 + 1)
            print("@" * string(step_count))
        end
        return
    end
    s = stack
    result = "@" * string(step_count) * " STACK = ("
    sep = ""
    while isa(s, Vector) && s[3] == 0
        result *= sep * show_trace(s[1])
        sep = " "
        s = s[2]
    end
    result *= ")\n"
    print(result)
end
#----- END OF DEBUG SECTION -----#

pos = 0
function get_byte()
    global pos
    pos += 1
    return Int(input[pos])
end

FALSE = [0, 0, 5]
TRUE = [0, 0, 5]
NIL = [0, 0, 5]

to_bool = x -> x ? TRUE : FALSE

function is_rib(x)
    return x isa Vector{}
end

stack = 0

function push(x)
    global stack
    stack = [x, stack, 0]
end

function pop()
    global stack
    x = stack[1]
    stack = stack[2]
    return x
end

prim1 = f -> () -> push(f(pop()))
prim2 = f -> () -> push(f(pop(), pop()))
prim3 = f -> () -> push(f(pop(), pop(), pop()))

function arg2()
    x = pop()
    pop()
    push(x)
end

function close()
    push([pop()[1], stack, 1])
end

function f0s(y, x)
    x[1] = y
    return y
end

function f1s(y, x)
    x[2] = y
    return y
end

function f2s(y, x)
    x[3] = y
    return y
end

primitives = [
    # @@(primitives (gen body)
    prim3((z, y, x) -> [x, y, z]), # @@(primitive (rib a b c))@@
    prim1(x -> x), # @@(primitive (id x))@@
    pop, # @@(primitive (arg1 x y))@@
    arg2, # @@(primitive (arg2 x y))@@
    close, # @@(primitive (close rib))@@
    prim1(x -> to_bool(is_rib(x))), # @@(primitive (rib? rib))@@
    prim1(x -> x[1]), # @@(primitive (field0 rib))@@
    prim1(x -> x[2]), # @@(primitive (field1 rib))@@
    prim1(x -> x[3]), # @@(primitive (field2 rib))@@
    prim2(f0s), # @@(primitive (field0-set! rib x))@@
    prim2(f1s), # @@(primitive (field1-set! rib x))@@
    prim2(f2s), # @@(primitive (field2-set! rib x))@@
    prim2((y, x) -> to_bool(is_rib(x) || is_rib(y) ? x === y : x == y)), # @@(primitive (eqv? x y))@@
    prim2((y, x) -> to_bool(x < y)), # @@(primitive (< a b))@@
    prim2((y, x) -> x + y), # @@(primitive (+ a b))@@
    prim2((y, x) -> x - y), # @@(primitive (- a b))@@
    prim2((y, x) -> x * y), # @@(primitive (* a b))@@
    prim2((y, x) -> trunc(Int, x / y)), # @@(primitive (quotient a b))@@
    getChar, # @@(primitive (getchar))@@
    prim1((x) -> putChar(x)), # @@(primitive (putchar c))@@
    prim1(exit), # @@(primitive (exit a))@@
    # )@@
]


function get_code()
    x = get_byte() - 35
    return x < 0 ? 57 : x
end

function get_int(n)
    x = get_code()
    n *= 46
    return x < 46 ? n + x : get_int(n + x - 46)
end

list_tail = (lst, i) -> i == 0 ? lst : list_tail(lst[2], i - 1)

# build the initial symbol table

symtbl = NIL
n = get_int(0)
while n > 0
    global n -= 1
    global symtbl = [[FALSE, [NIL, 0, 3], 2], symtbl, 0]
end

accum = NIL
n = 0
while true
    c = get_byte()
    if c == 44
        global symtbl = [[FALSE, [accum, n, 3], 2], symtbl, 0]
        global accum = NIL
        global n = 0
    else
        if c == 59
            break
        end
        accum = [c, accum, 0]
        n += 1
    end
end

symtbl = [[FALSE, [accum, n, 3], 2], symtbl, 0]

function symbol_ref(n)
    global symtbl
    return list_tail(symtbl, n)[1]
end

# decode the RVM instructions
while true
    global stack
    x = get_code()
    global n = x
    d = 0
    op = 0
    while true
        d = [20, 30, 0, 10, 11, 4][op+1]
        if n <= 2 + d
            break
        end
        n -= d + 3
        op += 1
    end
    if x > 90
        n = pop()
    else
        if op == 0
            stack = [0, stack, 0]
            stack = convert(Array{Any}, stack)
            op += 1
        end
        if n == d
            n = get_int(0)
        elseif n >= d
            n = symbol_ref(get_int(n - d - 1))
        elseif op < 3
            n = symbol_ref(n)
        end
        if 4 < op
            n = [[n, 0, pop()], NIL, 1]
            global stack
            if (stack == 0)
                break
            end
            op = 4
        end
    end
    stack[1] = [op - 1, n, stack[1]]
end

pc = n[1][3]

function get_opnd(o)
    global stack
    return is_rib(o) ? o : list_tail(stack, o)
end

function get_cont()
    s = stack
    while isa(s, Array) && s[3] == 0
        s = s[2]
    end
    return s
end

function set_global(val)
    global symtbl
    symtbl[1][1] = val
    symtbl = symtbl[2]
end

set_global([0, symtbl, 1]) # primitive 0
set_global(FALSE)
set_global(TRUE)
set_global(NIL)

stack = [0, 0, [5, 0, 0]] # primordial continuation (executes halt instr.)

while true
    if debug
        start_step()                                                 #DEBUG
    end

    global pc, stack, primitives
    o = pc[2]
    i = pc[1]
    if i < 1 # jump/call
        if tracing
            if is_rib(pc[3])
                print(string("call ", show_trace(o), '\n'))          #DEBUG
            else
                print(string("jump ", show_trace(o), '\n'))          #DEBUG
            end
        end
        o = get_opnd(o)[1]
        c = o[1]
        if is_rib(c)
            c2 = [0, o, 0]
            s2 = c2
            nargs = c[1]
            while nargs != 0
                s2 = [pop(), s2, 0]
                nargs -= 1
            end
            if is_rib(pc[3]) # call
                c2[1] = stack
                c2[3] = pc[3]
            else # jump
                k = get_cont()
                c2[1] = k[1]
                c2[3] = k[3]
            end
            stack = s2
        else
            primitives[c+1]()
            if is_rib(pc[3]) # call
                c = pc
            else # jump
                c = get_cont()
                stack[2] = c[1]
            end
        end
        pc = c[3]
    elseif i < 2 # set
        if tracing
            print(string("set ", show_trace(o), '\n'))               #DEBUG
        end
        get_opnd(o)[1] = stack[1]
        stack = stack[2]
        pc = pc[3]
    elseif i < 3 # get
        if tracing
            print(string("get ", show_trace(o), '\n'))               #DEBUG
        end
        push(get_opnd(o)[1])
        pc = pc[3]
    elseif i < 4 # const
        if tracing
            print(string("const ", show_trace(o), '\n'))             #DEBUG
        end
        push(o)
        pc = pc[3]
    elseif i < 5 # if
        if tracing
            print("if\n")                                            #DEBUG
        end
        if pop() === FALSE
            pc = pc[3]
        else
            pc = pc[2]
        end

    else # halt
        if tracing
            print("halt\n")                                          #DEBUG
        end
        break
    end
end
