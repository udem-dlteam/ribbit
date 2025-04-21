
# Copyright : Dominick Basque Morin
#			  Jordan Boulais-Richard
# Date : 02-02-2025

# This file contains an inmplementation of a Hello World RVM written in julia.
# It only implements the core features of Ribbit. 


# |----------- 0. Input string -----------|

# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
input = raw");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"
# )@@


# |----------- 2. VM definitions -----------|

global pos = 0 

# Retrieve bytes from an input string
function get_byte()
    global pos
    pos += 1
    return Int(input[pos])
end

# Decode a byte into a Scheme-compatible value
function get_code()
    x=get_byte()-35
    return  x < 0 ? 57 : x
end

# Decode recursively a integer from the input string
function get_int(accumulator)
    next_byte = get_code()
    accumulator *= 46
    if next_byte < 46
        return accumulator + next_byte
    else
        return get_int(accumulator + next_byte - 46)
    end
end

# Representation of scheme constants as Ribs.
# Special values are tagged with a 5 at the end.
FALSE=[0,0,5] #  #f
TRUE=[0,0,5]  #  #t
NIL=[0,0,5]   # '()

stack=0 # The stack is encoded using a linked list of ribs.

function push(value)
    global stack
    stack = Any[value,stack,0]
    return nothing
end

function pop()
    global stack
    x = stack[1]
    stack = stack[2]
    return x
end

# |----------- 3. VM primitives -----------|

# Function that prints a character to the standard output
function putchar(c)
    write(stdout, Char(c))
    flush(stdout)
    return c
end

# Function that returns the character in the standard input or -1.
function getchar()

    if eof(stdin)
        push(-1)
    else
        c = read(stdin, Char)
        push(Int(c))
    end
end


# Utility functions to create primitives with a specific number of args
function prim1(f)
    return () -> push(f(pop()))
end

function prim2(f)
    return () -> push(f(pop(), pop()))
end

function prim3(f)
    return () -> push(f(pop(), pop(), pop()))
end

# Function to transform a julia boolean into a Scheme one.
function bool2scm(x)
	return x ? TRUE : FALSE
end

# Function to check if a julia object is a rib.
function is_rib(x)
	return isa(x, Vector)
end

# Functions to insert y value in specific index in the x array 
function field0set(y,x)
    x[1] = y # *** indexing in julia
    return y
end

function field1set(y,x)
    x[2] = y 
    return y
end

function field2set(y,x)
    x[3] = y
    return y
end


# |----------- 3. VM primitives -----------|

primitives = Any[
 prim3((z,y,x) -> [x,y,z]),                                          
 prim1(x -> x),                                                      	
 () -> (pop(),0)[2],                                              
 () -> push([pop(),pop()][1]),   
 () -> push([pop()[1],stack,1]),                                         
 prim1(x -> bool2scm(is_rib(x))),                                     
 prim1(x -> x[1]),                                                   	
 prim1(x -> x[2]),                                                   	
 prim1(x -> x[3]),                                                   	
 prim2(field0set),                                                           
 prim2(field1set),                                                           
 prim2(field2set),                                                           
 prim2((y,x) -> bool2scm(is_rib(x) || is_rib(y) ? x === y : x==y)), # doute ici
 prim2((y,x) -> bool2scm(x<y)),                      # error ici                 
 prim2((y,x) -> x+y),                                                 
 prim2((y,x) -> x-y),                                                  
 prim2((y,x) -> x*y),                                                 
 prim2((y,x) -> div(x, y)),                                      
 getchar,                                                              
 prim1(putchar),                                                       
 prim1(exit),                                                          
]

# |----------- 4. VM Symbole table -----------|

# Function that returns the element at i index of lst linked list 
function list_tail(lst, i)
    while i > 0
        lst = lst[2]
        i -= 1
    end
    return lst
end

# Symbol table variable
symtbl=NIL

n = get_int(0)
for _ in 1:n
	global symtbl
	symtbl=Any[Any[FALSE,Any[NIL,0,3],2],symtbl,0]
end


symbol_name = NIL   # The current symbol name (string)
symbol_name_len = 0 # The length of the current symbol name (int)


while true
    global symbol_name, symbol_name_len, symtbl
    
    c = get_byte()  # Get the current byte

    if c == Int(',') # End of symbol

        # Append the symbol with its name to the symbol table
        symbol_to_append = [FALSE, [symbol_name, symbol_name_len, 3], 2]
        symtbl = [symbol_to_append, symtbl, 0]
        symbol_name = NIL
        symbol_name_len = 0

    else
        if c == Int(';') # End of symbol table
            # Append the last symbol and exit loop
            symbol_to_append = [FALSE, [symbol_name, symbol_name_len, 3], 2]
            symtbl = [symbol_to_append, symtbl, 0]
            break
        end

        # Append the current character to the symbol name
        symbol_name = [c, symbol_name, 0]

        # Increment the length of the symbol name
        symbol_name_len += 1

    end
end

# Function that returns the nth symbol in the symbol table
function symbol_ref(n)
    return list_tail(symtbl, n)[1]  #*** indexing in julia
end


# |----------- 5. Decoding of the Code Graph -----------|

while true
	global stack, n 
	x = get_code()
	n=x
	d=0
	op=0
	while true
		d=[20,30,0,10,11,4][op+1]
		if n<=2+d 
            break 
        end
		n-=d+3;op+=1
	end
	if x>90 
		n=pop()
	else
		if op==0 
			stack = Any[0,stack,0]
			op+=1 
		end
		n = n == d ? get_int(0) :
    		n >= d ? symbol_ref(get_int(n - d - 1)) :
    		op < 3 ? symbol_ref(n) : n
		if 4 < op
            n=[[n,0,pop()],NIL,1]
			if stack == 0
				stack = NIL 
                break 
            end
			op = 4
		end
	end
	stack[1] = Any[op - 1, n, stack[1]]
end


# |----------- 6. RVM execution -----------|


pc = n[1][3] 

get_opnd = o -> (is_rib(o) ? o : list_tail(stack, o))

function get_cont()
    s = stack
    
    while ((isa(s[3], Int) && pc[3] == 0) || (isa(pc[3], Vector) && length(s[3]) == 0) )
    	s = s[2]
    end
    return s
end

function set_global(val)
    global symtbl
    symtbl[1][1] = val
    symtbl = symtbl[2]
end

set_global([0, symtbl, 1])  # Primitive 0
set_global(FALSE)
set_global(TRUE)
set_global(NIL)

stack = Any[0, 0, Any[5, 0, 0]]  # Primordial continuation (executes halt instruction)

while true
	global pc, stack

    o = pc[2]  # Extract operand
    i = pc[1]  # Extract instruction type
    
    if i < 1  # Jump/Call
        o = get_opnd(o)[1]
        while true
            c = o[1]
            if is_rib(c)
                c2 = [0, o, 0]
                s2 = c2
                nparams = c[1] >> 1
                while nparams != 0
                    s2 = [pop(), s2, 0]
                    nparams -= 1
                end

                if ((isa(pc[3], Int) && pc[3] != 0) || (isa(pc[3], Vector) && length(pc[3]) > 0) )  # Call
                    c2[1] = stack
                    c2[3] = pc[3]
                else  # Jump
                    k = get_cont()
					c2[1] = k[1]
					c2[3] = k[3]
                end
                stack = s2
            else
				o = primitives[c+1]()
                if is_rib(o)
					continue 
				end
                if ((isa(pc[3], Int) && pc[3] != 0) || (isa(pc[3], Vector) && length(pc[3]) > 0) )  # Call
					c = pc 
                else # Jump
					c = get_cont()
					stack[2] = c[1]
				end
            end
            pc = c
            break
        end
    elseif i < 2  # Set
        get_opnd(o)[1] = stack[1]
        stack = stack[2]
    elseif i < 3 # Get --------
		push(get_opnd(o)[1]) 
		
    elseif i < 4 # const
		push(o) 
    elseif i < 5  # If
        if pop() !== FALSE
			pc = pc[2]
			continue 
		end 
    else  # Halt
        break
    end
    pc = pc[3]
end
