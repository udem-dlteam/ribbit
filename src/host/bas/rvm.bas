// @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
// RVM code that //prints HELLO!
input$ = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"
// )@@

// FONCTION TIRÉE DE LA DOCUMENTATION OFFICIELLE DE YA BASIC: https://2484.de/yabasic/yabasic.htm#ref_compile
sub evemex$(evemex_str$)

  local evemex_pos1, evemex_pos2, evemex_res$

  evemex_pos1 = 1 
  evemex_pos2 = 1 
  evemex_res$ = ""
  	
  while (evemex_pos1 < len(evemex_str$))
    if (mid$(evemex_str$, evemex_pos1, 2) = "{{") then
      evemex_res$ = evemex_res$ + mid$(evemex_str$, evemex_pos2, evemex_pos1 - evemex_pos2)
      evemex_pos1 = evemex_pos1 + 2
      evemex_pos2 = evemex_pos1
      while (evemex_pos2 < len(evemex_str$))
        if (mid$(evemex_str$, evemex_pos2, 2) = "}}") then
           rem 
           rem  See the use of eval in the next line
           rem
	   evemex_res$ = evemex_res$ + eval$("str$(" + mid$(evemex_str$, evemex_pos1, evemex_pos2 - evemex_pos1) + ")")
	   evemex_pos2 = evemex_pos2 + 2
	   evemex_pos1 = evemex_pos2
	   break
	else
	  evemex_pos2 = evemex_pos2 + 1
	endif
      wend
    else
      evemex_pos1 = evemex_pos1 + 1
    endif
  wend

  evemex_res$ = evemex_res$ + mid$(evemex_str$, evemex_pos2, evemex_pos1 - evemex_pos2 + 1)	
  return evemex_res$

end sub


// input string pointer
input_str_pos = 0

// Gets ascii value of byte at position "pos" in the input string
sub get_byte()
    input_str_pos = input_str_pos + 1
    //print "get_byte: ",mid$(input$, input_str_pos, 1)
    //print "get_byte() = ", asc(mid$(input$, input_str_pos, 1))
    return asc(mid$(input$, input_str_pos, 1))
end sub

// Gets ribbit value of byte at position "pos" in the input string
sub get_code()
    current_byte = get_byte()

    // Calculate position in ribbit character dataset
    tmp_code = current_byte - 35
    
    if tmp_code < 0 then
        //print "returning 57"
        return 57  // Special case for the '!' character
    else
        //print "returning ",tmp_code
        return tmp_code
    end if
end sub

// Reads a variable-length integer in the input string
sub get_int(accumulator)
    // Obtain next byte code
    next_byte = get_code()

    // build number in base 46
    accumulator = accumulator * 46

    if next_byte < 46 then
        return accumulator + next_byte // End of the integer
    else
        return get_int(accumulator + next_byte - 46)  // Recursive call to continue reading the integer
    end if
end sub

memory_size = 100
dim memory(memory_size, 2)
top_memory = -1

sub get_elem(rib_ptr, index)
    return memory(safe_shr(rib_ptr,1), index)
end sub

sub augment_memory()
    memory_size = memory_size * 2
    dim memory(memory_size,2)
end sub

sub safe_shl(x, y)
    if x >= 0 then
        return shl(x, y)
    else
        return -shl(abs(x), y)
    fi
end sub

sub safe_shr(x, y)
    if x >= 0 then
        return shr(x, y)
    else
        return -shr(abs(x), y)
    fi
end sub

sub sgy$(rib_ptr)
    if rib_ptr == -1 then
        return sgy$(0)
    elseif is_rib(rib_ptr) then
        return "[" + sgy2$(get_elem(rib_ptr,0)) + "," + sgy2$(get_elem(rib_ptr,1)) + "," + sgy2$(get_elem(rib_ptr,2)) + "]"
    else
        return str$(rib_ptr)
    fi
end sub

sub sgy2$(rib_ptr, level)
    if level == 5 then
        return "..."
    elseif rib_ptr == -1 then
        return sgy$(0)
    fi
    if is_rib(rib_ptr) then
        return "[" + sgy2$(get_elem(rib_ptr,0), level+1) + "," + sgy2$(get_elem(rib_ptr,1),level+1) + "," + sgy2$(get_elem(rib_ptr,2), level+1) + "]"
    else
        return str$(rib_ptr)
    fi
end sub

sub alloc_memory(rib_0,rib_1,rib_2)
    if top_memory >= memory_size then 
        augment_memory()
    fi
    top_memory = top_memory + 1
    memory(top_memory,0) = rib_0
    memory(top_memory,1) = rib_1
    memory(top_memory,2) = rib_2
    return top_memory*2+1
end sub

// Initialize global stack
top_stack = -1

sub push(element)
    //print "push called on ", sgy$(element)
    top_stack = alloc_memory(element,top_stack,0)
end sub

sub pop()
    if top_stack < 0 then
        //print "Stack underflow"
        return 0
    else
        local valeur
        valeur = get_elem(top_stack,0)
        top_stack = get_elem(top_stack,1)
        //print "pop: ", sgy$(valeur)
        //////print "stack: ", sgy$(top_stack)
        return valeur
    end if
end sub

sub putchar(c)
    c = safe_shr(c,1)
    print chr$(c);
    return c
end sub

// Stocks user or file line input
c_input$ = ""

// Pointer to next character to be read in user or file line input
c_input_ptr = 1

// Pushes byte value of next character in user or file line input
sub getchar()
    if c_input$ == "" then
        line input "" c_input$
        c_input_ptr = 1
        //print "c_input$ = ", c_input$
    end if
    if len(c_input$) >= c_input_ptr then
        curr_char$ = mid$(c_input$,c_input_ptr, 1)
        push(asc(curr_char$)*2)
        c_input_ptr = c_input_ptr + 1
    else
        push(-1*2) // eof
        c_input$ = ""
        c_input_ptr = 1
    end if
end sub

lambda_function_SN = 0

// Returns the name of newly compiled lambda function from function_name$ being the name of a function that takes 1 argument
sub prim1$(fonction_name$)
    lambda_function_SN = lambda_function_SN + 1
    
    string_to_compile$ = evemex$("sub lambda_function_{{lambda_function_SN}}():push("+fonction_name$+"(pop())):end sub")
    
    compile(string_to_compile$)
    
    return evemex$("lambda_function_{{lambda_function_SN}}")
end sub

// Returns the name of newly compiled lambda function from function_name$ being the name of a function that takes 2 arguments
sub prim2$(fonction_name$)
    lambda_function_SN = lambda_function_SN + 1
    
    string_to_compile$ = evemex$("sub lambda_function_{{lambda_function_SN}}():push("+fonction_name$+"(pop(),pop())):end sub")
    
    compile(string_to_compile$)
    
    return evemex$("lambda_function_{{lambda_function_SN}}")
end sub

// Returns the name of newly compiled lambda function from function_name$ being the name of a function that takes 3 arguments
sub prim3$(fonction_name$)
    lambda_function_SN = lambda_function_SN + 1
    
    string_to_compile$ = evemex$("sub lambda_function_{{lambda_function_SN}}():push("+fonction_name$+"(pop(),pop(),pop())):end sub")
    compile(string_to_compile$)
    
    return evemex$("lambda_function_{{lambda_function_SN}}")
end sub

sub is_rib(elem)
    if mod(elem, 2) == 0 then
        return 0
    fi
    return 1
end sub

// Primitive table as defined in the paper.
dim primitives$(1,21)

// Primitive 1
// prim3(lambda z,y,x: [x,y,z]), # (primitive (%%rib a b c))
sub primitive1(z, y, x)
    reference = alloc_memory(x,y,z)
    return reference
end sub

primitives$(1, 1) = prim3$("primitive1")

// Primitive 2
// prim1(lambda x:x), # (primitive (%%id x))
sub primitive2(x)
    return x
end sub

primitives$(1, 2) = prim1$("primitive2")

// Primitive 3
//lambda:(pop(),0)[1], # (primitive (%%arg1 x y))
sub primitive3()
    pop()
    return 0
end sub

primitives$(1, 3) = "primitive3"

// Primitive 4
//lambda:push([pop(),pop()][0]), # (primitive (%%arg2 x y))
sub primitive4()
    local elem_1
    elem_1 = pop()
    pop()
    push(elem_1)
end sub

primitives$(1, 4) = "primitive4"

// Primitive 5
// lambda:push([pop()[0],stack,1]), # (primitive (%%close rib))
sub primitive5()
    local elem_1
    elem_1 = pop()
    local elem_1_at_0
    elem_1_at_0 = get_elem(elem_1, 0)
    local mem_addr
    mem_addr = alloc_memory(elem_1_at_0, top_stack, 1)
    push(mem_addr)
end sub

primitives$(1, 5) = "primitive5"

// Primitive 6
// prim1(lambda x:bool2scm(is_rib(x))), # (primitive (%%rib? rib))
sub primitive6(x)
    return bool2scm(is_rib(x))
end sub

primitives$(1, 6) = prim1$("primitive6")

// Primitive 7
// prim1(lambda x:x[0]), # (primitive (%%field0 rib))
sub primitive7(x)
    return get_elem(x, 0)
end sub

primitives$(1, 7) = prim1$("primitive7")

// Primitive 8
//  prim1(lambda x:x[1]), # (primitive (%%field1 rib))
sub primitive8(x)
    return get_elem(x, 1)
end sub

primitives$(1, 8) = prim1$("primitive8")

// Primitive 9
// prim1(lambda x:x[2]), # (primitive (%%field2 rib))
sub primitive9(x)
    return get_elem(x, 2)
end sub

primitives$(1, 9) = prim1$("primitive9")

// Primitive 10
// prim2(field0set), # (primitive (%%field0-set! rib x))
// Met à jour l'élément 0 du tableau x avec y
sub field0set(y, x)
    local mem_indx
    mem_indx = safe_shr(x,1)
    memory(mem_indx,0) = y
    return y
end sub

primitives$(1,10) = prim2$("field0set")

// Primitive 1
// prim2(field1set), # (primitive (%%field1-set! rib x))
// Met à jour l'élément 1 du tableau x avec y
sub field1set(y, x)
    local mem_indx
    mem_indx = safe_shr(x,1)
    memory(mem_indx,1) = y
    return y
end sub

primitives$(1,11) = prim2$("field1set")

// Primitive 12
// prim2(field2set), # (primitive (%%field2-set! rib x))
// Met à jour l'élément 2 du tableau x avec y
sub field2set(y, x)
    local mem_indx
    mem_indx = safe_shr(x,1)
    memory(mem_indx,2) = y
    return y
end sub

primitives$(1,12) = prim2$("field2set")

// Primitive 13
// prim2(lambda y,x:bool2scm(x is y if is_rib(x) or is_rib(y) else x==y)), # (primitive (%%eqv? x y))
// NOTE: Puisque toutes nos données sont des int (même les addresses mémoire), on n'a pas besoin de vérifier si x et y sont des ribs, on peut directement les comparer
sub primitive13(y, x)
    if y = x then
        return TrueRib
    fi
    return FalseRib
end sub

primitives$(1,13) = prim2$("primitive13")

// Primitive 14
// prim2(lambda y,x:bool2scm(x<y)), # (primitive (%%< a b))
sub primitive14(y, x)
    return bool2scm(x<y)
end sub

primitives$(1,14) = prim2$("primitive14")

// Primitive 15
// prim2(lambda y,x:x+y), # (primitive (%%+ a b))
sub primitive15(y,x)
    local sum
    sum = safe_shr(x,1) + safe_shr(y,1)
    return safe_shl(sum,1)
end sub

primitives$(1,15) = prim2$("primitive15")

// Primitive 16
// prim2(lambda y,x:x-y), # (primitive (%%- a b))
sub primitive16(y,x)
    local substraction
    substraction = safe_shr(x,1) - safe_shr(y,1)
    return safe_shl(substraction,1)
end sub

primitives$(1,16) = prim2$("primitive16")

// Primitive 17
// prim2(lambda y,x:x*y), # (primitive (%%* a b))
sub primitive17(y,x)
    return safe_shr(x,1) * y
end sub

primitives$(1,17) = prim2$("primitive17")

// Primitive 18
// prim2(lambda y,x:int(x/y)), # (primitive (%%quotient a b))
sub primitive18(y,x)
    return safe_shl(x/y,1)
end sub

primitives$(1,18) = prim2$("primitive18")

// Primitive 19
// getchar, # (primitive (%%getchar))

primitives$(1,19) = "getchar"

// Primitive 20
// prim1(putchar), # (primitive (%%putchar c))

primitives$(1,20) = prim1$("putchar")

// Primitive 21
// prim1(exit), # (primitive (%%exit a))
sub primitive21()
    exit
end sub

primitives$(1,21) = prim1$("primitive21")



// Careful: This function is implemented with 0-indexing as in the hello.py reference file
sub list_tail(lst, index)
    while index > 0
        lst = get_elem(lst,1)
        index = index - 1
    wend
    return lst
end sub

FalseRib = alloc_memory(0,0,5*2)
TrueRib = alloc_memory(0,0,5*2)
NilRib = alloc_memory(0,0,5*2)


// Prend un bool (1 ou 0) en entrée et retourne l'adresse équivalente du booléen Scheme
sub bool2scm(bool)
    if bool = 1 then
        return TrueRib
    endif
    return FalseRib
end sub

symtbl_ptr = NilRib

n = get_int(0)
for x=1 to n
    empty_str = alloc_memory(NilRib,0,3*2)
    empty_symbol = alloc_memory(FalseRib,empty_str,2*2)
    symtbl_ptr = alloc_memory(empty_symbol, symtbl_ptr, 0)
next x

// The bytecode contains the symbols name in reverse order, separated by a comma.
// For example, the symbols foo and bar would be encoded as "rab,oof"

symbol_name = NilRib // The current symbol name (string)
symbol_name_len = 0 // The length of the current symbol name (int)

while 1
    char_byte = get_byte() // current byte
    //print "c = ",char_byte
    if char_byte == asc(",") then // end of symbol
        // Appends the symbol with its name to the symbol table
        symbol_name_rib = alloc_memory(symbol_name, symbol_name_len*2, 3*2)
        symbol_to_append = alloc_memory(FalseRib, symbol_name_rib, 2*2)
        symtbl_ptr = alloc_memory(symbol_to_append, symtbl_ptr, 0)
        //////print "symtbl = ",sgy$(symtbl_ptr)
        symbol_name = NilRib
        symbol_name_len = 0
    else
        if char_byte == asc(";") then // end of symbol table
            // Append last symbol and leave
            symbol_name_rib = alloc_memory(symbol_name, symbol_name_len*2, 3*2)
            symbol_to_append = alloc_memory(FalseRib, symbol_name_rib, 2*2)
            //print "symbol_to_append = ",sgy$(symbol_to_append)
            symtbl_ptr = alloc_memory(symbol_to_append, symtbl_ptr, 0)
            //////print "symtbl2 = ",sgy$(symtbl_ptr)
            goto end_while_symtbl
        fi
        // Appends the current character to the symbol name
        symbol_name = alloc_memory(char_byte*2, symbol_name, 0) // TAG BYTE
        //print "curr_symbol_name = ",sgy$(symbol_name)

        // Increment the length of the symbol name
        symbol_name_len = symbol_name_len + 1
    fi
wend
label end_while_symtbl

sub symbol_ref(n)
    // Returns the nth symbol in the symbol table
    return get_elem(list_tail(symtbl_ptr, n),0)
end sub

// ========================================
// == (5) Decoding of the RVM's bytecode ==
// ========================================
dim opTable(6)
opTable(0) = 20 : opTable(1) = 30 : opTable(2) = 0 : opTable(3) = 10 : opTable(4) = 11 : opTable(5) = 4 

while 1
    //print "here"
    ////print "stack = ",sgy$(top_stack)
    x = get_code()
    n = x
    d = 0
    op = 0
    while 1
        d = opTable(op)
        //print "n = ",n
        //print "d = ",d
        //print "op = ",op
        if (n <= (2 + d)) then
            //print "break"
            goto inner_while_loop
        endif
        n = n - (d + 3)
        op = op + 1
        //print "n2 = ",n
        //print "op2 = ",op
    wend
    label inner_while_loop
    //print "here2"
    if (x > 90) then
        //print "in x>90"
        n = pop()
    elseif 1 then
        if (op = 0) then 
            //print "in op==0"
            top_stack = alloc_memory(0,top_stack,0)
            op = op + 1
            ////print "stack = ",sgy$(top_stack)
        endif
        if (n = d) then
            //print "n=d"
            n = safe_shl(get_int(0),1) // TAG_NUM
            //print "n = ",n
        elseif (n >= d) then
                //print "n>=d"
                n = symbol_ref(get_int(n - d - 1))
                //print "n = ",sgy$(n)
        elseif (op<3) then
                //print "op<3"
                n = symbol_ref(n)
                //print "n = ",sgy$(n)
        elseif 1 then
            //print "else"
            //print "n = ",n
            n = safe_shl(n,1) // TAG_NUM
        endif
        
        if (4 < op) then
            //print "in 4<op"
            popVal = pop()
            //print "n_before = ",n
            //////print "popVal = ",popVal
            n_rib0 = alloc_memory(n, 0, popVal)
            n = alloc_memory(n_rib0, NilRib, 1*2)
            //print "n_after = ",sgy$(n)
            if (top_stack = -1) then
                //print "in not stack"
                goto while_end
            endif
            op = 4
        endif
    endif
    field0set(alloc_memory(safe_shl(op-1,1), n, get_elem(top_stack, 0)), top_stack) // TAG_NUM op
wend
label while_end
////print "stack = ",sgy$(top_stack)
// =======================
// == (6) RVM execution ==
// =======================

pc = get_elem(get_elem(n,0),2) // pc = n[0][2]
//////print "pc = ",sgy$(pc)

sub get_opnd(obj) // get_opnd=lambda o:(o if is_rib(o) else list_tail(stack,o))
    if is_rib(obj) then 
        return obj
    fi
    return list_tail(top_stack, safe_shr(obj,1)) // UNTAG_NUM
end sub

sub get_cont()
    local s
    s = top_stack
    while !get_elem(s,2)
        s = get_elem(s,1)
    wend
    return s
end sub

sub set_global(valeur)
    field0set(valeur, get_elem(symtbl_ptr,0))
    symtbl_ptr = get_elem(symtbl_ptr,1)
end sub

//////print "symtbl0 = ",sgy$(symtbl_ptr)
set_global(alloc_memory(0, symtbl_ptr, 1)) // primitive 0
//////print "symtbl1 = ",sgy$(symtbl_ptr)
set_global(FalseRib)
//////print "symtbl2 = ",sgy$(symtbl_ptr)
set_global(TrueRib)
//////print "symtbl3 = ",sgy$(symtbl_ptr)
set_global(NilRib)
//////print "symtbl4 = ",sgy$(symtbl_ptr)

top_stack= alloc_memory(0, 0, alloc_memory(10,0,0)) // primordial continuation (executes halt instr.)
label outer_while
while 1
    //print "pc = ",sgy$(pc)
    o=get_elem(pc, 1) // o=pc[1]
    //print "o = ",sgy$(o)
    //print "stack = ",sgy$(top_stack)
    
    i = safe_shr(get_elem(pc, 0),1) //i=pc[0] - UNTAG
    //print "i = ",i
    if i<1 then // jump/call
        o=get_elem(get_opnd(o), 0) // o=get_opnd(o)[0]
        //print "o2 = ",sgy$(o)
        while 1:
            label inner_while
            c=get_elem(o, 0)
            //print "c = ",c
            if is_rib(c) then
                c2= alloc_memory(0, o, 0)
                s2=c2
                nparams=safe_shr(get_elem(c,0),2)
                //print "c2 = ",sgy$(c2),", nparams = ",nparams
                while nparams
                    s2= alloc_memory(pop(), s2, 0)
                    nparams = nparams - 1
                wend
                if get_elem(pc,2) then // call
                    //print "call"
                    field0set(top_stack, c2)
                    field2set(get_elem(pc, 2), c2)
                else // jump
                    //print "jump"
                    k=get_cont()
                    field0set(get_elem(k,0), c2)
                    field2set(get_elem(k,2), c2)
                fi
                top_stack=s2
                ////print "stack_s2 = ",sgy$(top_stack)
            else:
                c = safe_shr(c,1) // UNTAG
                no_of_primitive = c+1
                //print "calling primitive #",c
                o = execute(primitives$(1,no_of_primitive))
                if is_rib(o) then 
                    goto inner_while
                fi
                if get_elem(pc,2) then // call
                    //print "call2"
                    c=pc
                else // jump
                    //print "jump2"
                    c=get_cont()
                    //print "c_cont = ",sgy$(c)
                    field1set(get_elem(c,0), top_stack)
                    //print "stack_cont = ",sgy$(top_stack)
                fi
            fi
            pc=c
            goto inner_while_end
        wend
        label inner_while_end
    elseif i<2 then // set
            //print "i<2"
            field0set(get_elem(top_stack,0), get_opnd(o))
            top_stack = get_elem(top_stack,1)
            ////print "stack_i_int<2 = ",sgy$(top_stack)
            
    elseif i<3 then // get
            //print "i<3"
            push(get_elem(get_opnd(o),0))
    elseif i<4 then // const
            //print "i<4"
            push(o)
    elseif i<5 then // if
            //print "i<5"
            //print "pc_before = ",sgy$(pc)
            elem = pop()
            if elem!=FalseRib then
                pc = get_elem(pc,1)
                //print "pc_after = ",sgy$(pc)
                goto outer_while
            fi
            
    else // halt
        //print "program_end"
        goto program_end
    fi
    pc = get_elem(pc, 2)
wend
label program_end
