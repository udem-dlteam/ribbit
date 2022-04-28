local input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

-- All variables are named such that the 'local' declarations can be
-- removed without causing clashes.  This rreduces the output Lua size
-- after minification, but makes LuaJIT less effective.

-- Functionality exposed to test wrapper.
local function rib(x,y,z) return {x,y,z} end
local function rib(x,y,z) --debug--
   assert(x and y and z) --debug--
   return { class = 'rib', [1]=x, [2]=y, [3]=z } --debug--
end --debug--

local FALSE=rib(0,0,5)
local TRUE=rib(0,0,5)
local NIL=rib(0,0,5)
local function is_rib(x) return type(x) == "table" end
-- local function is_rib(x) return type(x) == 'table' and x.class == 'rib' end --debug--

-- Called from test wrapper.  In normal use, this code falls into the
-- interpreter, and expects input to be defined.
local function test(input, dbg)  --debug--


local trace_instruction = function(...) end --debug--
if dbg then trace_instruction = dbg.trace_instruction end --debug--

local function putchar(x)
   local a=io.stdout
   a:write(string.char(x))
   a:flush()
   return x
end

local stack=0
local function push(x)
   stack = rib(x,stack,0)
end
local function getchar()
   local c=io.stdin:read(1)
   if not c then c=-1 else c=c:byte(1) end
   push(c)
end

local pos=0
local function get_byte()
   pos=pos+1
   local a=input:byte(pos) or 0
   return a
end

local function to_bool(x)
   if x then
      return TRUE
   else
      return FALSE
   end
end

local function pop()
   local x=stack[1]
   stack=stack[2]
   assert(x) --debug--
   return x
end

-- Argument evaluation order is undefined in Lua, vs. Python left-to-right.
-- Since we need to be explicit anyway, we use normal argument order for f,
-- as opposed to Python RVM which has them reversed.
local function prim1(f)
   return function()
      local a=pop()
      push(f(a))
   end
end
local function prim2(f)
   return function()
      local b=pop()
      local a=pop()
      push(f(a,b))
   end
end
local function prim3(f)
   return function()
      local c=pop()
      local b=pop()
      local a=pop()
      push(f(a,b,c))
   end
end

local function arg2()
      local a = pop()
      pop()
      push(a)
end
local function close()
      push(rib(pop()[1],stack,1))
end
local function f0s(x,y)
      x[1]=y
      return y
end
local function f1s(x,y)
      x[2]=y
      return y
end
local function f2s(x,y)
      x[3]=y
      return y
end

local function quotient(x, y)
   local b = math.floor
   local a = x/y
   if a<0 then
      return -b(-a)
   else
      return b(a)
   end
end

local primitives = {
   prim3(rib), -- 0
   prim1(function(x) return x end), -- 1
   pop, -- 2
   arg2, -- 3
   close, -- 4
   prim1(function(x) return to_bool(is_rib(x)) end), -- 5
   prim1(function(x) return x[1] end), -- 6
   prim1(function(x) return x[2] end), -- 7
   prim1(function(x) return x[3] end), -- 8
   prim2(f0s), -- 9
   prim2(f1s), -- 10
   prim2(f2s), -- 11
   prim2(function(x,y) return to_bool(x == y) end), -- 12
   prim2(function(x,y) return to_bool(x<y) end), -- 13
   prim2(function(x,y) return x + y end), -- 14
   prim2(function(x,y) return x - y end), -- 15
   prim2(function(x,y) return x * y end), -- 16
   prim2(quotient), -- 17
   getchar, -- 18
   prim1(putchar), -- 19
   prim1(os.exit) -- 20
}

local function get_code()
   local a = get_byte() - 35
   if a<0 then
      return 57
   else
      return a
   end
end

local function get_int(x)
   local a = get_code()
   x = x * 46
   if a<46 then
      return x+a
   else
      return get_int(x + a -46)
   end
end

local function list_tail(x, y)
   if y==0 then
      return x
   else
      return list_tail(x[2],y-1)
   end
end

local symtbl = NIL


local function symbol_ref(x)
   return list_tail(symtbl,x)[1]
end


local function set_global(x)
   symtbl[1][1]=x
   symtbl=symtbl[2]
end

-- build symtbl

-- Note that this variable is used to pass data between subsequent
-- parts of the code, similar to the Python code.
local n = get_int(0)

while n>0 do
   n = n - 1
   symtbl = rib(rib(0,rib(NIL,0,3),2),symtbl,0)
end
local accum = NIL
n=0
while true do
   local c = get_byte()
   if c == 44 then
      symtbl = rib(rib(0,rib(accum,n,3),2),symtbl,0)
      accum = NIL
      n = 0
   else
      if c == 59 then break end
      accum = rib(c, accum, 0)
      n = n + 1
   end
end
symtbl=rib(rib(0,rib(accum,n,3),2),symtbl,0)

-- n is unused

-- decode
while true do
   local x=get_code()
   n=x
   local d=0
   local op=0
   while true do
      local a={20,30,0,10,11,4}
      d=a[op+1]
      if n<=2+d then break end
      n=n-(d+3) ; op=op+1
   end
   if x>90 then
      n=pop()
   else
      if op==0 then
         stack=rib(0,stack,0);
         op = op + 1
      end
      if n==d then
         n = get_int(0)
      elseif n>= d then
         n = symbol_ref(get_int(n-d-1))
      elseif op<3 then
         n = symbol_ref(n)
      end
      if 4<op then
         n=rib(rib(n,0,pop()),0,1)
         if not is_rib(stack) then
            break
         end
         op=4
      end
   end
   stack[1]=rib(op-1,n,stack[1])
end

-- n contains main-proc, e.g. result of (decode) in hosts/scm/rvm.scm

set_global(rib(0,symtbl,1)) -- procedure type, primitive 0
set_global(FALSE)
set_global(TRUE)
set_global(NIL)

local function get_opnd(o)
   if is_rib(o) then return o
   else return list_tail(stack,o) end
end

local function get_cont()
   local a = stack
   while not is_rib(a[3]) do a = a[2] end
   return a
end

local pc = n[1][3]
stack=rib(0,0,rib(5,0,0)) -- primordial continuation (executes halt instr.)

-- run
while true do
   local o=pc[2]
   local i=pc[1]
   if i<1 then -- jump/call
      trace_instruction("jump/call",o,stack) --debug--
      o=get_opnd(o)[1]
      local c=o[1]
      if is_rib(c) then
         local c2=rib(0,o,0)
         local s2=c2
         nargs=c[1]
         while nargs > 0 do
            s2=rib(pop(),s2,0)
            nargs=nargs-1
         end
         if is_rib(pc[3]) then -- call
            c2[1]=stack
            c2[3]=pc[3]
         else -- jump
            k=get_cont()
            c2[1]=k[1]
            c2[3]=k[3]
         end
         stack=s2

      else
         primitives[c+1]()
         if is_rib(pc[3]) then -- call
            c=pc
         else --  jump
            c=get_cont()
            stack[2]=c[1]
         end
      end
      pc=c[3]

   elseif i<2 then -- set
      trace_instruction("set",o,stack) --debug--
      x=pop()
      get_opnd(o)[1]=x
      pc=pc[3]

   elseif i<3 then -- get
      trace_instruction("get",o,stack) --debug--
      push(get_opnd(o)[1])
      pc=pc[3]

   elseif i<4 then -- const
      trace_instruction("const",o,stack) --debug--
      push(o)
      pc=pc[3]

   elseif i<5 then -- if
      trace_instruction("if",o,stack) --debug--
      if pop() == FALSE then
         pc=pc[3]
      else
         pc=pc[2]
      end

   else -- halt
      break
   end
end

end --debug--

-- https://stackoverflow.com/questions/49375638/how-to-determine-whether-my-code-is-running-in-a-lua-module
-- Checks if there is anything in the 1st variable at the 4th level,
-- which would be the caller of the current module (if required) or
-- nothing in the case of the main script.
if pcall(debug.getlocal, 4, 1) then --debug--
   -- The debug code is part of lure luarocks package as
   -- 'lure.rvm_debug'.  It will load this code as a module, so defer
   -- exeuction here.
   return { test = test, is_rib = is_rib, rib = rib, FALSE = FALSE, TRUE = TRUE, NIL = NIL } --debug--
else test(input) end --debug--
