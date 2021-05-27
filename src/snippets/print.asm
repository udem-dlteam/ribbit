# print value in cx with carriage return + newline
pusha
push cx
push 0
call print_i
add  sp, 2
popa

mov  al, `\r`
call write

mov  al, `\n`
call write
