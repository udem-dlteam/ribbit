    global loader                   ; the entry symbol for ELF
    global receive_scancode
    global inb
    global outw
    global outb
    global lidt
    global idtr_generic
    global irq_handlers
    extern handle_irq


    MAGIC_NUMBER equ 0x1BADB002     ; define the magic number constant
    FLAGS        equ 0x0            ; multiboot flags
    CHECKSUM     equ -MAGIC_NUMBER  ; calculate the checksum
                                    ; (magic number + checksum + flags should equal 0)


%macro irq_handler_noerror 1
  align 4
  irq_%1:
    pushad
    push 0
    push 0
    push %1
    cld
    call handle_irq
    add esp,12
    popad
    iret
%endmacro
%macro irq_handler_error 1
  align 4
  irq_%1:
    push eax
    mov eax,[esp+4]
    pushad
    push eax
    push 1
    push %1
    cld
    call handle_irq
    add esp,12
    popad
    pop eax
    add esp,4
    iret
%endmacro
%macro irq_entry 1
  dd irq_%1
%endmacro

    section .boot.text
    align 4                         ; the code must be 4 byte aligned
        dd MAGIC_NUMBER             ; write the magic number to the machine code,
        dd FLAGS                    ; the flags,
        dd CHECKSUM                 ; and the checksum

    extern kernel_main
    extern c_irq
    loader:                         ; the loader label (defined as entry point in linker script)
      cli
      jmp 0x08:kload
    kload:
       mov edx,eax
       mov ax,0x10
       mov ds,ax
       mov es,ax
       mov fs,ax
       mov gs,ax
       mov ss,ax
       mov ax,0xf001
       out 0x60,ax
       mov esp, kernel_stack + KERNEL_STACK_SIZE 
       lea eax, end_of_mem
       push eax
       push edx
       push ebx
       call kernel_main
    .loop:
        jmp .loop                   ; loop forever
    enable_paging:
       mov eax, cr0
       or eax, 0x80000001
       mov cr0, eax
       ret
    lidt:
      mov eax,[esp+4]
      lidt [eax]
      ret
    idtr_generic:
      cli
      pushad
      call c_irq
      popad
      ret
    receive_scancode:
      in al,64h
      test al,1
      je receive_scancode
      in al,60h
      ret
    inb:
      mov edx,[esp+4]
      xor eax,eax
      in al,dx
      ret
    outw:
      mov edx,[esp+4]
      mov eax,[esp+8]
      out dx,ax
      ret
    outb:
      mov edx,[esp+4]
      mov eax,[esp+8]
      out dx,al
      ret
    set_page_directory:
      pop eax
      mov cr3,eax
      ret
section .text

%assign i 0
%rep 7
  irq_handler_noerror i
%assign i i+1
%endrep

irq_handler_noerror 7
irq_handler_error 8
irq_handler_noerror 9

%assign i 10
%rep 5
  irq_handler_error i
%assign i i+1
%endrep

irq_handler_noerror 15
irq_handler_noerror 16
irq_handler_error 17

%assign i 18
%rep 3
  irq_handler_noerror i
%assign i i+1
%endrep

irq_handler_error 21

%assign i 22
%rep 6
  irq_handler_noerror i
%assign i i+1
%endrep

irq_handler_noerror 28
irq_handler_error 29
irq_handler_error 30
irq_handler_error 31

%assign i 32
%rep 224
  irq_handler_noerror i
%assign i i+1
%endrep

irq_handlers:
%assign i 0
%rep 256
  irq_entry i
%assign i i+1
%endrep

gdtr:
  dw 0x18
  dd gdt
gdt:
  dq 0
gdt_code:
  dd 0xffff
  dd 0x0000
  db 0x00
  db 0x9a
  db 0xfc
  db 0x00
gdt_data:
  dd 0xffff
  dd 0x0000
  db 0x00
  db 0x92
  db 0xfc
  db 0x00
  

    KERNEL_STACK_SIZE equ 16384; size of stack in bytes
    section .bss
    align 4                                     ; align at 4 bytes
    kernel_stack:                               ; label points to beginning of memory
        resb KERNEL_STACK_SIZE
    section .fini
    end_of_mem:


