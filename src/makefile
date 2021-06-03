# Sector Scheme Makefile
#
# On MacOS, the makefile builds using gcc installed
# by brew.

UNAME = $(shell uname)

ifeq ($(UNAME), Darwin)
LD=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-ld
GCC=/usr/local/Cellar/x86_64-elf-gcc/11.1.0/bin/x86_64-elf-gcc
AS=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-as
else
LD=ld
GCC=gcc
AS=as
NASM=nasm
endif

GCC_DEFS += -DPC
CFLAGS += -g -Os -fno-stack-protector -Wall -fomit-frame-pointer -ffreestanding -nostdlib -nostdinc -fno-pie -m16 -fno-asynchronous-unwind-tables
AS_FLAGS += --32
LDFLAGS += --strip-all --discard-all -no-pie -static

build: bin_files

run: bin_files
	qemu-system-i386 -hda boot.bin

debug: bin_files
	qemu-system-i386 -s -S -hda boot.bin

attach:
	gdb boot.bin.elf \
		-ex 'target remote localhost:1234' \
        -ex 'set architecture i8086' \
				-ex 'layout asm' \
        -ex 'continue'

clean:
	rm -rf *.bin *.o *.elf *.elf.stripped

bin_files: boot.bin

boot.bin: boot.o mem.o stdlib.o mem_test.o clumps.o dispatcher.o
	$(LD) $(LDFLAGS) boot.o mem.o stdlib.o mem_test.o clumps.o dispatcher.o -o boot.bin.elf -T link.ld --omagic -m elf_i386 --entry=boot
	objcopy -x --remove-section .note --remove-section .comment boot.bin.elf boot.bin.elf.stripped
	objcopy -SO binary boot.bin.elf.stripped boot.bin

boot.o: boot.s
	$(AS) $(AS_FLAGS) -o $*.o -s $*.s

mem.o: mem.asm
	$(NASM) $*.asm -f elf32 -o $*.o

utils.o: utils.c
	$(GCC) $(GCC_DEFS) $(CFLAGS) -c $*.c -o $*.o

clumps.o: clumps.asm
	$(NASM) $*.asm -f elf32 -o $*.o

dispatcher.o: dispatcher.asm
	$(NASM) $*.asm -f elf32 -o $*.o

mem_test.o: mem_test.asm
	$(NASM) $*.asm -f elf32 -o $*.o

stdlib.o: stdlib.c
	$(GCC) $(GCC_DEFS) $(CFLAGS) -c $*.c -o $*.o
