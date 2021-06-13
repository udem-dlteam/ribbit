# Sector Scheme Makefile
#
# On MacOS, the makefile builds using gcc installed
# by brew.

UNAME = $(shell uname)

ifeq ($(UNAME), Darwin)
LD=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-ld
GCC=/usr/local/Cellar/x86_64-elf-gcc/11.1.0/bin/x86_64-elf-gcc
AS=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-as
OBJC=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-objcopy
NASM=nasm
else
LD=ld
GCC=gcc
AS=as
OBJC=objcopy
NASM=nasm
endif


GCC_DEFS += -DPC
CFLAGS += -g -Os -fno-stack-protector -Wall -fomit-frame-pointer -ffreestanding -nostdlib -nostdinc -fno-pie -m16 -fno-asynchronous-unwind-tables
AS_FLAGS += --32
LDFLAGS += --strip-all --discard-all -static

ifeq ($(UNAME), Darwin)
else
LDFLAGS += -no-pie
endif

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

boot.bin: boot.asm
	$(NASM) -f bin boot.asm -o boot.bin

stdlib.o: stdlib.c
	$(GCC) $(GCC_DEFS) $(CFLAGS) -c $*.c -o $*.o
