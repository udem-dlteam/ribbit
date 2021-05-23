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
endif

GCC_DEFS += -DPC
CFLAGS += -Os -fno-stack-protector -Wall -fomit-frame-pointer -ffreestanding -nostdlib -nostdinc -fno-pie -m16 -fno-asynchronous-unwind-tables
AS_FLAGS += --32
LDFLAGS += --strip-all --discard-all -no-pie -static

build: bin_files

run: bin_files
	qemu-system-i386 -hda boot.bin

debug: bin_files
	qemu-system-i386 -s -S -hda boot.bin

clean:
	rm -rf *.bin *.o

bin_files: boot.bin

boot.bin: boot.o vm.o decode.o mem.o
	$(LD) $(LDFLAGS) boot.o vm.o decode.o -o boot.bin.elf -T link.ld --omagic -m elf_i386 --entry=boot
	objcopy -x --remove-section .note --remove-section .comment boot.bin.elf
	objcopy -SO binary boot.bin.elf boot.bin

boot.o: boot.s
	$(AS) $(AS_FLAGS) -o $*.o -s $*.s

vm.o: vm.c
	$(GCC) $(GCC_DEFS) $(CFLAGS) -c $*.c -o $*.o

mem.o: mem.c
	$(GCC) $(GCC_DEFS) $(CFLAGS) -c $*.c -o $*.o

decode.o: decode.c
	$(GCC) $(GCC_DEFS) $(CFLAGS) -c $*.c -o $*.o

utils.o: utils.c
	$(GCC) $(GCC_DEFS) $(CFLAGS) -c $*.c -o $*.o
