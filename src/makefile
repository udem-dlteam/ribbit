# Sector Scheme

GCC=/usr/local/Cellar/x86_64-elf-gcc/11.1.0/bin/x86_64-elf-gcc
AS=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-as

CFLAGS += -g -Os -fno-stack-protector -Wall -fomit-frame-pointer -ffreestanding -nostdlib -nostdinc -fno-pie -m16
AS_FLAGS += --32
LDFLAGS +=

build: bin_files

run: bin_files
	qemu-system-i386 boot.bin

debug: bin_files
	qemu-system-i386 -s -S boot.bin

clean:
	rm -rf *.bin *.o

bin_files: boot.bin

boot.bin: boot.o vm.o
	$(LD) $(LDFLAGS) boot.o vm.o -o boot.bin -T link.ld --omagic -m elf_i386 --entry=boot --oformat binary

boot.o: boot.s
	$(AS) $(AS_FLAGS) -o $*.o -s $*.s

vm.o: vm.c
	$(GCC) $(CFLAGS) -c $*.c -o $*.o
