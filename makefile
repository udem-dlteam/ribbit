# Sector Scheme

CFLAGS += -g -O3 -fno-stack-protector -Wall -fomit-frame-pointer -ffreestanding -nostdlib -nostdinc -fno-pie
LDFLAGS +=

build: bin_files

run: bin_files
	qemu-system-i386 boot.bin

debug: bin_files
	qemu-system-i386 -s -S boot.bin

clean:
	rm *.bin *.o

bin_files: boot.bin

boot.bin: boot.o vm.o
	$(LD) $(LDFLAGS) boot.o vm.o -o boot.bin -T link.ld --omagic -m elf_i386 --entry=boot --oformat binary

boot.o: boot.s
	as --32 -o $*.o -s $*.s

vm.o: vm.c
	gcc $(CFLAGS) -m32 -c vm.c -o vm.o
