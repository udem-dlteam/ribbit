# Makefile to Compile the C VM as the smallest possible VM
UNAME = $(shell uname)
OUT=vm.o
INT=vm.o.unstripped
SRC=vm.c

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

GCC_DEFS =
CFLAGS = -m32 \
		 -ffunction-sections -fdata-sections -fno-plt -fno-stack-protector \
		 -fno-stack-check -fno-unwind-tables -flto -fno-stack-protector \
		 -fno-stack-check -Os \
		 -Wl,--build-id=none -z norelro -Wl,-z,noseparate-code \
		 -Wl,--no-eh-frame-hdr -no-pie -Wl,--no-ld-generated-unwind-info \
		 -Wl,--hash-style=sysv -Wl,-z,nodynamic-undefined-weak \
		 -Wl,--gc-sections -fno-exceptions -fno-rtti -fno-threadsafe-statics \
		 -fno-asynchronous-unwind-tables -fomit-frame-pointer -fno-pic -fno-PIE -fno-PIC -ffast-math -s

all: $(OUT)

clean:
	rm -f $(OUT)
	rm -f $(INT)

$(OUT): $(INT)
	$(OBJC) -S --remove-section .comment --remove-section .note --remove-section .hash --remove-section .eh_frame $(INT) $(OUT)

$(INT): $(SRC)
	$(GCC) vm.c $(CFLAGS) -o $(INT)

