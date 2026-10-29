#include "kernel.h"
#include "multiboot.h"
#include <stdarg.h>
#define DEFAULT_COLOR 0xf0
#define IS_VALID_MEMORY_MAP (1 << 6)
#define ASSEMBLE_64BIT(low,high) ( ((multiboot_uint64_t)low) | ((multiboot_uint64_t)high << 32) )
void vga_write(char c, uint8_t color, int index) {
  uint16_t *vga = (uint16_t *)0xb8000;
  vga[index] = c | color << 8;
}
int errno = 0;
int position = 0;
char kernel_heap[1024 * 1024];
static char scancode_to_ascii[128] =
    "&&1234567890-=\b\tqwertyuiop[]\n&asdfghjkl;'`\xff\\zxcvbnm,./\xff*& ";
static char scancode_to_ascii_shift[128] =
    "&&!@#$%^&*()_+\b\tQWERTYUIOP{}\n&ASDFGHJKL:\"~\xff|ZXCVBNM<>?\xff*& ";
struct heap_chunk *heap = (struct heap_chunk*)kernel_heap;
struct stack_frame *last_frame = NULL;

struct idt idt;
struct gate_descriptor gates[256];
typedef void (*irq_handler)();
irq_handler handlers[256] = {0};
struct stack irqs;
struct interrupt_frame;

void enable_cursor(uint8_t cursor_start, uint8_t cursor_end) {
  outw(0x3D4, 0x0A);
  outw(0x3D5, (inb(0x3D5) & 0xC0) | cursor_start);
  outw(0x3D4, 0x0B);
  outw(0x3D5, (inb(0x3D5) & 0xE0) | cursor_end);
}

void update_cursor(uint16_t pos) {
  outw(0x3D4, 0x0F);
  outw(0x3D5, (uint8_t)(pos & 0xFF));
  outw(0x3D4, 0x0E);
  outw(0x3D5, (uint8_t)((pos >> 8) & 0xFF));
}

void show_stack_trace() {
  struct stack_frame* li = __builtin_frame_address(0);
  puts("segfault in");
  while (li != NULL) {
    printf("eip:0x%x ebp:0x%x\n",li->eip,li->ebp);
    li = li->ebp;
  }
  for (;;);
}

void kernel_main(multiboot_info_t *info, unsigned magic,void* end) {
  enable_cursor(0,15);
  if (magic != MULTIBOOT_BOOTLOADER_MAGIC) {
    printf("wrong magic %x\n",magic);
    exit(-1);
  }
  if (!(info->flags & IS_VALID_MEMORY_MAP)) {
    printf("invalid memory map.\n");
    exit(-1);
  }
  heap->next = NULL;
  heap->size = sizeof(kernel_heap);
  struct heap_chunk* local = heap;
  for (int i=0;i < info->mmap_length;i += sizeof(multiboot_memory_map_t)) {
    multiboot_memory_map_t *map = (void*)(info->mmap_addr + i);
    if (map->type == MULTIBOOT_MEMORY_AVAILABLE) {
      if ((void*)(map->addr_low + map->len_low) > end) {
        if ((void*)map->addr_low >= end) {
           local->next = (void*)map->addr_low; 
           local->next->size = map->len_low;
           local->next->next = NULL;
           local = local->next; 
        } else {
           local->next = (void*)end; 
           local->next->size = map->addr_low + map->len_low - (multiboot_uint32_t)end;
           local->next->next = NULL;
           local = local->next; 
        }
      }
    }
  }
  idt.offset = gates;
  idt.size = 256;
  for (int i=0;i < 256;i++) {
    gates[i].present = 1;
    gates[i].zero = 0;
    gates[i].reserved_zero = 0;
    gates[i].selector = 0x08;
    gates[i].gate_type = 0xe;
    gates[i].dpl = 0;
    gates[i].offset_1 = (uint32_t)irq_handlers[i] & 0x0000ffff;
    gates[i].offset_2 = ((uint32_t)irq_handlers[i] & 0xffff0000) >> 16;
  }
  lidt(&idt);
//  printf("%d\n",magic/(magic - magic));
  init();
}

void handle_irq(const int number,const int has_code,const int code)
{
  if (irqs.i >= sizeof(irqs.data)/sizeof(irqs.data[0])-1) {
    printf("unhandled irq %d\n",number);
    return;
  }
  if (has_code)
    printf("irq %d with code %d\n",number,code);
  else
    printf("irq %d with no code\n",number);
  if (number == 13) show_stack_trace();
  irqs.data[irqs.i] = number;
  irqs.i++;
}

void *malloc(size_t size) {
  static int heap_lock = 0;
  if (heap_lock == 1) return NULL;
  else heap_lock = 1;
  struct heap_chunk *local = heap;
  struct heap_chunk *prev = heap;
  while (local != NULL) {
    if (local->size > size) {
      if (local->size - size > 0x20) {
        struct heap_chunk *fragment = (void *)(local) + size;
        prev->next = fragment;
        fragment->next = local->next;
        fragment->size = local->size - size;
        local->size = size;
        heap_lock = 0; 
        return (void *)(local) + sizeof(local->size);
      } else {
        prev->next = local->next;
        heap_lock = 0; 
        return (void *)(local) + sizeof(local->size);
      }
    }
    prev = local;
    local = local->next;
  }
  //  printf("malloc failed: request size:%d\n", size);
  heap_lock = 0; 
  return NULL;
}

void free(void *a) {
  
  struct heap_chunk *new = (void *)(a - sizeof(heap->size));
  struct heap_chunk *local = heap;
  struct heap_chunk *prev = heap;
  while (local != NULL) {
    if ((void *)(local) + local->size == new) {
      local->size += new->size;
      return;
    }
    if ((void *)(new) + new->size == local) {
      prev->next = new;
      new->next = local->next;
      new->size += local->size;
      return;
    }
    prev = local;
    local = local->next;
  }
  prev->next = new;
  
}

void exit(int code) {
  puts("\nexit");
  while (1)
    ;
}

int putchar(char c) {
  if (position == 80 * 25) {
    uint16_t *vga = (uint16_t *)0xb8000;
    for (int i = 80; i < 80 * 25; i++)
      vga[i - 80] = vga[i];
    for (int i = 80 * 24; i < 80 * 25; i++)
      vga_write('c', 0x00, i);
    position -= 80;
  }
  switch (c) {
  case '\n':
    position += 80 - (position % 80);
    break;
  default:
    vga_write(c, DEFAULT_COLOR, position);
    position++;
  }
  update_cursor(position);
  return 1;
}

size_t read(char *txt, size_t size) {
  
  static int shift = 0;
  for (int i = 0; i < size; i++) {
    short c = receive_scancode();
    if (c == 0x2a) {
      shift = 1;
      i--;
      continue;
    } else if (c == 0xaa) {
      shift = 0;
      i--;
      continue;
    } else if (c > 127 || c < 0) {
      i--;
      continue;
    }
    if (shift) {
      txt[i] = scancode_to_ascii_shift[c];
    } else {
      txt[i] = scancode_to_ascii[c];
    }
//    putchar(txt[i]);
  }
  
  return size;
}

void c_irq() {
  puts("irq");
}
