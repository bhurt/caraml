#include <stddef.h>
#include <sys/mman.h>

long * caraml_base = NULL;
long * caraml_limit = NULL;

#define PAGE_SIZE ((size_t) (0x10000ul))

static void alloc_page() {
    void * ptr = mmap(NULL, PAGE_SIZE, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS, -1, (off_t) 0);
    caraml_limit = (long *) ptr;
    caraml_base = ((long *) ptr) + (PAGE_SIZE / sizeof(long));
}

void caraml_gc(long nwords) {
    nwords = nwords; /* Not used currently */
    alloc_page();
}

void caraml_gc_init() {
    alloc_page();
}


