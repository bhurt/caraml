
#include <stdio.h>

long print_int(long x) {
    printf("%ld", x);
    return 0;
}

long print_newline(long x) {
    x = x;
    printf("\n");
    fflush(stdout);
    return 0;
}

long caraml_get_tagword(long * ptr) {
    register int sp = 0;
    register long tagword = ptr[-1];
    long *(stack[10]);

    do {
        if (sp == 10) {
            long prev_val = (long) ptr;
            while (sp > 0) {
                long * ptr2;
                sp -= 1;
                ptr2 = stack[sp];
                __sync_bool_compare_and_swap(ptr2 - 1, prev_val, tagword);
                prev_val = (long) ptr2;
            }
        }
        stack[sp] = ptr;
        sp += 1;
        ptr = (long *) tagword;
        tagword = ptr[-1];
    } while ((tagword & 1L) == 0L);

    do {
        long prev_val = (long) ptr;
        while (sp > 0) {
            long * ptr2;
            sp -= 1;
            ptr2 = stack[sp];
            __sync_bool_compare_and_swap(ptr2 - 1, prev_val, (long) ptr);
            prev_val = (long) ptr2;
        }
    } while (0);

    return tagword;
}

    
