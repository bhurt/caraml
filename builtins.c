
#include <stdio.h>

void got_here() {
    printf("Got here!\n");
}

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

