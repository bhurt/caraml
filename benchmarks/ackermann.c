#include <stdio.h>

static long ackermann(long m, long n) {
    if (m == 0) {
        return n+1;
    } else if (n == 0) {
        return ackermann(m-1, 1);
    } else {
        return ackermann(m-1, ackermann(m, n-1));
    }
}

int main(void) {
    printf("%ld\n", ackermann(3L, 14L));
    return 0;
}

