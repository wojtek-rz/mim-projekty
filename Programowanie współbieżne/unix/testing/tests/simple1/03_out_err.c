#include <unistd.h>
#include <stdio.h>

#define N 10

const int period = 100000; // 100 milliseconds

int main() {
    fprintf(stderr, "err line %d\n", 1);
    fflush(stderr);
    fprintf(stdout, "out line %d\n", 1);
    fflush(stdout);

    fprintf(stderr, "err line %d\n", 2);
    fflush(stderr);
    fprintf(stdout, "out line %d\n", 2);
    fflush(stdout);

    return 0;
}