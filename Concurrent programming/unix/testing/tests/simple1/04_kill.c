#include <unistd.h>
#include <stdio.h>

const int period = 100000; // 100 milliseconds

int main() {
    fprintf(stdout, "line 1\n");
    fflush(stdout);

    usleep(period * 10);

    return 0;
}