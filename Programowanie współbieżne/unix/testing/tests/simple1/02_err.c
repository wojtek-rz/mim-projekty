#include <unistd.h>
#include <stdio.h>

#define SCHEDULE_N 10

const int period = 100000;

int main() {
    fprintf(stderr, "Not the last line.\n");
    fprintf(stderr, "Last line.\n");
    return 0;
}