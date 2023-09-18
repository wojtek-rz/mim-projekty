#include <unistd.h>
#include <stdio.h>

#define SCHEDULE_N 10

const int period = 100000;

int main() {
    printf("Not the last line.\n");
    printf("Last line.\n");
    return 0;
}