#include <stdio.h>
#include <unistd.h>
#include <time.h>

int main() {

    printf("Line 1\n");
    fflush(stdout);
    sleep(2);

    printf("Line 2\n");
    fflush(stdout);
    sleep(2);

    printf("Line 3\n");
    fflush(stdout);
    sleep(2);

    printf("Line 4");
    fflush(stdout);

    return 0;
}