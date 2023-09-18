#include <unistd.h>
#include <stdio.h>

// Task will wait 500ms and then exit.
// In test we will sleep 1000ms and then print 'task ended' communicates before processing next user commands.

const int period = 100000; // 100 milliseconds

int main() {
    fprintf(stdout, "should be written last\n");
    fflush(stdout);

    usleep(period * 5);

    return 0;
}