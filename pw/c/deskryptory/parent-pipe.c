#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "err.h"

const char message[] = "Hello from your parent!";

int main(void)
{
    int pipe_dsc[2];
    ASSERT_SYS_OK(pipe(pipe_dsc));

    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if (!pid) {
        // Child process.
        // Close writing end of pipe.
        ASSERT_SYS_OK(close(pipe_dsc[1]));

        // Convert pipe's read descriptor number to c-string.
        char read_dsc_str[4];
        int ret = snprintf(read_dsc_str, sizeof(read_dsc_str), "%d", pipe_dsc[0]);
        if (ret < 0 || ret >= (int)sizeof(read_dsc_str))
            fatal("Error in snprintf.");

        ASSERT_SYS_OK(execl("./child-pipe", "./child-pipe", read_dsc_str, NULL));
    } else {
        // Parent process.
        // Close reading end of pipe.
        ASSERT_SYS_OK(close(pipe_dsc[0]));

        // Write to pipe (excluding null character at the end).
        ssize_t wrote = write(pipe_dsc[1], message, sizeof(message) - 1);
        ASSERT_SYS_OK(wrote);
        if (wrote != sizeof(message) - 1)
            fatal("Wrote less than expected: %zd out of %zu bytes.", wrote, sizeof(message) - 1);

        // Wait for child.
        ASSERT_SYS_OK(wait(NULL));
        return 0;
    }
}
