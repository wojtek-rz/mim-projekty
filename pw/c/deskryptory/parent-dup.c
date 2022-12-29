#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "err.h"

const char message[] = "Hello from your parent!\n";

int main(int argc, char* argv[])
{
    // Parse args.
    if (argc < 2)
        fatal("Usage: %s program_name [...]\n", argv[0]);
    const char* program_name = argv[1];
    char** program_args = &argv[1];  // Same array as argv, but without argv[0].

    int pipe_dsc[2];
    ASSERT_SYS_OK(pipe(pipe_dsc));

    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if (!pid) {
        // Close the write descriptor.
        ASSERT_SYS_OK(close(pipe_dsc[1]));

        // Replace the standard input with the pipe's reading end.
        printf("Child: replacing stdin descriptor %d with a copy of %d\n", STDIN_FILENO, pipe_dsc[0]);
        ASSERT_SYS_OK(dup2(pipe_dsc[0], STDIN_FILENO));
        ASSERT_SYS_OK(close(pipe_dsc[0]));  // Close the original copy.

        ASSERT_SYS_OK(execvp(program_name, program_args));
    } else {
        // Close the read descriptor.
        ASSERT_SYS_OK(close(pipe_dsc[0]));

        // Write to child (excluding null character).
        ssize_t wrote = write(pipe_dsc[1], message, sizeof(message) - 1);
        ASSERT_SYS_OK(wrote);
        if (wrote != sizeof(message) - 1)
            fatal("Wrote less than expected.");

        // Close the pipe - otherwise the child's read()s could block, waiting for more.
        ASSERT_SYS_OK(close(pipe_dsc[1]));

        // Wait for child (after closing the pipe, otherwise we would have a dead-lock).
        ASSERT_SYS_OK(wait(NULL));
        return 0;
    }
}
