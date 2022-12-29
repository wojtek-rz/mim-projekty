#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "err.h"
#include "pipeline-utils.h"

int main(void)
{
    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if (!pid) {
        // Close all non-standard inherited file descriptors (like those leaked by VS Code).
        // There's no easy way to go over all open descriptors,
        // so just try to close a lot and ignore errors.
        for (int i = 3; i < 256; ++i)
            close(i);

        ASSERT_SYS_OK(execlp("./pipeline", "./pipeline", "ls", "head", "tail", "wc", NULL));
    }

    ASSERT_SYS_OK(wait(NULL));

    return 0;
}
