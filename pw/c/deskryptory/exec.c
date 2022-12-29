#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "err.h"

int main(void)
{
    pid_t pid;
    ASSERT_SYS_OK(pid = fork());
    if (!pid) {
        // Child process
        printf("Child: my pid is %d, my parent's pid is %d\n", getpid(), getppid());

        // Replace the current process image to execute the "ps" command.
        ASSERT_SYS_OK(execlp("ps", "ps", NULL));
    } else {
        // Parent process
        printf("Parent: my pid is %d, my child's pid is %d\n", getpid(), pid);

        // Wait for child.
        ASSERT_SYS_OK(wait(NULL));

        return 0;
    }
}
