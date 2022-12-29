#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "err.h"

const char message[] = "Hello from your parent!";

int main(void)
{
    const char* file_name = "/tmp/fifo_tmp";

    ASSERT_SYS_OK(mkfifo(file_name, 0755));

    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if (!pid) {
        ASSERT_SYS_OK(execl("./child-fifo", "./child-fifo", file_name, NULL));
    }

    printf("Parent: opening fifo.\n");
    int desc = open(file_name, O_WRONLY);
    ASSERT_SYS_OK(desc);

    // Write to fifo (excluding null character at the end).
    ssize_t wrote = write(desc, message, strlen(message));
    ASSERT_SYS_OK(wrote);
    printf("Parent: wrote %zd byte(s) and waiting.\n", wrote);

    ASSERT_SYS_OK(wait(NULL));
    ASSERT_SYS_OK(close(desc));
    ASSERT_SYS_OK(unlink(file_name));

    return 0;
}
