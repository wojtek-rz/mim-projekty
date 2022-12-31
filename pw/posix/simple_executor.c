#include "err.h"
#include "utils.h"
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(){

    int pipe_dsc[2];
    ASSERT_SYS_OK(pipe(pipe_dsc));

    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if(!pid) {
        // Close read descriptors.
        ASSERT_SYS_OK(close(pipe_dsc[0]));

        ASSERT_SYS_OK(dup2(pipe_dsc[1], STDOUT_FILENO));
        ASSERT_SYS_OK(close(pipe_dsc[1]));

        ASSERT_SYS_OK(execlp("./task1", "./task1", NULL));
    } else {
        // Close write descriptors.
        ASSERT_SYS_OK(close(pipe_dsc[1]));

        FILE *stream = fdopen(pipe_dsc[0], "r");

        char *line = malloc(1024);
        size_t n = 1024;
        long code;
        while ((code = getline(&line, &n, stream)) != -1){
                printf("Ended code %ld\n", code);
                printf("Task wrote: %s.", line);
        };

    }
}