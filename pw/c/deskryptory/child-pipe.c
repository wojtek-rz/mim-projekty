#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "err.h"

int main(int argc, char* argv[])
{
    // Parse args.
    if (argc != 2)
        fatal("Usage: %s <read_fd>\n", argv[0]);
    int read_dsc = atoi(argv[1]);

    printf("Child: Reading data from descriptor %d\n", read_dsc);

    // Use the raw 'read()' system function. It just reads bytes, which may
    // or may not contain a null character. To make a null-terminated string,
    // we'll append the null character. Hence we read at most sizeof(buf) - 1.
    char buf[1024];
    ssize_t read_len = read(read_dsc, buf, sizeof(buf) - 1);
    ASSERT_SYS_OK(read_len);
    buf[read_len] = '\0';

    if (read_len == 0)
        printf("Child: We read nothing, end-of-file.\n");
    else
        printf("Child: We read %zd byte(s): '%s'\n", read_len, buf);

    return 0;
}
