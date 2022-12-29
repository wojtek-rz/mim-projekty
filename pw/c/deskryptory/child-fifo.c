#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "err.h"

int main(int argc, char* argv[])
{
    // Parse args.
    if (argc != 2)
        fatal("Usage: %s <fifo-name>\n", argv[0]);
    const char* fifo_name = argv[1];

    printf("Child: trying to open fifo: %s\n", fifo_name);
    int desc = open(fifo_name, O_RDWR);
    ASSERT_SYS_OK(desc);

    printf("Child: reading data from descriptor %d\n", desc);
    char buf[1024];
    ssize_t read_len = read(desc, buf, sizeof(buf) - 1);
    ASSERT_SYS_OK(read_len);
    buf[read_len] = '\0';

    if (read_len == 0)
        printf("Child: We read nothing, end-of-file.\n");
    else
        printf("Child: read %zd byte(s): '%s'\n", read_len, buf);

    ASSERT_SYS_OK(close(desc));

    return 0;
}
