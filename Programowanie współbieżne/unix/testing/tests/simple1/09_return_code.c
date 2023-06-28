#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    // Check if a command line argument was provided
    if (argc < 2) {
        usleep(100);
        return 0;
    }

    // Get the return code from the command line argument
    int return_code = atoi(argv[1]);
    printf("%d\n", return_code);

    // Return the specified return code
    return return_code;
}
