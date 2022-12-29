#include "utils.h"
#include "err.h"
#include <unistd.h>

#DEFINE MAX_COMMAND_LENGTH 512

int main(){
    char buff[MAX_COMMAND_LENGTH];

    while (read_line(buff, MAX_COMMAND_LENGTH, STDIN_FILENO)){
        printf("%s", buff);
    }




    return 0;
}