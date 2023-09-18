#include <stdlib.h>
#include <stdio.h>
#include "errors.h"

void memory_allocation_error() {
    fprintf(stderr, "ERROR 0\n");
    exit(1);
}

void wrong_input_error(int line) {
    fprintf(stderr, "ERROR %d\n", line);
}