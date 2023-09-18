#include <stdio.h>
#include <stdint.h>
#include "array.h"
#include "input.h"
#include "bitvectors.h"
#include "bfs.h"
#include "types.h"

Labyrinth generate_structures() {
    Labyrinth data;
    data.walls = create_bitvector();
    data.dimensions = create_array(0);
    data.start_position = create_array(0);
    data.end_position = create_array(0);
    return data;
}

void clean_memory(Labyrinth data) {
    delete_array(data.dimensions);
    delete_array(data.start_position);
    delete_array(data.end_position);
    delete_bitvector(data.walls);
}

int main() {
    Labyrinth data = generate_structures();
    int exitcode;

    if (read_input(data) == FUNCTION_FAILURE) {
        exitcode = 1;
    }
    else {
        int64_t ans = find_shortest_path(data);

        if (ans == NO_WAY_CODE) {
            printf("NO WAY\n");
        }
        else {
            printf("%zu\n", ans);
        }
        exitcode = 0;
    }

    clean_memory(data);
    return exitcode;
}