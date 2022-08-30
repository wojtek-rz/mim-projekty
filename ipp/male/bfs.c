#include <stddef.h>
#include <inttypes.h>
#include "bfs.h"
#include "array.h"
#include "bitvectors.h"
#include "queue.h"
#include "types.h"

size_t get_hash_from_coordinates(Array *coordinates, Array *dimensions) {
    size_t hash = 0, product = 1;
    for (size_t i = 0; i < get_array_length(dimensions); i++) {
        // for given i: product = n_1 * n_2 * ... * n_{i-1}
        // hash += (z_i - 1) * product
        hash += (get_from_array(coordinates, i) - 1) * product;
        product *= get_from_array(dimensions, i);
    }
    return hash;
}

static void add_child_states_to_queue(SimplerQueue *q, size_t current_state_hash, Bitvector *walls, Array *dimensions) {
    // if the coordinates are z_1, ... z_n and dimensions are n_1, ..., n_n
    // hash = (z_1 - 1) + (z_2 - 1) * n_1 + (z_3 - 1) * n_1 * n_2 + ... + (z_n - 1) * n_1 ... * n_{n-1}
    size_t child_state, product = 1;
    size_t state_divided = current_state_hash;

    for (size_t i = 0; i < get_array_length(dimensions); i++) {
        // for given i: state_divided = hash / (n_1 * n_2 * n_3 ... * n_{i-1})
        // and product = (n_1 * n_2 ... * n_{i-1})
        state_divided /= get_from_array(dimensions, i);

        // changing the i-th dimension by adding 1 to z_i
        child_state = current_state_hash + product;
        // checking whether (i+1)-th coordinate wasn't changed (we can't move through the edges of the labyrinth)
        if (child_state / (product * get_from_array(dimensions, i)) == state_divided) {
            if (get_bit_from_bitvector(walls, child_state) == 0) {
                push_to_queue(q, child_state);
                set_bit_in_bitvector(walls, child_state, 1);
            }
        }
        child_state = current_state_hash - product;
        if (child_state / (product * get_from_array(dimensions, i)) == state_divided) {
            if (get_bit_from_bitvector(walls, child_state) == 0) {
                push_to_queue(q, child_state);
                set_bit_in_bitvector(walls, child_state, 1);
            }
        }
        product *= get_from_array(dimensions, i);
    }
}

int64_t find_shortest_path(Labyrinth data) {
    Array *start_pos = data.start_position;
    Array *end_pos = data.end_position;
    Array *dimensions = data.dimensions;
    Bitvector *walls = data.walls;
    int64_t depth = 0;

    size_t current_state = get_hash_from_coordinates(start_pos, dimensions);
    size_t end_state = get_hash_from_coordinates(end_pos, dimensions);

    SimplerQueue *queue1 = create_queue();
    SimplerQueue *queue2 = create_queue();

    set_bit_in_bitvector(walls, current_state, 1);
    push_to_queue(queue1, current_state);

    while (!is_queue_empty(queue1)) {
        while (!is_queue_empty(queue1)) {
            current_state = get_from_queue(queue1);
            if (current_state == end_state) {
                delete_queue(queue1);
                delete_queue(queue2);
                return (int64_t) depth;
            }
            add_child_states_to_queue(queue2, current_state, walls, dimensions);
        }
        depth++;
        switch_queues(&queue1, &queue2);
    }

    delete_queue(queue1);
    delete_queue(queue2);
    return NO_WAY_CODE;
}