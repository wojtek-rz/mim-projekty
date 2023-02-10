#include <stdlib.h>
#include "queue.h"
#include "errors.h"
#include "types.h"

// simpler version of queue
// that expands dynamically when pushing elements to the end
// and moves the pointer to the first element when gets an element from the queue
// frees memory only when the last element is removed from the queue
typedef struct SimplerQueueTag {
    size_t length;
    size_t allocated_memory;
    size_t front;
    size_t *values;
} SimplerQueue;

SimplerQueue *create_queue() {
    SimplerQueue *q = (SimplerQueue *) malloc(sizeof(SimplerQueue));
    if (q == NULL) memory_allocation_error();

    q->length = q->front = q->allocated_memory = 0;
    return q;
}

void push_to_queue(SimplerQueue *q, size_t value) {
    if (q->allocated_memory == 0) {
        q->values = malloc(1 * sizeof(size_t));
        if (q->values == NULL) memory_allocation_error();
        q->allocated_memory = 1;
    }

    if (q->length >= q->allocated_memory) {
        q->values = realloc(q->values,
                            ALLOCATION_MULTIPLIER * q->allocated_memory * sizeof(size_t));
        if (q->values == NULL) memory_allocation_error();
        q->allocated_memory *= ALLOCATION_MULTIPLIER;
    }

    q->values[q->length] = value;
    q->length++;
}

size_t get_from_queue(SimplerQueue *q) {
    size_t response;

    // the element we are removing is the last one,
    // and we are freeing the memory
    if (q->front == q->length - 1) {
        response = q->values[q->front];
        free(q->values);

        q->allocated_memory = q->length = q->front = 0;
    }
    else {
        response = q->values[q->front];
        q->front++;
    }
    return response;
}

int is_queue_empty(SimplerQueue *q) {
    if (q->front == q->length) {
        return 1;
    }
    return 0;
}

void switch_queues(SimplerQueue **q1, SimplerQueue **q2) {
    SimplerQueue *q_tmp = *q1;
    *q1 = *q2;
    *q2 = q_tmp;
}

void delete_queue(SimplerQueue *q) {
    if (q->allocated_memory != 0) {
        free(q->values);
    }
    free(q);
}