#include <stdlib.h>
#include "array.h"
#include "errors.h"
#include "types.h"

struct ArrayTag {
    size_t length;
    size_t allocated_memory;
    size_t *values;
};

Array *create_array(size_t size) {
    Array *tab = (Array *) malloc(sizeof(Array));
    if (tab == NULL) memory_allocation_error();

    // allocating memory for one cell in array
    tab->values = (size_t *) malloc((size + 1) * sizeof(size_t));
    if (tab->values == NULL) memory_allocation_error();

    tab->length = 0;
    tab->allocated_memory = (size + 1);
    return tab;
}

void set_in_array(Array *arr, size_t elem, size_t index) {
    // extending the allocated memory to fit new element with index 'index'
    while (arr->allocated_memory <= index) {
        arr->values = (size_t *) realloc(arr->values,
                                         ALLOCATION_MULTIPLIER * arr->allocated_memory * sizeof(size_t));
        if (arr->values == NULL) memory_allocation_error();

        arr->allocated_memory *= ALLOCATION_MULTIPLIER;
    }

    arr->values[index] = elem;
    arr->length = index + 1;
}

inline void set_at_the_end(Array *arr, size_t elem) {
    set_in_array(arr, elem, arr->length);
}

inline size_t get_from_array(Array *arr, size_t i) {
    return arr->values[i];
}

inline size_t get_array_length(Array *arr) {
    return arr->length;
}

void delete_array(Array *arr) {
    free(arr->values);
    free(arr);
}

size_t multiply_array(Array *arr) {
    size_t result = 1, prev_product = 1, product;
    for (size_t i = 0; i < arr->length; i++) {
        // checking for overflow, when: a * b = c, it should also be: c / b = a
        product = prev_product * arr->values[i];
        if (product / prev_product != arr->values[i]) {
            memory_allocation_error();
        }
        prev_product = product;

        result *= arr->values[i];
    }
    return result;
}