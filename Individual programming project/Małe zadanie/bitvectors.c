#include <stdlib.h>
#include <stdio.h>
#include "bitvectors.h"
#include "errors.h"

#define MAX_UCHAR 255
#define START_MALLOC_SIZE 1

typedef unsigned char uchar;

Bitvector *create_bitvector() {
    Bitvector *created_bv = (Bitvector *) malloc(sizeof(Bitvector));
    if (created_bv == NULL) memory_allocation_error();

    // Allocating 1 element Array
    created_bv->vector = (uchar *) malloc(sizeof(uchar) * START_MALLOC_SIZE);
    if (created_bv->vector == NULL) memory_allocation_error();

    created_bv->length = 0;
    return created_bv;
}

static uchar set_nth_bit_of_number(uchar char_number, size_t n, size_t bit_value) {
    size_t number = (size_t) char_number;
    // setting nth bit to 0
    number = number & (MAX_UCHAR - (1 << n));
    // setting nth bit to 'bit_value'
    number = number | (bit_value << n);
    return (uchar) number;
}

static int get_nth_bit_from_number(uchar char_number, size_t n) {
    size_t number = (size_t) char_number;
    // setting all bits but nth to 0
    number = number & (1 << n);
    // moving nth bit to the begging
    number = number >> n;
    return (int) (number);
}

void set_bit_in_bitvector(Bitvector *b_vector, size_t pos_of_value, int value) {
    // invalid options
    if ((value != 0 && value != 1) || pos_of_value > b_vector->length) {
        return;
    }

    size_t mod_8 = pos_of_value & 7; // modulo 8
    size_t vector_pos = (pos_of_value >> 3); // division by 8

    b_vector->vector[vector_pos] = set_nth_bit_of_number(b_vector->vector[vector_pos], mod_8, value);
}

int get_bit_from_bitvector(Bitvector *b_vector, size_t pos_of_value) {
    if (pos_of_value > b_vector->length) return 0;
    size_t mod_8 = pos_of_value & 7; // modulo 8
    size_t vector_pos = (pos_of_value >> 3); // division by 8
    return get_nth_bit_from_number(b_vector->vector[vector_pos], mod_8);
}

void set_bitvector_to_zeros(Bitvector *created_bitvector) {
    size_t len = (created_bitvector->length >> 3); // division by 8
    for (size_t i = 0; i < len + 1; i++) {
        created_bitvector->vector[i] = 0;
    }
}

void allocate_memory_to_bitvector(Bitvector *created_bitvector, size_t size) {
    created_bitvector->vector = (uchar *) realloc(created_bitvector->vector, sizeof(uchar) * ((size + 8) / 8));
    if (created_bitvector->vector == NULL) memory_allocation_error();

    created_bitvector->length = size;
}

void delete_bitvector(Bitvector *created_bitvector) {
    free(created_bitvector->vector);
    free(created_bitvector);
}