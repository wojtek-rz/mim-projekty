#ifndef IPP_MALE_ARRAY_H
#define IPP_MALE_ARRAY_H

#include <stdio.h>

struct ArrayTag;

typedef struct ArrayTag Array;

extern Array *create_array(size_t size);

extern void set_in_array(Array *arr, size_t elem, size_t index);

extern void set_at_the_end(Array *arr, size_t elem);

extern size_t get_from_array(Array *arr, size_t i);

extern void delete_array(Array *arr);

extern size_t get_array_length(Array *arr);

extern size_t multiply_array(Array *arr);

#endif //IPP_MALE_ARRAY_H