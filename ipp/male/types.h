#ifndef IPP_MALE_TYPES_H
#define IPP_MALE_TYPES_H

#include "array.h"
#include "bitvectors.h"

#define FUNCTION_FAILURE 1
#define FUNCTION_SUCCESS 0
#define NO_WAY_CODE -1
#define ALLOCATION_MULTIPLIER 2

typedef struct LabyrinthTag {
    Bitvector *walls;
    Array *dimensions;
    Array *start_position;
    Array *end_position;
} Labyrinth;

#endif //IPP_MALE_TYPES_H
