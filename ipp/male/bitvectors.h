#ifndef IPP_MALE_BITVECTORS_H
#define IPP_MALE_BITVECTORS_H

#include <stdlib.h>

typedef struct BitvectorTag {
    size_t length;
    unsigned char *vector;
} Bitvector;

extern Bitvector *create_bitvector();

extern void delete_bitvector(Bitvector *created_bitvector);

extern void set_bit_in_bitvector(Bitvector *b_vector, size_t pos_of_value, int value);

extern int get_bit_from_bitvector(Bitvector *b_vector, size_t pos_of_value);

extern void set_bitvector_to_zeros(Bitvector *created_bitvector);

extern void allocate_memory_to_bitvector(Bitvector *created_bitvector, size_t size);

#endif //IPP_MALE_BITVECTORS_H
