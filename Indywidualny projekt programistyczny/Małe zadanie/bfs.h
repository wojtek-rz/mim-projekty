#ifndef IPP_MALE_BFS_H
#define IPP_MALE_BFS_H

#include <inttypes.h>
#include "array.h"
#include "bitvectors.h"
#include "types.h"

extern int64_t find_shortest_path(Labyrinth data);

extern size_t get_hash_from_coordinates(Array *coordinates, Array *dimensions);

#endif //IPP_MALE_BFS_H
