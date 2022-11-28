#ifndef HASH_H
#define HASH_H

#ifdef __cplusplus

#include <cstdint>
#include <cstdlib>
// We force include iostream to make sure that streams are initialized before main.
#include <iostream>

namespace jnp1 {
    extern "C" {

#else

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

#endif
        typedef uint64_t (*hash_function_t)(const uint64_t *, size_t);

        unsigned long hash_create(hash_function_t hash_function);

        void hash_delete(unsigned long id);

        size_t hash_size(unsigned long id);

        bool hash_insert(unsigned long id, uint64_t const * seq, size_t size);

        bool hash_remove(unsigned long id, uint64_t const * seq, size_t size);

        void hash_clear(unsigned long id);

        bool hash_test(unsigned long id, uint64_t const * seq, size_t size);

#ifdef __cplusplus
    } // extern "C"
} // jnp1
#endif

#endif //HASH_H