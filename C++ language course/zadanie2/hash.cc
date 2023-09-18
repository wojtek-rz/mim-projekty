//
// Authors: Wojciech Rzepliński and Jan Wangrat
//

#include "hash.h"

#include <unordered_map>
#include <unordered_set>
#include <iostream>
#include <cassert>
#include <vector>

namespace {
#ifdef NDEBUG
    const bool debug = false;
#else
    const bool debug = true;
#endif
    // ================================= TYPES =================================

    struct custom_hash_t;
    using std::cout, std::endl;
    using jnp1::hash_function_t;
    using hash_t = uint64_t;
    using sequence_t = std::vector<uint64_t>;
    using set_t = std::unordered_set<sequence_t, custom_hash_t>;
    using sets_by_id_t = std::unordered_map<unsigned long, set_t>;

    struct custom_hash_t {
        hash_function_t hash_function;

        explicit custom_hash_t(hash_function_t hash_func) {
            hash_function = hash_func;
        }

        std::size_t operator()(sequence_t const &v) const {
            return hash_function(v.data(), v.size());
        }
    };
    // ================================= DECLARATIONS =================================
    // We use getter to avoid static initialization order fiasco.
    sets_by_id_t &get_sets() {
        static sets_by_id_t map;
        return map;
    }

    // ================================= DEBUG LOG =================================

    void cerr_seq(uint64_t const *seq, size_t size) {
        if (seq == nullptr) std::cerr << "NULL";
        else if (size == 0) std::cerr << "\"\"";
        else {
            std::cerr << "\"" << seq[0];
            for (size_t i = 1; i < size; i++) std::cerr << " " << seq[i];
            std::cerr << "\"";
        }
    }

    void cerr_func_input(const std::string &fun, unsigned long id) {
        std::cerr << fun << "(" << id << ")\n";
    }

    void cerr_func_input(const char *fun, unsigned long id, uint64_t const *seq, size_t size) {
        std::cerr << fun << "(" << id << ", ";
        cerr_seq(seq, size);
        std::cerr << ", " << size << ")\n";
    }

    inline void cerr_size_invalid(const std::string &fun) {
        std::cerr << fun << ": invalid size (0)\n";
    }

    inline void cerr_pointer_invalid(const std::string &fun) {
        std::cerr << fun << ": invalid pointer (NULL)\n";
    }

    inline void cerr_not_exists(const std::string &func_name, unsigned long id) {
        std::cerr << func_name << ": hash table #" << id << " does not exist\n";
    }

    bool validate_args(const std::string &fun, unsigned long id, sets_by_id_t::iterator iter) {
        bool is_present = iter != get_sets().end();
        if (!is_present) {
            if (debug) cerr_not_exists(fun, id);
            return false;
        }
        return true;
    }

    bool validate_args(const std::string &fun, unsigned long id, sets_by_id_t::iterator iter,
                       uint64_t const *seq, size_t size) {
        if (seq == nullptr || size == 0) {
            if (debug) {
                if (seq == nullptr) cerr_pointer_invalid(fun);
                if (size == 0) cerr_size_invalid(fun);
            }
            return false;
        }
        if (!validate_args(fun, id, iter)) return false;
        return true;
    }

    void cerr_hash_insert(const std::string &fun, unsigned long id, const uint64_t *seq,
                          size_t size, bool result) {
        if (result) {
            std::cerr << fun << ": hash table #" << id << ", sequence ";
            cerr_seq(seq, size);
            std::cerr << " inserted\n";
        } else {
            std::cerr << fun << ": hash table #" << id << ", sequence ";
            cerr_seq(seq, size);
            std::cerr << " was present\n";
        }
    }

    void cerr_hash_remove(const std::string &fun, unsigned long id, const uint64_t *seq,
                          size_t size, unsigned long removed) {
        if (removed == 0) {
            std::cerr << fun << ": hash table #" << id << ", sequence ";
            cerr_seq(seq, size);
            std::cerr << " was not present\n";
        } else {
            std::cerr << fun << ": hash table #" << id << ", sequence ";
            cerr_seq(seq, size);
            std::cerr << " removed\n";
        }
    }

    void cerr_hash_test(const std::string &fun, unsigned long id, const uint64_t *seq,
                        size_t size, bool count) {
        if (count) {
            std::cerr << fun << ": hash table #" << id << ", sequence ";
            cerr_seq(seq, size);
            std::cerr << " is present\n";
        } else {
            std::cerr << fun << ": hash table #" << id << ", sequence ";
            cerr_seq(seq, size);
            std::cerr << " is not present\n";
        }
    }
} /* anonymous namespace */

namespace jnp1 {
    // ================================= MODULE IMPLEMENTATION =================================
    unsigned long hash_create(hash_function_t hash_function) {
        if (debug) {
            std::cerr << __func__ << "(" << reinterpret_cast<void*>(hash_function) << ")\n";
            assert(hash_function != nullptr);
        }
        static size_t counter = 0;
        get_sets().insert({counter, set_t(0, custom_hash_t(hash_function))});

        if (debug) std::cerr << __func__ << ": hash table #" << counter << " created\n";

        return counter++;
    }

    void hash_delete(unsigned long id) {
        if (debug) cerr_func_input(__func__, id);
        if (!validate_args(__func__, id, get_sets().find(id))) return;

        get_sets().erase(id);
        if (debug) std::cerr << __func__ << ": hash table #" << id << " deleted\n";
    }

    size_t hash_size(unsigned long id) {
        if (debug) cerr_func_input(__func__, id);
        auto iter = get_sets().find(id);
        if (!validate_args(__func__, id, iter)) return 0;

        size_t size = iter->second.size();
        if (debug)
            std::cerr << __func__ << ": hash table #" << id << " contains " << size << " element(s)\n";
        return size;
    }



    bool hash_insert(unsigned long id, uint64_t const *seq, size_t size) {
        if (debug) cerr_func_input(__func__, id, seq, size);
        auto iter = get_sets().find(id);
        if (!validate_args(__func__, id, iter, seq, size)) return false;

        bool result = iter->second.insert(std::vector(seq, seq + size)).second;
        if (debug) cerr_hash_insert(__func__, id, seq, size, result);
        return result;
    }


    bool hash_remove(unsigned long id, uint64_t const *seq, size_t size) {
        if (debug) cerr_func_input(__func__, id, seq, size);
        auto iter = get_sets().find(id);
        if (!validate_args(__func__, id, iter, seq, size)) return false;

        unsigned long how_many_removed = iter->second.erase(std::vector(seq, seq + size));
        if (debug) cerr_hash_remove(__func__, id, seq, size, how_many_removed);
        return how_many_removed > 0;
    }

    void hash_clear(unsigned long id) {
        if (debug) cerr_func_input(__func__, id);
        auto iter = get_sets().find(id);
        if (!validate_args(__func__, id, iter)) return;

        if (iter->second.empty()) {
            if (debug) std::cerr << __func__ << ": hash table #" << id << " was empty\n";
        } else {
            iter->second.clear();
            if (debug) std::cerr << __func__ << ": hash table #" << id << " cleared\n";
        }
    }

    bool hash_test(unsigned long id, uint64_t const *seq, size_t size) {
        if (debug) cerr_func_input(__func__, id, seq, size);
        auto iter = get_sets().find(id);
        if (!validate_args(__func__, id, iter, seq, size)) return false;

        bool count = iter->second.count(std::vector(seq, seq + size));
        if (debug) cerr_hash_test(__func__, id, seq, size, count);
        return count;
    }
}