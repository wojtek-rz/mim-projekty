/** @file
 * Implementacja klasy przechowującej przekierowania numerów telefonicznych
 *
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "forward_trie.h"
#include"phone_forward.h"
#include "reverse_trie.h"
#include "phone_numbers.h"

/** @brief Struktura przechowująca przekierowania numerów telefonów.
 */
struct PhoneForward {
    Node *fwd_tree; ///< wskaźnik na węzeł będący korzeniem drzewa przekierowań trie
    Node *rev_tree; ///< wskaźnik na węzeł będący korzeniem drzewa trie,
    ///< trzymającego przekierowania w odwrotną stronę.
};

PhoneForward *phfwdNew(void) {
    PhoneForward *pf = malloc(sizeof(PhoneForward));
    if (pf == NULL) return NULL;

    pf->fwd_tree = create_fwd_node(NULL, -1);
    if (pf->fwd_tree == NULL) {
        free(pf);
        return NULL;
    }

    pf->rev_tree = create_rev_node(NULL, -1);
    if (pf->rev_tree == NULL) {
        delete_trie(pf->fwd_tree);
        free(pf);
        return NULL;
    }

    return pf;
}

void phfwdDelete(PhoneForward *pf) {
    if (pf != NULL) {
        delete_trie(pf->fwd_tree);
        delete_trie(pf->rev_tree);
        free(pf);
    }
}

bool phfwdAdd(PhoneForward *pf, char const *num1, char const *num2) {
    if (pf == NULL) return false;

    if (!is_number_valid(num1) || !is_number_valid(num2)) return false;
    if (strcmp(num1, num2) == 0) return false;

    Elem *elem;
    Node *rev_node = add_num_to_reverse_trie(pf->rev_tree, num2, num1, &elem);

    if (rev_node == NULL) return false;

    Node *fwd_node = add_num_to_fwd_trie(pf->fwd_tree, num1, num2, elem, rev_node);
    if (fwd_node == NULL) {
        delete_list_elem_from_rev_trie(elem, rev_node);
        return false;
    }
    return true;
}

void phfwdRemove(PhoneForward *pf, char const *num1) {
    if (!is_number_valid(num1) || pf == NULL) return;
    size_t i;

    Node *node = find_node(pf->fwd_tree, num1, &i);
    if (num1[i] != END_OF_STRING) return;

    // Jeśli usuwamy całe drzewo, to musimy utworzyć nowy pierwszy węzeł.
    if (node == pf->fwd_tree) pf->fwd_tree = create_fwd_node(NULL, -1);

    if (node != NULL) delete_trie(node);
}

PhoneNumbers *phfwdGet(PhoneForward const *pf, char const *num) {
    if (pf == NULL) return NULL;

    PhoneNumbers *pn = phnumNew();
    if (pn == NULL) return NULL;

    if (!is_number_valid(num)) return pn;

    char *last_fwd_prefix, *last_fwd_suffix;
    find_forwarding_in_fwd_trie(pf->fwd_tree, num, &last_fwd_prefix, &last_fwd_suffix);

    if (!concat_to_phnum(pn, last_fwd_prefix, last_fwd_suffix)) {
        phnumDelete(pn);
        return NULL;
    }

    return pn;
}

PhoneNumbers *phfwdReverse(PhoneForward const *pf, char const *num) {
    if (pf == NULL) return NULL;
    PhoneNumbers *pn = phnumNew();
    if (pn == NULL) return NULL;

    if (!is_number_valid(num)) return pn;

    if (!find_nums_that_forwards_to_num(pf->rev_tree, num, pn) || !phnumAdd(pn, num)) {
        phnumDelete(pn);
        return NULL;
    }
    sort_phnum(pn);

    if (!remove_adjacent_duplicates(pn)) {
        phnumDelete(pn);
        return NULL;
    };
    return pn;
}


PhoneNumbers *phfwdGetReverse(PhoneForward const *pf, char const *num) {
    PhoneNumbers *pn = phfwdReverse(pf, num);
    if (pn != NULL) {
        if (!remove_invalid_numbers(pn, pf, num)) {
            phnumDelete(pn);
            return NULL;
        }
    }
    return pn;
}