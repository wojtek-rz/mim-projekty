/** @file
 * Implementacja klasy operacji wspólnych dla drzew "forward" i "reverse".
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include "trie_common.h"
#include "reverse_trie.h"

bool is_node_empty(Node *node) {
    if (node->is_reverse) {
        // Wersja dla "reverse trie".
        if (node->forwards_from == NULL) return true;
        return is_list_empty(node->forwards_from);
    } else {
        // Wersja dla "forward trie".
        return node->forwards_to == NULL;
    }
}

/** @brief Usuwa z pamięci pojedynczy węzeł.
 * @param[in] node - wskaźnik na węzeł;
 */
static void delete_one_node(Node *node) {
    if (node->parent != NULL) node->parent->children[node->code] = NULL;

    if (node->is_reverse) {
        // Wersja dla "reverse trie".
        if (node->forwards_from != NULL) {
            delete_list(node->forwards_from);
        }
    } else {
        // Wersja dla "forward trie".
        if (!is_node_empty(node)) {
            free(node->forwards_to);
            delete_list_elem_from_rev_trie(node->list_elem, node->rev_node_ptr);
        }
    }
    free(node);
}

void delete_empty_nodes_upwards(Node *node) {
    bool is_every_node_empty = true;
    Node *tmp_node;
    while (is_every_node_empty && node != NULL && node->parent != NULL) {
        for (int i = 0; i < NUMBER_OF_DIGITS; i++)
            if (node->children[i] != NULL) is_every_node_empty = false;

        if (!is_node_empty(node)) is_every_node_empty = false;

        tmp_node = node->parent;
        if (is_every_node_empty) {
            delete_one_node(node);
        }
        node = tmp_node;
    }
}

void delete_trie(Node *node) {
    Node *new_node, *end_node = node->parent;

    int i = 0;
    while (node != end_node) {
        while (i < NUMBER_OF_DIGITS && node->children[i] == NULL) i++;

        if (i == NUMBER_OF_DIGITS) {
            new_node = node->parent;
            i = node->code + 1;

            delete_one_node(node);
        } else {
            new_node = node->children[i];
            i = 0;
        }
        node = new_node;
    }

    delete_empty_nodes_upwards(node);
}

Node *find_node(Node *node, char const *num, size_t *i) {
    size_t child_index;
    Node *prev_node = node;
    *i = 0;

    while (node != NULL && num[*i] != END_OF_STRING) {
        child_index = get_digit_code(num[*i]);
        prev_node = node;
        node = node->children[child_index];
        //z prev_node do node prowadzi krawędź o wartości num[*i]
        if (node != NULL) {
            (*i)++;
            prev_node = node;
        }
    }

    return prev_node;
}