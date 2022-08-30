/** @file
 * Implementacja klasy przechowującej przekierowania numerów telefonów w drzewie typu trie.
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "forward_trie.h"
#include "utils.h"
#include "reverse_trie.h"
#include "trie_common.h"


Node *create_fwd_node(Node *parent, int code) {
    Node *node = (Node *) malloc(sizeof(Node));
    if (node == NULL) return NULL;

    node->parent = parent;
    node->code = code;
    node->is_reverse = false;

    for (int i = 0; i < NUMBER_OF_DIGITS; i++) node->children[i] = NULL;
    node->forwards_from = NULL;
    node->forwards_to = NULL;
    node->list_elem = NULL;
    node->rev_node_ptr = NULL;

    return node;
}

/** Tworzy nowe węzły "forward" wzdłuż ścieżki określonej przez napis.
 * @param[in] root - wskaźnik na węzeł początkowy;
 * @param[in] num - napis, który definiuje ścieżkę;
 * @return Zwraca wskaźnik na węzeł, który znajduje się na końcu ścieżki
 * lub \p NULL, jeśli nie udała się alokacja pamięci.
 */
Node *create_fwd_nodes(Node *root, const char *num) {
    size_t i = 0;
    int digit;
    Node *node = root, *new_node;
    while (num[i] != END_OF_STRING) {
        digit = get_digit_code(num[i]);
        if (node->children[digit] == NULL) {
            new_node = create_fwd_node(node, digit);
            if (new_node == NULL) return NULL;

            node->children[digit] = new_node;
        } else {
            new_node = node->children[digit];
        }
        node = new_node;
        i++;
    }
    return node;
}

bool add_forwarding_for_fwd_node(Node *node, const char *num, Elem *list_elem, Node *rev_node) {
    if (node->is_reverse) return false;

    char *new_forwards_to = (char *) malloc((strlen(num) + 1) * sizeof(char));
    if (new_forwards_to == NULL) return false;

    if (!is_node_empty(node)) {
        free(node->forwards_to);
        node->forwards_to = NULL;
        // Jeśli węzeł zawierał przekierowanie, musimy również je usunąć z drugiego drzewa.
        delete_list_elem_from_rev_trie(node->list_elem, node->rev_node_ptr);
        node->list_elem = NULL;
        node->rev_node_ptr = NULL;
    }

    node->forwards_to = new_forwards_to;
    strcpy(node->forwards_to, num);

    node->list_elem = list_elem;
    node->rev_node_ptr = rev_node;
    return true;
}

void find_forwarding_in_fwd_trie(Node *node, char const *num, char **last_fwd_prefix, char **last_fwd_suffix) {
    size_t child_index;
    size_t i = 0;

    *last_fwd_prefix = "";
    *last_fwd_suffix = (char *) num;

    while (node != NULL && num[i] != END_OF_STRING) {
        child_index = get_digit_code(num[i]);
        node = node->children[child_index];
        i++;

        if (node != NULL && !is_node_empty(node)) {
            *last_fwd_prefix = node->forwards_to;
            *last_fwd_suffix = (char *) (num + i);
        }
    }
}

Node *add_num_to_fwd_trie(Node *parent, const char *num1, const char *num2, Elem *elem, Node *rev_node) {
    size_t i;

    Node *node = find_node(parent, num1, &i);
    if (num1[i] != END_OF_STRING) node = create_fwd_nodes(node, num1 + i);
    if (node == NULL) return NULL;

    if (!add_forwarding_for_fwd_node(node, num2, elem, rev_node))
        // Jeśli dodawanie napisu się nie powiedzie, na razie zostawiamy strukturę bez zmian.
        return NULL;

    return node;
}
