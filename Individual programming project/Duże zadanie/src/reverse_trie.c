/** @file
 * Implementacja interfejsu "reverse_trie.h".
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include "reverse_trie.h"

Node *create_rev_node(Node *parent, int code) {
    Node *node = (Node *) malloc(sizeof(Node));
    if (node == NULL) return NULL;

    node->is_reverse = true;
    node->parent = parent;
    node->code = code;

    for (int i = 0; i < NUMBER_OF_DIGITS; i++) node->children[i] = NULL;
    node->forwards_from = NULL;
    node->forwards_to = NULL;
    node->list_elem = NULL;
    node->rev_node_ptr = NULL;

    return node;
}

/**
 * @brief Tworzy nowe węzły typu "reverse".
 * Tworzy nowe puste węzły od podanego węzła o dzieciach na ścieżce
 * zadanej przez napis.
 * W przypadku, gdy węzły na szukanej ścieżce istnieją zachowanie niezdefiniowane.
 * @param[in] parent_node - wskaźnik na początkowy węzeł;
 * @param[in] num - wskaźnik na napis;
 * @return Zwraca wskaźnik na ostatni utworzony węzeł.
 */
Node *create_rev_nodes(Node *parent_node, const char *num) {
    size_t i = 0;
    int code;
    Node *node = parent_node;

    while (num[i] != END_OF_STRING) {
        code = get_digit_code(num[i]);
        node->children[code] = create_rev_node(node, code);
        if (node->children[code] == NULL) return NULL;
        node = node->children[code];
        i++;
    }
    return node;
}

void delete_list_elem_from_rev_trie(Elem *elem, Node *contains_elem) {
    remove_list_element(elem);
    delete_empty_nodes_upwards(contains_elem);
}

/**
 * Dodaje numer do listy w węźle.
 * @param[in] node - wskaźnik na węzeł;
 * @param[in] num - wskaźnik na numer;
 * @return Zwraca utworzony element listy lub \p NULL w przypadku niepowodzenia.
 */
static Elem *add_num_to_rev_node(Node *node, const char *num) {
    if (node->forwards_from == NULL) {
        node->forwards_from = create_list();
        if (node->forwards_from == NULL) return NULL;
    }
    return add_element_to_list(node->forwards_from, num);
}

bool find_nums_that_forwards_to_num(Node *node, char const *num, PhoneNumbers *pn) {
    size_t child_index;
    size_t i = 0;

    while (node != NULL && num[i] != END_OF_STRING) {
        child_index = get_digit_code(num[i]);
        node = node->children[child_index];
        i++;

        if (node != NULL && !is_node_empty(node)) {
            if (!concat_from_list_to_phnum(node->forwards_from, num + i, pn))
                return false;
        }
    }
    return true;
}

Node *add_num_to_reverse_trie(Node *rev_trie, const char *num1, const char *num2, Elem **elem_ptr) {
    size_t i;

    Node *rev_node = find_node(rev_trie, num1, &i);
    if (num1[i] != END_OF_STRING) {
        rev_node = create_rev_nodes(rev_node, num1 + i);
    }
    if (rev_node == NULL) return NULL;

    *elem_ptr = add_num_to_rev_node(rev_node, num2);
    if (*elem_ptr == NULL) {
        // Jeśli nie uda się dodać do listy, to może możemy usunąć wierzchołek.
        delete_empty_nodes_upwards(rev_node);
        return NULL;
    }

    return rev_node;
}
