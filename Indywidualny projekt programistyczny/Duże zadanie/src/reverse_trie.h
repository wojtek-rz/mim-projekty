/** @file
 * Intefejs klasy przechowującej odwrotność przekierowań w drzewie typu "trie".
 * Wszystkie funkcje w tej klasie operują na węzłach typu "reverse",
 * które zawierają listę numerów w węzłach.
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#ifndef PHONE_NUMBERS_REVERSE_TRIE_H
#define PHONE_NUMBERS_REVERSE_TRIE_H


#include "linked_list.h"
#include "utils.h"
#include "phone_forward.h"
#include "trie_common.h"

/**
 * @brief Tworzy pojedynczy węzeł.
 * Tworzy i alokuje pamięć na nowy węzeł typu "reverse".
 * @param[in] parent - wskaźnik na nowy węzeł;
 * @param[in] code - numer krawędzi, która prowadzi do nowego węzła;
 * @return Zwraca wskaźnik na nowy węzeł lub @p NULL,
 * jeśli nie uda się alokacja pamięci.
 */
Node *create_rev_node(Node *parent, int code);

/**
 * @brief Usuwa element listy z drzewa.
 * Usuwa element listy z listy oraz usuwa puste węzły drzewa typu "reverse".
 * @param[in] elem - wskaźnik na element listy;
 * @param[in] contains_elem - wskaźnik na wierzchołek zawierający tę listę;
 */
void delete_list_elem_from_rev_trie(Elem *elem, Node *contains_elem);


/**
 * @brief Dodaje przekierowania na podany numer.
 * Dodaje wszystkie przekierowania na podany numer i zapisuje je w strukturze PhoneNumbers.
 * @param[in] node - drzewo typu "reverse";
 * @param[in] num - numer na który przekierowujemy;
 * @param[in,out] pn - struktura PhoneNumbers, do której dodajemy numery;
 * @return Zwraca \p true jeśli operacja się powiodła i \p false, jeśli nie.
 * Funkcja nie usuwa z pamięci struktury @p PhoneNumbers w przypadku niepowodzenia.
 */
bool find_nums_that_forwards_to_num(Node *node, char const *num, PhoneNumbers *pn);

/**
 * @brief Dodaje przekierowanie na numer w drzewie typu "reverse".
 * Dodaje przekierowanie z jednego numeru na drugi
 * i zwraca wskaźnik węzeł z numerem pierwotnym
 * oraz na element listy zawierający ten numer.
 * @param[in] rev_trie - drzewo typu "reverse";
 * @param[in] num1 - numer, na który przekierowujemy;
 * @param[in] num2 - napis, z którego przekierowujemy;
 * @param[in,out] elem_ptr - wskaźnik na element listy, który zostanie utworzony;
 * @return Zwraca wskaźnik na węzeł, do którego zostało dodane przekierowanie,
 * lub @p NULL, jeśli alokacja pamięci się nie powiodła.
 */
Node *add_num_to_reverse_trie(Node *rev_trie, const char *num1, const char *num2, Elem **elem_ptr);

#endif //PHONE_NUMBERS_REVERSE_TRIE_H
