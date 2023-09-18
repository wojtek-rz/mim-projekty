/** @file
 * Interfejs klasy operacji na drzewie typu trie z węzłami "forward"
 * do przechowywania przekierowań numerów telefonów.
 * Węzły te zawierają pojedynczy numer i są zdefiniowane w pliku @p trie_common.h.
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */
#ifndef PHONE_NUMBERS_FORWARD_TRIE_H
#define PHONE_NUMBERS_FORWARD_TRIE_H

#include <stdbool.h>
#include "linked_list.h"
#include "utils.h"
#include "reverse_trie.h"

/**
 * Tworzy pusty węzeł typu "forward" o zadanych parametrach.
 * Wszystkie pola oprócz przekazanych w argumentach są zainicjowane na @p NULL.
 * @param[in] parent - wskaźnik na węzeł ojca;
 * @param[in] code - numer krawędzi, która prowadzi od ojca do nowego węzła;
 * @return Zwraca wskaźnik na @p Node, jeśli uda się utworzyć nowy węzeł lub
 *          @p NULL, jeśli nie uda się alokować potrzebnej pamięci.
 */
Node *create_fwd_node(Node *parent, int code);

/** @brief Dodaje przekierowanie do węzła typu "forward".
 * Ustawia nowe przekierowanie w węźle, a jeśli węzeł zawierał przekierowanie wcześniej,
 * usuwa je. Jeśli alokacja pamięci się nie uda, zostawia strukturę w niezmienionym stanie.
 * @param[in] node - węzeł;
 * @param[in] num - wskaźnik na napis, na który chcemy zmienić przekierowanie w tym węźle
 * @param[in] list_elem - wskaźnik na element listy w odpowiadającym drzewie "reverse",
 * który służy do szybkiego usuwania przekierowania z obu drzew w razie potrzeby;
 * @param[in] rev_node - wskaźnik na węzeł typu "reverse" zawierającego powyższy element listy;
 * @return Zwraca @p true, jeśli operacja się udała lub @p false, jeśli nie.
 */
bool add_forwarding_for_fwd_node(Node *node, const char *num, Elem *list_elem, Node *rev_node);


/**
 * Dla podanego numeru zwraca wskaźnik na dwie części numeru po przekierowaniu,
 * jego przekierowany prefiks i niezmieniony sufiks.
 * Znajduje przekierowanie z największym prekierowanym prefiksem.
 * @param[in] node - wskaźnik na drzewo z przekierowaniami;
 * @param[in] num - wskaźnik na napis zawierający numer, którego przekierowania szukamy;
 * @param[in,out] last_fwd_prefix - wskaźnik na przekierowany prefiks numeru;
 * @param[in,out] last_fwd_suffix - wskaźnik na niezmieniony sufiks numeru;
 */
void find_forwarding_in_fwd_trie(Node *node, char const *num,
                                 char **last_fwd_prefix, char **last_fwd_suffix);

/**
 * Dodaje numer @p num2 do drzewa typu "forward" do węzła na ścieżce zadanej przez @p num1.
 * Jeśli brakuje odpowiednich węzłów tworzy je. Jeśli węzeł zawiera inny numer, usuwa go.
 * @param[in] parent - wskaźnik na korzeń drzewa;
 * @param[in] num1 - napis określający ścieżkę do węzła;
 * @param[in] num2 - napis, który węzeł ma przechowywać;
 * @param[in] elem - wskaźnik na odpowiadający element listy w drzewie "reverse";
 * @param[in] rev_node - wskaźnik na węzeł w drzewie "reverse", który zawiera powyższy element listy;
 * @return Zwraca wskaźnik na węzeł, który zawiera @p num2 lub @p NULL w przypadku niepowodzenia.
 */
Node *add_num_to_fwd_trie(Node *parent, const char *num1, const char *num2, Elem *elem, Node *rev_node);

#endif //PHONE_NUMBERS_FORWARD_TRIE_H
