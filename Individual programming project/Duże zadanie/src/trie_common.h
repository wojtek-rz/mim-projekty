/** @file
 * Interfejs klasy operacji wspólnych dla drzew "forward" i "reverse".
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */


#ifndef PHONE_NUMBERS_TRIE_COMMON_H
#define PHONE_NUMBERS_TRIE_COMMON_H

#include <stdbool.h>
#include "linked_list.h"
#include "utils.h"

struct Node;
/** @typedef Node
 * Definiuje strukturę Node.
 */
typedef struct Node Node;

/**
  * Struktura przechowująca węzeł drzewa.
  * Każdy węzeł drzewa ma wskaźniki na tyle dzieci trzymanych w tablicy,
  * ile wynosi @p NUMBER_OF_DIGITS.
  * Dziecko o indeksie i łączy się z węzłem krawędzią o numerze i.
  * Jeśli węzeł jest korzeniem to jego ojcem jest \p NULL a wartość krawędzi
  * idącej do ojca to -1.
  * Węzeł może należeć do jednego z dwóch drzew "forward" i "reverse",
  * co określa wartość typu \p bool.
  * Jeśli węzeł jest typu "forward" to zawiera pole \p forwards_to, a
  * pole \p forwards_from jest ustawione na \p NULL, a jeśli należy do
  * typu "reverse" odwrotnie.
  */
struct Node {
    char *forwards_to; ///< wskaźnik na numer przekierowania;
    List *forwards_from; ///< wskaźnik na numery, które przekierowują na ten numer;
    Node *children[NUMBER_OF_DIGITS]; ///< wskaźniki na dzieci węzła;
    Node *parent; ///< wskaźnik na ojca;
    Elem *list_elem; ///< wskaźnik element listy w drzewie 'reverse',
    ///< określające to samo przekierowanie, co \p Node;
    Node *rev_node_ptr; ///< wskaźnik na węzeł drzewa 'reverse',
    ///< zawierające element listy powyżej;
    int code; ///< numer krawędzi idącej do ojca;
    bool is_reverse; ///< wartość określająca, czy węzeł
    ///< należy do drzewa "reverse", czy "forward";
};

/**
 * Stwierdza, czy węzeł zawiera jakieś przekierowanie.
 * @param[in] node - wskaźnik na węzeł;
 * @return Jeśli węzeł jest pusty zwraca \p true, jeśli nie \p false.
 */
bool is_node_empty(Node *node);

/** @brief Usuwa puste węzły.
 * Funkcja sprawdza, czy węzeł i wszystkie wyższe węzły
 * nie są puste, a jeśli tak, to je usuwa z pamięci.
 * W szczególności może nie zrobić nic. Nie usunie korzenia drzewa.
 * @param[in] node - węzeł początkowy;
 */
void delete_empty_nodes_upwards(Node *node);

/**
 * Usuwa węzeł i wszystkie jego węzły potomne.
 * @param[in] node - węzeł;
 */
void delete_trie(Node *node);

/**
 * Znajduje węzeł idąc po krawędziach zdefiniowanych przez napis.
 * Jeśli w trakcie przechodzenia napotka na brak węzła, to zwróci
 * \p NULL oraz wskaźnik na literę w napisie do której funkcja doszła.
 * @param[in] node - wskaźnik na węzęl początkowy;
 * @param[in] num - wskaźnik na napis;
 * @param[in,out] i - wskaźnik na pozycję w napisie, do której funkcja doszła;
 * @return - Zwraca wskaźnik na węzeł o szukanej pozycji lub \p NULL,
 * jeśli ten węzeł nie istnieje.
 */
Node *find_node(Node *node, char const *num, size_t *i);

#endif //PHONE_NUMBERS_TRIE_COMMON_H
