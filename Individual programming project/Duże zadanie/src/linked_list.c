/** @file
 * Implementacja klasy przechowującej listę dwukierunkową z napisami w węzłach.
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include "linked_list.h"

/**
 * Struktura przechowująca elementy listy dwukierunkowej.
 */
struct Elem {
    Elem *prev; ///< wskaźnik na poprzedni element listy;
    Elem *next; ///< wskaźnik na następny element listy;
    char *num; ///< wskaźnik na napis, który przechowuje element listy;
};

/**
 * Struktura przechowująca listę dwukierunkową z napisami w węzłach.
 * Implementacja opiera się na dwóch strażnikach, którzy stoją
 * na dwóch końcach listy dla łatwiejszej implementacji, ale nie przechowują napisu.
 * Elementy właściwe znajdują się między strażnikami.
 */
struct List {
    Elem *start; ///< wskaźnik na początkowego strażnika;
    Elem *end; ///< wskaźnik na końcowego strażnika;
};

List *create_list() {
    List *list = (List *) malloc(sizeof(List));
    if (list == NULL) return NULL;

    list->start = (Elem *) malloc((sizeof(Elem)));
    if (list->start == NULL) {
        free(list);
        return NULL;
    }

    list->end = (Elem *) malloc((sizeof(Elem)));
    if (list->end == NULL) {
        free(list->start);
        free(list);
        return NULL;
    }

    list->start->prev = NULL;
    list->start->next = list->end;
    list->end->prev = list->start;
    list->end->next = NULL;
    list->start->num = list->end->num = NULL;

    return list;
}

Elem *add_element_to_list(List *list, const char *num) {
    Elem *elem = (Elem *) malloc(sizeof(Elem));
    if (elem == NULL) return NULL;

    elem->num = (char *) malloc((strlen(num) + 1) * sizeof(char));
    if (elem->num == NULL) {
        free(elem);
        return NULL;
    }

    strcpy(elem->num, num);

    elem->next = list->start->next;
    elem->next->prev = elem;

    elem->prev = list->start;
    list->start->next = elem;

    return elem;
}

void remove_list_element(Elem *elem) {
    if (elem == NULL) return;
    elem->prev->next = elem->next;
    elem->next->prev = elem->prev;

    free(elem->num);
    free(elem);
}

void delete_list(List *list) {
    if (list == NULL) return;
    Elem *current = list->start->next;
    Elem *next_current;
    while (current != list->end) {
        next_current = current->next;
        free(current->num);
        free(current);
        current = next_current;
    }
    free(list->start);
    free(list->end);
    free(list);
}

bool concat_from_list_to_phnum(List *list, const char *num_suffix, PhoneNumbers *pn) {
    Elem *current = list->start->next;
    while (current != list->end) {
        if (!concat_to_phnum(pn, current->num, num_suffix)) return false;
        current = current->next;
    }
    return true;
}

bool is_list_empty(List *list) {
    return list->start->next == list->end;
}