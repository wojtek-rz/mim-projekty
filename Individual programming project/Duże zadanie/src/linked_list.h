/** @file
 * Interfejs listy dwukierunkowej przechowującej napisy.
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */
#ifndef PHONE_NUMBERS_LINKED_LIST_H
#define PHONE_NUMBERS_LINKED_LIST_H


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "phone_forward.h"

/**
 * Struktura przechowująca element listy dwukierunkowej.
 */
struct Elem;

/**
 * @typedef Elem
 * Definiuje strukturę \p Elem.
 */
typedef struct Elem Elem;

/**
 * Struktura przechowująca całą listę.
 */
struct List;

/**
 * @typedef List
 * Definiuje strukturę \p List.
 */
typedef struct List List;

/**
 * Tworzy nową pustą listę.
 * @return Wskaźnik do struktury lub \p NULL w przypadku braku pamięci.
 */
List *create_list();

/**
 * Dodaje nowy napis do listy.
 * @param[in,out] list - wskaźnik na listę;
 * @param[in] num - wskaźnik na napis;
 * @return Wskaźnik na utworzony element listy lub \p NULL w przypadku braku pamięci.
 */
Elem *add_element_to_list(List *list, const char *num);

/**
 * Usuwa element z listy.
 * @param[in] elem - wskaźnik na element, który ma zostać usunięty.
 */
void remove_list_element(Elem *elem);

/**
 * Usuwa listę i zwalnia pamięć komputera.
 * @param[in] list - wskaźnik na listę;
 */
void delete_list(List *list);

/**
 * Dla każdego elementu listy dodaje na końcu napis i całość dodaje do struktury \p PhoneNumbers.
 * @param[in] list - wskaźnik na listę;
 * @param[in] num_suffix - wskaźnik na napis;
 * @param[in,out] pn - wskaźnik na strukturę \p PhoneNumbers;
 * @return Zwraca \p true, jeśli operacja się udała lub \p false,
 * jeśli dodawania do struktury \p PhoneNumbers się nie powiodło.
 */
bool concat_from_list_to_phnum(List *list, const char *num_suffix, PhoneNumbers *pn);

/**
 * Sprawdza, czy lista jest pusta.
 * @param[in] list - wskaźnik na listę;
 * @return Zwraca \p true, jeśli lista jest pusta lub \p false w przeciwnym wypadku.
 */
bool is_list_empty(List *list);


#endif //PHONE_NUMBERS_LINKED_LIST_H