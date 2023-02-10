/** @file
 * Interfejs zbioru narzędzi i funkcji wykorzystywanych między modułami.
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */
#ifndef PHONE_NUMBERS_UTILS_H
#define PHONE_NUMBERS_UTILS_H

#include <stdbool.h>

/**
 * @def END_OF_STRING
 * Wartość char oznaczająca koniec napisu.
 */
#define END_OF_STRING 0

/**
 * @def NUMBER_OF_DIGITS
 * Liczba znaków, które mogą wystąpić w numerze.
 */
#define NUMBER_OF_DIGITS 12

/**
 * @brief Sprawdza, czy znak może się pojawić z numerze.
 * @param[in] c - znak;
 * @return Zwraca \p true, jeśli tak oraz \p false, jeśli nie.
 */
bool is_digit_valid(char c);

/** @brief Sprawdza, czy napis jest poprawnym numerem.
 * Napis jest poprawnym numerem, jeśli zawiera tylko i wyłącznie cyfry od 0 do 9
 * oraz kończy się znakiem @p \0.
 * @param[in] num - wskaźnik na napis, który sprawdzamy;
 * @return Zwraca @p true, jeśli napis jest poprawnym numerem oraz @p false, jeśli nie.
 */
bool is_number_valid(char const *num);

/**
 * @brief Zwraca liczbę, z którą utożsamiamy dany znak w tym programie.
 * @param[in] c - znak;
 * @return Zwraca kod liczbowy znaku lub -1, jeśli znak nie może się pojawić w numerze.
 */
int get_digit_code(char c);

/**
 * @brief Porównuje dwa napisy i stwierdza, który powinien wystąpić wcześniej przy sortowaniu.
 * @param[in] s_a - wskaźnik na pierwszy napis;
 * @param[in] s_b - wskaźnik na drugi napis;
 * @return Zwraca wartość ujemną, jeśli pierwszy powinien być napis pierwszy, dodatnią jeśli drugi,
 * a jeśli są sobie równe wartość 0.
 */
int compare_string(const void *s_a, const void *s_b);

#endif //PHONE_NUMBERS_UTILS_H
