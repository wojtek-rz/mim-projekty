/** @file
 * Interfejs modułu do przechowywania ciągu numerów telefonów.
 *
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#ifndef PHONE_NUMBERS_PHONE_NUMBERS_H
#define PHONE_NUMBERS_PHONE_NUMBERS_H


#include <stdbool.h>


/**
 * To jest struktura przechowująca ciąg numerów telefonów.
 */
struct PhoneNumbers;

/** @typedef PhoneNumbers
 * Definiuje strukturę PhoneNumbers.
 */
typedef struct PhoneNumbers PhoneNumbers;

// Dwie forward deklaracje.
typedef struct PhoneForward PhoneForward;

PhoneNumbers *phfwdGet(PhoneForward const *pf, char const *num);

/** @brief Usuwa strukturę.
 * Usuwa strukturę wskazywaną przez @p pn. Nic nie robi, jeśli wskaźnik ten ma
 * wartość NULL.
 * @param[in] pn – wskaźnik na usuwaną strukturę.
 */
void phnumDelete(PhoneNumbers *pn);

/** @brief Udostępnia numer.
 * Udostępnia wskaźnik na napis reprezentujący numer. Napisy są indeksowane
 * kolejno od zera.
 * @param[in] pnum – wskaźnik na strukturę przechowującą ciąg numerów telefonów;
 * @param[in] idx  – indeks numeru telefonu.
 * @return Wskaźnik na napis reprezentujący numer telefonu. Wartość NULL, jeśli
 *         wskaźnik @p pnum ma wartość NULL lub indeks ma za dużą wartość.
 */
char const *phnumGet(PhoneNumbers const *pnum, size_t idx);

/**
 * Tworzy nową strukturę @p PhoneNumbers.
 * @return Zwraca wskaźnik na utworzoną strukturę. Jeśli alokacja pamięci się nie uda, zwraca @p NULL.
 */
PhoneNumbers *phnumNew(void);

/**
 * Dodaje numer do struktury @p PhoneNumbers.
 * @param[in] pn - wskaźnik na strukturę @p PhoneNumbers;
 * @param[in] num - wskaźnik na napis oznaczający numer;
 * @return Zwraca @p true, jeśli operacja się powiodła lub @p false, jeśli nie. W przypadku niepowodzenia
 * nie narusza istniejącej struktury.
 */
bool phnumAdd(PhoneNumbers *pn, char const *num);

/**
 * Łączy dwa części numeru i dodaje całość do struktury @p PhoneNumbers.
 * @param[in,out] pn - wskaźnik na strukturę @p PhoneNumbers;
 * @param[in] num_pref - wskaźnik na pierwszą część numeru;
 * @param[in] num_suf - wskaźnik na drugą część numeru;
 * @return Zwraca @p true, jeśli operacja się powiodła lub @p false, jeśli nie. W przypadku niepowodzenia
 * nie narusza istniejącej struktury.
 */
bool concat_to_phnum(PhoneNumbers *pn, char const *num_pref, char const *num_suf);

/** @brief Sortuje numery.
* @param[in,out] pn - wskaźnik na strukturę PhoneNumbers;
*/
void sort_phnum(PhoneNumbers *pn);

/** @brief Usuwa duplikaty ze struktury PhoneNumbers, jeśli znajdują się obok siebie.
 * Jeśli operacja się nie powiedzie, zostawia strukturę niezmienioną.
 * @param[in,out] pn - wskaźnik na strukturę PhoneNumbers;
 * @return Zwraca \p true, jeśli operacja się powiedzie i \p false, jeśli nie.
 */
bool remove_adjacent_duplicates(PhoneNumbers *pn);

/** @brief Usuwa numery, dla których wywołąnie @p phfwdGet(pf, numer) zwraca coś innego
 * niż podany, oczekiwany wynik.
 * Jeśli operacja się nie powiedzie, zostawia strukturę niezmienioną.
 * @param[in,out] pn - wskaźnik na strukturę PhoneNumbers;
 * @param[in] pf - wskaźnik na strukturę PhoneForward;
 * @param[in] expected_num - wskaźnik na oczekiwany numer;
 * @return Zwraca \p true, jeśli operacja się powiedzie i \p false, jeśli nie.
 */
bool remove_invalid_numbers(PhoneNumbers *pn, const PhoneForward *pf, const char *expected_num);

#endif //PHONE_NUMBERS_PHONE_NUMBERS_H
