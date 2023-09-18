/** @file
 * Implementacja modułu do przechowywania ciągu numerów telefonów.
 *
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "phone_numbers.h"
#include "utils.h"


/** @brief Struktura przechowująca ciąg numerów telefonów.
*/
struct PhoneNumbers {
    char **strings; ///< tablica wskaźników na napisy
    size_t length; ///< długość tablicy wskaźników
    size_t allocated_memory; ///< liczba wskaźników na napisy, na które została przydzielona pamięć
};

PhoneNumbers *phnumNew(void) {
    PhoneNumbers *pn = (PhoneNumbers *) malloc(sizeof(PhoneNumbers));
    if (pn == NULL) return NULL;

    pn->strings = (char **) malloc(sizeof(char *));
    if (pn->strings == NULL) {
        free(pn);
        return NULL;
    }

    pn->length = 0;
    pn->allocated_memory = 1;
    return pn;
}

bool concat_to_phnum(PhoneNumbers *pn, char const *num_pref, char const *num_suf) {
    if (pn == NULL) return false;

    // Powiększa tablicę napisów.
    if (pn->length == pn->allocated_memory) {
        char **new_strings = (char **) realloc(pn->strings, 2 * pn->allocated_memory * sizeof(char *));
        if (new_strings == NULL) {
            return false;
        }
        pn->strings = new_strings;
        pn->allocated_memory *= 2;
    }

    size_t pref_length = strlen(num_pref), suf_length = strlen(num_suf);
    char *num = malloc((pref_length + suf_length + 1) * sizeof(char));
    if (num == NULL) {
        return false;
    }

    strcpy(num, num_pref);
    strcpy(num + pref_length, num_suf);

    pn->strings[pn->length] = num;
    pn->length++;
    return true;
}

bool phnumAdd(PhoneNumbers *pn, char const *num) {
    return concat_to_phnum(pn, num, "");
}


void phnumDelete(PhoneNumbers *pn) {
    if (pn != NULL) {
        for (size_t i = 0; i < pn->length; i++) {
            free(pn->strings[i]);
        }
        free(pn->strings);
        free(pn);
    }
}


char const *phnumGet(PhoneNumbers const *pnum, size_t idx) {
    if (pnum == NULL) return NULL;
    if (idx >= pnum->length) return NULL;
    return pnum->strings[idx];
}

void sort_phnum(PhoneNumbers *pn) {
    qsort(pn->strings, pn->length, sizeof(char *), compare_string);
}


bool remove_adjacent_duplicates(PhoneNumbers *pn) {
    if (pn == NULL) return false;
    if (pn->length == 0) return true;
    size_t li = 1;

    char *prev_string = pn->strings[0], *string = "";

    for (size_t i = 1; i < pn->length; i++) {
        string = pn->strings[i];
        if (compare_string(&string, &prev_string) != 0) {
            pn->strings[li] = string;
            prev_string = string;
            li++;
        } else {
            free(string);
        }
    }
    pn->length = li;

    char **new_strings = (char **) realloc(pn->strings, (li + 1) * sizeof(char *));
    if (new_strings == NULL) {
        return false;
    }
    pn->strings = new_strings;
    pn->allocated_memory = li + 1;
    return true;
}

bool remove_invalid_numbers(PhoneNumbers *pn, const PhoneForward *pf, const char *expected_num) {
    if (pn == NULL || pf == NULL) return false;
    if (pn->length == 0) return true;
    size_t li = 0;
    for (size_t i = 0; i < pn->length; i++) {
        PhoneNumbers *forwards_to = phfwdGet(pf, pn->strings[i]);
        if (forwards_to == NULL) return false;
        const char *s = phnumGet(forwards_to, 0);
        if (strcmp(s, expected_num) == 0) {
            pn->strings[li] = pn->strings[i];
            li++;
        } else {
            free(pn->strings[i]);
        }
        phnumDelete(forwards_to);
    }
    pn->length = li;

    char **new_strings = (char **) realloc(pn->strings, (li + 1) * sizeof(char *));
    if (new_strings == NULL) {
        return false;
    }
    pn->strings = new_strings;
    pn->allocated_memory = li + 1;
    return true;
}