/** @file
 * Implementacja zbioru narzędzi i funkcji wykorzystywanych między modułami.
 * @author Wojciech Rzepliński <wr438709@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include <stddef.h>
#include "utils.h"

bool is_digit_valid(char c) {
    if (c >= '0' && c <= '9') return true;
    else if (c == '#' || c == '*') return true;
    return false;
}

bool is_number_valid(char const *num) {
    if (num == NULL) return false;
    if (num[0] == 0) return false;
    int i = 0;
    while (is_digit_valid(num[i])) i++;
    return num[i] == 0;
}

int get_digit_code(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c == '*') return 10;
    if (c == '#') return 11;
    return -1;
}

int compare_string(const void *s_a, const void *s_b) {
    char *a = *(char **) s_a;
    char *b = *(char **) s_b;

    size_t i = 0;
    while (a[i] == b[i] && a[i] != END_OF_STRING && b[i] != END_OF_STRING) {
        i++;
    }
    if (a[i] == END_OF_STRING && b[i] == END_OF_STRING) return 0;
    if (b[i] == END_OF_STRING) return 1;
    if (a[i] == END_OF_STRING) return -1;
    return get_digit_code(a[i]) - get_digit_code(b[i]);
}