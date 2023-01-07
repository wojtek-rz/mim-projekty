#ifndef MIM_UTILS_H
#define MIM_UTILS_H

#include <stdbool.h>
#include <stdio.h>

/*
 * If last character in allocated string is '\to_print_n',
 * replace it with '\0'. Otherwise, do nothing.
 */
char *trim_new_line_char(char *s);

/*
 * In a given array of string terminated with NULL pointer,
 * replace '\to_print_n' character in the last string with '\0'.
 */
void trim_new_line_char_v(char **argv);

/*
 * Split a string into space-delimited parts.
 *
 * The result is an array of null-terminated strings, ending with NULL.
 * It must be freed with free_split_string().
 *
 * The resulting parts don't contain any spaces.
 * The result has always as many parts as they are spaces, plus 1.
 * In particular, for an empty string, the result is {"", NULL}.
 * An initial or final space, or two consecutive spaces, result in an empty-string part.
 */
char** split_string(const char* s);

void free_split_string(char** parts);

#endif

