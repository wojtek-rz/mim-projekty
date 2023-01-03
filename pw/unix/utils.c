#include "utils.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

char *trim_new_line_char(char *s){
    unsigned long len = strlen(s);
    if (s[len - 1] == '\n'){
        s[len - 1] = '\0';
    }
    return s;
}

void trim_new_line_char_v(char **argv){
    unsigned long i = 0;
    while (argv[i] != NULL) i++;
    argv[i - 1] = trim_new_line_char(argv[i - 1]);
}

char** split_string(const char* s)
{
    size_t len = strlen(s);
    int spaces = 0;
    for (int i = 0; i < len; ++i)
        if (s[i] == ' ')
            spaces++;
    char** parts = calloc(spaces + 2, sizeof(char*));
    parts[spaces + 1] = NULL;
    int p = 0;
    int b = 0;
    for (int i = 0; i < len; ++i) {
        if (s[i] == ' ') {
            parts[p++] = strndup(s + b, i - b);
            b = i + 1;
        }
    }
    parts[p++] = strndup(s + b, len - b);
    assert(p == spaces + 1);
    return parts;
}

void free_split_string(char** parts)
{
    for (int i = 0; parts[i] != NULL; ++i)
        free(parts[i]);
    free(parts);
}