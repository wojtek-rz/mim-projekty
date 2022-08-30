#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <inttypes.h>
#include <errno.h>
#include "input.h"
#include "array.h"
#include "errors.h"
#include "bitvectors.h"
#include "bfs.h"
#include "types.h"

#define NUMBER_OF_PARAMETERS_IN_R 5
#define TWO_TO_32 4294967296
#define END_OF_STRING 0

static size_t find_first_no_space_index(const char line[], size_t starting_from) {
    size_t i = starting_from;
    while (isspace(line[i])) i++;
    return i;
}

// calls 'getline' and checks for errors
static int read_line_with_getline(char **line, size_t *length) {
    size_t buff_size = 0;
    ssize_t line_length;
    line_length= getline(line, &buff_size, stdin);
    // checks for 'getline' memory allocation error
    if (errno == ENOMEM) memory_allocation_error();

    // getline returns -1 when reading fails
    if (line_length == -1){
        free(*line);
        return FUNCTION_FAILURE;
    }
    else {
        *length = (size_t) line_length;
    }

    return FUNCTION_SUCCESS;
}

// reads a number that is starting in position 'start_index'.
// if function fails sets 'error_indicator' to 1 .
// returns position of the first character after the digit.
static size_t read_number_from_line(char line[], size_t start_index, size_t *number_ptr, int *error_indicator) {
    size_t i = start_index;

    // moves index to the first value that isn't a digit.
    while (isdigit(line[i])) i++;

    if (!isspace(line[i]) && line[i] != END_OF_STRING) {
        *error_indicator = FUNCTION_FAILURE;
        return start_index;
    }

    *number_ptr = strtoull(line + start_index * sizeof(char), NULL, 10);
    // checks if a number exceeds maximum possible value.
    if (errno == ERANGE) memory_allocation_error();

    return i;
}

static int read_line_to_array(Array *numbers) {
    char *line = NULL;
    int error_indicator = FUNCTION_SUCCESS;
    size_t parsed_number, i;
    size_t line_length;

    if (read_line_with_getline(&line, &line_length) == FUNCTION_FAILURE){
        return FUNCTION_FAILURE;
    }

    i = find_first_no_space_index(line, 0);

    while (i < line_length) {
        i = read_number_from_line(line, i, &parsed_number, &error_indicator);

        if (error_indicator == FUNCTION_FAILURE || parsed_number == 0) {
            free(line);
            return FUNCTION_FAILURE;
        }

        set_at_the_end(numbers, parsed_number);
        i = find_first_no_space_index(line, i);
    }

    free(line);
    return FUNCTION_SUCCESS;
}

static int get_hex_value_from_char(char c) {
    if (c >= '0' && c <= '9') return (c - '0');
    else if (c >= 'a' && c <= 'f') return (c - 'a' + 10);
    else if (c >= 'A' && c <= 'F') return (c - 'A' + 10);
    else return 0;
}

static inline int get_nth_bit_in_number(int number, int n) {
    return (number & (1 << n)) >> n;
}

static int parse_hex_line_to_bitvector(Bitvector *labyrinth, const char line[], size_t start_pos, size_t line_length) {
    int hex_value;
    size_t end_pos = start_pos, i, position_in_labyrinth_mod_4 = 0;

    // Finds the end position of the hex digit
    while (isxdigit(line[end_pos])) end_pos++;
    i = end_pos;

    // Checks if there are no other characters in the line after the hex digit 
    while (i < line_length) {
        if (!isspace(line[i])) return FUNCTION_FAILURE;
        i++;
    }

    for (i = end_pos - 1; i >= start_pos; i--) {
        if (!isxdigit(line[i])) return FUNCTION_FAILURE;
        hex_value = get_hex_value_from_char(line[i]);

        for (int j = 0; j < 4; j++) {
            if (position_in_labyrinth_mod_4 + j >= labyrinth->length) {
                if (get_nth_bit_in_number(hex_value, j) == 1) return FUNCTION_FAILURE;
            }
            else {
                set_bit_in_bitvector(labyrinth, position_in_labyrinth_mod_4 + j, get_nth_bit_in_number(hex_value, j));
            }
        }

        position_in_labyrinth_mod_4 += 4;
    }
    return FUNCTION_SUCCESS;
}

static inline int wrong_R_input(Array *array, size_t number) {
    return (number > UINT32_MAX || get_array_length(array) >= NUMBER_OF_PARAMETERS_IN_R
            || (get_array_length(array) == 2 && number == 0));
}

static int parse_R_line_to_array(Array *parameters, char line[], size_t start_index, size_t line_length) {
    size_t parsed_number;
    int error_indicator = FUNCTION_SUCCESS;
    size_t i = find_first_no_space_index(line, start_index);

    while (i < line_length) {
        i = read_number_from_line(line, i, &parsed_number, &error_indicator);

        if (error_indicator == FUNCTION_FAILURE || wrong_R_input(parameters, parsed_number)) {
            return FUNCTION_FAILURE;
        }

        set_at_the_end(parameters, parsed_number);
        i = find_first_no_space_index(line, i);
    }

    if (get_array_length(parameters) < NUMBER_OF_PARAMETERS_IN_R) return FUNCTION_FAILURE;
    return FUNCTION_SUCCESS;
}

static void fill_labyrinth_from_R_parameters(Bitvector *labyrinth, Array *parameters) {
    size_t dim_product = labyrinth->length;

    size_t a = get_from_array(parameters, 0), b = get_from_array(parameters, 1);
    size_t m = get_from_array(parameters, 2), r = get_from_array(parameters, 3);
    size_t s_0 = get_from_array(parameters, 4);

    Array *si_wi = create_array(get_from_array(parameters, 3));
    set_in_array(si_wi, (a * s_0 + b) % m, 0);

    for (size_t i = 1; i < r; i++) {
        set_in_array(si_wi, (a * get_from_array(si_wi, i - 1) + b) % m, i);
    }
    for (size_t i = 0; i < r; i++) {
        set_in_array(si_wi, get_from_array(si_wi, i) % dim_product, i);
        size_t n = 0;
        while (get_from_array(si_wi, i) + TWO_TO_32 * n < labyrinth->length) {
            set_bit_in_bitvector(labyrinth, get_from_array(si_wi, i) + TWO_TO_32 * n, 1);
            n += 1;
        }
    }
    delete_array(si_wi);
}

static int parse_R_line_to_bitvector(Bitvector *labyrinth, char *line, size_t start_pos, size_t line_length) {
    Array *parameters = create_array(5);

    if (parse_R_line_to_array(parameters, line, start_pos, line_length) == FUNCTION_FAILURE) {
        delete_array(parameters);
        return FUNCTION_FAILURE;
    }
    fill_labyrinth_from_R_parameters(labyrinth, parameters);

    delete_array(parameters);
    return FUNCTION_SUCCESS;
}

static int read_labyrinth_to_bitvector(Bitvector *labyrinth) {
    int return_value;

    char *line = NULL;
    size_t i = 0, line_length;

    if (read_line_with_getline(&line, &line_length) == FUNCTION_FAILURE){
        return FUNCTION_FAILURE;
    }

    set_bitvector_to_zeros(labyrinth);

    i = find_first_no_space_index(line, i);
    if (line[i] == '0') {
        if (line[i + 1] != 'x') return_value = FUNCTION_FAILURE;
        else return_value = parse_hex_line_to_bitvector(labyrinth, line, i + 2, line_length);
    }
    else if (line[i] == 'R') {
        return_value = parse_R_line_to_bitvector(labyrinth, line, i + 1, line_length);
    }
    else {
        return_value = FUNCTION_FAILURE;
    }

    free(line);
    return return_value;
}

static int handle_no_characters_on_end() {
    char c = (char) getchar();
    if (c != EOF) {
        wrong_input_error(5);
        return FUNCTION_FAILURE;
    }
    return FUNCTION_SUCCESS;
}

static int handle_positions_outside_dimensions(Labyrinth data) {
    for (size_t i = 0; i < get_array_length(data.dimensions); i++) {
        if (get_from_array(data.start_position, i) > get_from_array(data.dimensions, i)
            || get_from_array(data.start_position, i) == 0) {
            wrong_input_error(2);
            return FUNCTION_FAILURE;
        }
        if (get_from_array(data.end_position, i) > get_from_array(data.dimensions, i)
            || get_from_array(data.start_position, i) == 0) {
            wrong_input_error(3);
            return FUNCTION_FAILURE;
        }
    }
    return FUNCTION_SUCCESS;
}

static int handle_positions_in_walls(Labyrinth data) {
    size_t hash = get_hash_from_coordinates(data.start_position, data.dimensions);
    if (get_bit_from_bitvector(data.walls, hash) == 1) {
        wrong_input_error(2);
        return FUNCTION_FAILURE;
    }

    hash = get_hash_from_coordinates(data.end_position, data.dimensions);
    if (get_bit_from_bitvector(data.walls, hash) == 1) {
        wrong_input_error(3);
        return FUNCTION_FAILURE;
    }
    return FUNCTION_SUCCESS;
}

int read_input(Labyrinth labyrinth) {
    int res;
    Array *dimensions = labyrinth.dimensions;
    Array *start_pos = labyrinth.start_position;
    Array *end_pos = labyrinth.end_position;
    Bitvector *walls = labyrinth.walls;

    res = read_line_to_array(dimensions);
    if (res == FUNCTION_FAILURE || get_array_length(dimensions) == 0) {
        wrong_input_error(1);
        return FUNCTION_FAILURE;
    }

    res = read_line_to_array(start_pos);
    if (res == FUNCTION_FAILURE || get_array_length(dimensions) != get_array_length(start_pos)) {
        wrong_input_error(2);
        return FUNCTION_FAILURE;
    }

    res = read_line_to_array(end_pos);
    if (res == FUNCTION_FAILURE || get_array_length(dimensions) != get_array_length(end_pos)) {
        wrong_input_error(3);
        return FUNCTION_FAILURE;
    }

    if (handle_positions_outside_dimensions(labyrinth) == FUNCTION_FAILURE) return FUNCTION_FAILURE;

    size_t labyrinth_length = multiply_array(dimensions);
    allocate_memory_to_bitvector(walls, labyrinth_length);

    if (read_labyrinth_to_bitvector(walls) == FUNCTION_FAILURE) {
        wrong_input_error(4);
        return FUNCTION_FAILURE;
    }

    if (handle_positions_in_walls(labyrinth) == FUNCTION_FAILURE) return FUNCTION_FAILURE;
    if (handle_no_characters_on_end() == FUNCTION_FAILURE) return FUNCTION_FAILURE;
    return FUNCTION_SUCCESS;
}