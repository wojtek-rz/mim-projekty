#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#define FIRST_BIT (1 << 31)

// Ten plik zawiera przykład użycia funkcji:
bool inverse_permutation(size_t n, int *p);

// Sprawdza, czy permutacja p2 jest permutacją odwrotną do permutacji p1 o długości n.
static bool check_inverse_permutation(size_t n, int const *p1, int const *p2) {
    for (size_t i = 0; i < n; ++i)
        if ((size_t)p2[p1[i]] != i)
            return false;
    return true;
}

// Sprawdza, czy permutacje p1 i p2 o długości n są identyczne.
static bool compare_permutations(size_t n, int const *p1, int const *p2) {
    for (size_t i = 0; i < n; ++i)
        if (p1[i] != p2[i])
            return false;
    return true;
}

void print_array(int *a, size_t n) {
    for (size_t i = 0; i < n; ++i)
    {
        printf("%d ", a[i]);
    }
    printf("\n");
}

int power(int a, int b) {
    int result = 1;
    for (int i = 0; i < b; ++i) result *= a;
    return result;
}

// Checks every permutation of seq_size numbers form [-seq_size, seq_size]
// when mode = 1 then numbers from -seq_size to -1 are replaced with numbers with first bit set.
void test(int seq_size, int mode) {
    int max_number = power(2 * seq_size + 1, seq_size);
    bool should_be_correct;
    bool result;
    int seq[seq_size];
    bool a[seq_size];

    for (int number = 0; number < max_number; ++number) {
        for (int i = seq_size - 1, number_copy = number; i >= 0; --i) {
            seq[i] = number_copy % (seq_size * 2 + 1) - (seq_size);
            number_copy /= (seq_size * 2 + 1);

            if (mode == 1 && seq[i] < 0)
                seq[i] = -seq[i] + FIRST_BIT;
        }

        for (int i = 0; i < seq_size; ++i) a[i] = false;
        should_be_correct = true;

        for (int i = 0; i < seq_size; ++i) {
            if (seq[i] < 0 || seq[i] >= seq_size || a[seq[i]])
            {
                should_be_correct = false;
                break;
            }
            a[seq[i]] = true;
        }

        // copy permutation
        int seq2[seq_size];
        for (int i = 0; i < seq_size; ++i) seq2[i] = seq[i];

        if (should_be_correct) {
            result = inverse_permutation(seq_size, seq);
            result = result && (check_inverse_permutation(seq_size, seq2, seq));
        }
        else {
            result = !inverse_permutation(seq_size, seq);
            result = result && compare_permutations(seq_size, seq2, seq);
        }
        if (!result) {
            printf("\nTest for sequence: \n");
            print_array(seq, seq_size);
            printf("failed.\n\n");
            assert(false);
        }
    }
    printf("Test (%d, %d) passed\n", seq_size, mode);
}

int main() {
    for (int i = 1; i <= 6; ++i)
    {
        test(i, 0);
        test(i, 1);
    }

    return 0;
}