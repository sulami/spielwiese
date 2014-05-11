#include <assert.h>
#include <stdio.h>
#include "gol.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
    fn_name();

void test_exits_gracefully(void);
void test_creates_random_start(void);
void test_values_are_zero_ore_none(void);
void test_conways_rule_one(void);
void test_conways_rule_two(void);
void test_conways_rule_three(void);

void test_exits_gracefully() {
    assert(cgol_live() == 0);
}

void test_creates_random_start() {
    assert(cgol_gen_start(10, 10) != NULL);
}

void test_values_are_zero_ore_none() {
    unsigned int x = 50;
    unsigned int y = 50;
    int *screen = cgol_gen_start(y, x);
    for (unsigned int i=0; i<y; i++) {
        for (unsigned int j=0; j<x; j++) {
            int test = *(screen + (i * j) + j);
            assert(test == 0 || test == 1);
        }
    }
}

void test_conways_rule_one() {
    /* Any live cell with fewer than two live neighbours dies */
    int *screen = malloc(sizeof(int) * 9);
    if (screen != NULL) {
        *screen = 0;
        *(screen + 1) = 0;
        *(screen + 2) = 0;
        *(screen + 3) = 0;
        *(screen + 4) = 1;
        *(screen + 5) = 0;
        *(screen + 6) = 0;
        *(screen + 7) = 0;
        *(screen + 8) = 0;
        screen = cgol_next_gen(screen, 3, 3);
        assert(*(screen + 4) == 0);
        free(screen);
    }
}

void test_conways_rule_two() {
    /* Any live cell with two or three live neighbours lives on to the
     * next generation. */
    int *screen = malloc(sizeof(int) * 9);
    if (screen != NULL) {
        *screen = 0;
        *(screen + 1) = 0;
        *(screen + 2) = 0;
        *(screen + 3) = 0;
        *(screen + 4) = 1;
        *(screen + 5) = 0;
        *(screen + 6) = 1;
        *(screen + 7) = 1;
        *(screen + 8) = 0;
        screen = cgol_next_gen(screen, 3, 3);
        assert(*(screen + 4) == 1);
        free(screen);
    }
}

void test_conways_rule_three() {
    /* Any live cell with more than three live neighbours dies */
    int *screen = malloc(sizeof(int) * 9);
    if (screen != NULL) {
        *screen = 0;
        *(screen + 1) = 0;
        *(screen + 2) = 1;
        *(screen + 3) = 0;
        *(screen + 4) = 1;
        *(screen + 5) = 1;
        *(screen + 6) = 0;
        *(screen + 7) = 1;
        *(screen + 8) = 1;
        screen = cgol_next_gen(screen, 3, 3);
        assert(*(screen + 4) == 0);
        free(screen);
    }
}

int main() {
    printf("\nRunning tests:\n");
    run_test(test_exits_gracefully);
    run_test(test_creates_random_start);
    run_test(test_values_are_zero_ore_none);
    run_test(test_conways_rule_one);
    run_test(test_conways_rule_two);
    run_test(test_conways_rule_three);
    printf("\n => All tests successful!\n");
    return(0);
}

