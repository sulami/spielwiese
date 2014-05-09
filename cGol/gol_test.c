#include <assert.h>
#include <stdio.h>
#include "gol.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
    fn_name();

void test_exits_gracefully(void);
void test_creates_random_start(void);
void test_values_are_zero_ore_none(void);

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
    unsigned int i;
    for (i=0; i<y; i++) {
        unsigned int j;
        for (j=0; j<x; j++) {
            int test = *(screen + (i * j) + j);
            assert(test == 0 || test == 1);
        }
    }
}

int main() {
    printf("\nRunning tests:\n");
    run_test(test_exits_gracefully);
    run_test(test_creates_random_start);
    run_test(test_values_are_zero_ore_none);
    printf("\n => All tests successful!\n");
    return(0);
}

