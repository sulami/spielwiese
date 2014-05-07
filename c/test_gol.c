#include <stdio.h>
#include <assert.h>
#include "gol.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
    fn_name();

void test_exits_gracefully() {
    assert(cgol_live() == 0);
}

void test_creates_random_start() {
    assert(cgol_gen_start(80, 32) != NULL);
}

int main() {
    printf("\nRunning tests:\n");
    run_test(test_exits_gracefully);
    run_test(test_creates_random_start);
    printf("\n => All tests successful!\n");
    return(0);
}

