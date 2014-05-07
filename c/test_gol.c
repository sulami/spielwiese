#include <stdio.h>
#include <assert.h>
#include "gol.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
    fn_name();

void test_exits_gracefully() {
    assert(live() == 0);
}

int main() {
    printf("\nRunning tests:\n");
    run_test(test_exits_gracefully);
    printf("\n => All tests successful!\n");
    return(0);
}

