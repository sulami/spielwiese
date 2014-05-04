/* Tests for binarytree.h */
#include <stdio.h>
#include <assert.h>
#include "binarytree.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
    fn_name();

void test_first_node() {
    struct node *node = insert(NULL, 5, 50);
    assert(node != NULL);
    assert(node->key == 5);
    assert(node->data == 50);
}

void test_second_node() {
    struct node *tree = insert(NULL, 5, 50);
    tree = insert(tree, 3, 30);
    assert(tree != NULL);
    assert(tree->key == 5);
    assert(tree->data == 50);
    assert(lookup(tree, 5));
    assert(lookup(tree, 3));
    assert(lookup(tree, 2) == false);
}

int main(int argc, const char *argv[])
{
    printf("\nRunning tests:\n");
    run_test(test_first_node);
    run_test(test_second_node);
    printf("\x1B[32mAll tests successful!\x1B[0m\n");
    return 0;
}

