/* Tests for binarytree.h */
#include <stdio.h>
#include <assert.h>
#include "binarytree.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
    fn_name();

void test_first_node() {
    struct node *node = btree_insert(NULL, 5, 50);
    assert(node != NULL);
    assert(node->key == 5);
    assert(node->data == 50);
}

void test_second_node() {
    struct node *tree = btree_insert(NULL, 5, 50);
    tree = btree_insert(tree, 3, 30);
    assert(tree != NULL);
    assert(tree->key == 5);
    assert(tree->data == 50);
    assert(btree_lookup(tree, 5));
    assert(btree_lookup(tree, 3));
    assert(btree_lookup(tree, 2) == false);
}

void test_delete_node() {
    struct node *tree = btree_insert(NULL, 5, 50);
    tree = btree_insert(tree, 3, 30);
    tree = btree_insert(tree, 7, 70);
    tree = btree_insert(tree, 6, 60);
    tree = btree_insert(tree, 1, 10);
    tree = btree_insert(tree, 2, 20);

    tree = btree_remove(tree, 3);

    assert(btree_lookup(tree, 1));
    assert(btree_lookup(tree, 2));
    assert(btree_lookup(tree, 3) == false);
    assert(btree_lookup(tree, 4));
    assert(btree_lookup(tree, 5));
    assert(btree_lookup(tree, 6));
    assert(btree_lookup(tree, 7));
}

int main(int argc, const char *argv[])
{
    printf("\nRunning tests:\n");
    run_test(test_first_node);
    run_test(test_second_node);
    run_test(test_delete_node);
    printf("\x1B[32mAll tests successful!\x1B[0m\n\n");
    return 0;
}

