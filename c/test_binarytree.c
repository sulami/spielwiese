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
    assert(node->left == NULL);
    assert(node->right == NULL);
}

/***********************************************************************
Our complete test tree looks like this:

        5
       / \
      /   \
     3     7
    / \   /
   1   4 6
    \
     2

***********************************************************************/

void test_full_tree() {
    struct node *tree = btree_insert(NULL, 5, 50);
    tree = btree_insert(tree, 3, 30);
    tree = btree_insert(tree, 4, 40);
    tree = btree_insert(tree, 7, 70);
    tree = btree_insert(tree, 6, 60);
    tree = btree_insert(tree, 1, 10);
    tree = btree_insert(tree, 2, 20);

    assert(btree_lookup(tree, 1));
    assert(btree_lookup(tree, 2));
    assert(btree_lookup(tree, 3));
    assert(btree_lookup(tree, 4));
    assert(btree_lookup(tree, 5));
    assert(btree_lookup(tree, 6));
    assert(btree_lookup(tree, 7));
}

void test_delete_leaf() {
    struct node *tree = btree_insert(NULL, 5, 50);
    tree = btree_insert(tree, 3, 30);
    tree = btree_insert(tree, 4, 40);
    tree = btree_insert(tree, 7, 70);
    tree = btree_insert(tree, 6, 60);
    tree = btree_insert(tree, 1, 10);
    tree = btree_insert(tree, 2, 20);

    tree = btree_remove(tree, 6); /* Leaf node */

    assert(btree_lookup(tree, 1));
    assert(btree_lookup(tree, 2));
    assert(btree_lookup(tree, 3));
    assert(btree_lookup(tree, 4));
    assert(btree_lookup(tree, 5));
    assert(btree_lookup(tree, 6) == false);
    assert(btree_lookup(tree, 7));
}

void test_delete_mid() {
    struct node *tree = btree_insert(NULL, 5, 50);
    tree = btree_insert(tree, 3, 30);
    tree = btree_insert(tree, 4, 40);
    tree = btree_insert(tree, 7, 70);
    tree = btree_insert(tree, 6, 60);
    tree = btree_insert(tree, 1, 10);
    tree = btree_insert(tree, 2, 20);

    tree = btree_remove(tree, 3); /* Middle node, 2 children */

    assert(btree_lookup(tree, 1));
    assert(btree_lookup(tree, 2));
    assert(btree_lookup(tree, 3) == false);
    assert(btree_lookup(tree, 4));
    assert(btree_lookup(tree, 5));
    assert(btree_lookup(tree, 6));
    assert(btree_lookup(tree, 7));

    tree = btree_remove(tree, 1); /* Middle node, 1 child */

    assert(btree_lookup(tree, 1) == false);
    assert(btree_lookup(tree, 2));
    assert(btree_lookup(tree, 3) == false);
    assert(btree_lookup(tree, 4));
    assert(btree_lookup(tree, 5));
    assert(btree_lookup(tree, 6));
    assert(btree_lookup(tree, 7));
}

void test_delete_root() {
    struct node *tree = btree_insert(NULL, 5, 50);
    tree = btree_insert(tree, 3, 30);
    tree = btree_insert(tree, 4, 40);
    tree = btree_insert(tree, 7, 70);
    tree = btree_insert(tree, 6, 60);
    tree = btree_insert(tree, 1, 10);
    tree = btree_insert(tree, 2, 20);

    tree = btree_remove(tree, 5); /* Root node, 2 children */

    assert(btree_lookup(tree, 1));
    assert(btree_lookup(tree, 2));
    assert(btree_lookup(tree, 3));
    assert(btree_lookup(tree, 4));
    assert(btree_lookup(tree, 5) == false);
    assert(btree_lookup(tree, 6));
    assert(btree_lookup(tree, 7));

    assert(tree->key == 3); /* The new root node */

    tree = btree_remove(tree, 3); /* Root node, 1 child */

    assert(btree_lookup(tree, 1));
    assert(btree_lookup(tree, 2));
    assert(btree_lookup(tree, 3) == false);
    assert(btree_lookup(tree, 4));
    assert(btree_lookup(tree, 5) == false);
    assert(btree_lookup(tree, 6));
    assert(btree_lookup(tree, 7));
}

int main(int argc, const char *argv[])
{
    printf("\nRunning tests:\n");
    run_test(test_first_node);
    run_test(test_full_tree);
    run_test(test_delete_leaf);
    run_test(test_delete_mid);
    run_test(test_delete_root);
    printf("\x1B[32mAll tests successful!\x1B[0m\n\n");
    return 0;
}

