/* Binary Search Trees! */
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct node {
    int key;
    int data;
    struct node *left;
    struct node *right;
};

struct node *NewNode(int key, int data) {
    struct node* node = malloc(sizeof(node));
    if (node != NULL) {
        node->key = key;
        node->data = data;
        node->left = NULL;
        node->right = NULL;
        return(node);
    }
}

struct node *insert(struct node *node, int key, int data) {
    if (node == NULL) {
        return(NewNode(key, data));
    } else {
        if (key <= node->key) {
            node->left = insert(node->left, key, data);
        } else {
            node->right = insert(node->right, key, data);
        }
        return(node);
    }
}

static int lookup(struct node *node, int target) {
    if (node == NULL) {
        return(false);
    } else {
        if (target == node->key) {
            return(node->data);
        } else {
            if (target < node->key) {
                return(lookup(node->left, target));
            } else {
                return(lookup(node->right, target));
            }
        }
    }
}

int main(int argc, const char *argv[])
{
    struct node *tree = insert(NULL, 4, 16);
    insert(tree, 2, 4);
    insert(tree, 3, 9);
    insert(tree, 6, 36);
    insert(tree, 7, 49);
    insert(tree, 5, 25);

    printf("Insert Key: ");
    int key;
    scanf("%d", &key);
    if (lookup(tree, key) != false) {
        printf("Value for %d: %d\n", key, lookup(tree, key));
    } else {
        printf("No Value for %d in memory\n", key);
    }
    return 0;
}

