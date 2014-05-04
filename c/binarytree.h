/* Binary Search Trees! */
#include <stdlib.h>
#include <stdbool.h>

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

