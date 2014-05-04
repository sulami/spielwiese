/* Binary Search Trees! */
#include <stdlib.h>
#include <stdbool.h>

struct node {
    int key;
    int data;
    struct node *left;
    struct node *right;
};

struct node *_NewNode(int key, int data) {
    struct node* node = malloc(sizeof(node));
    if (node != NULL) {
        node->key = key;
        node->data = data;
        node->left = NULL;
        node->right = NULL;
        return(node);
    }
}

struct node *btree_insert(struct node *node, int key, int data) {
    if (node == NULL) {
        return(_NewNode(key, data));
    } else {
        if (key <= node->key) {
            node->left = btree_insert(node->left, key, data);
        } else {
            node->right = btree_insert(node->right, key, data);
        }
        return(node);
    }
}

int btree_lookup(struct node *node, int target) {
    if (node == NULL) {
        return(false);
    } else {
        if (target == node->key) {
            return(node->data);
        } else {
            if (target < node->key) {
                return(btree_lookup(node->left, target));
            } else {
                return(btree_lookup(node->right, target));
            }
        }
    }
}

static struct node *_parent(struct node *node, int key) {
    if ((node->left->key == key) || (node->right->key == key)) {
        return(node);
    } else {
        if (key <= node->key) {
            return _parent(node->left, key);
        } else {
            return _parent(node->right, key);
        }
    }
}

struct node *btree_remove(struct node *node, int key) {
    if (node == NULL) {
        return(false);
    }
    if (node->key == key) {
        /* TODO delete root node */
    }
    if (btree_lookup(node, key)) {
        /* TODO find and delete node */
        struct node *parent = _parent(node, key);
    } else {
        return(false);
    }
}

