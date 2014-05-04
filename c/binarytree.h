/* Binary Search Trees! */
#include <stdlib.h>
#include <stdbool.h>

struct node {
    int key;
    int data;
    struct node *left;
    struct node *right;
};

static struct node *_new_node(int key, int data) {
    struct node* node = malloc(sizeof(node));
    if (node != NULL) {
        node->key = key;
        node->data = data;
        node->left = NULL;
        node->right = NULL;
        return(node);
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

static int _left_path(struct node *node, int count) {
    if (node->right != NULL) {
        return(_left_path(node->right, ++count));
    } else {
        return(++count);
    }
}

static int _right_path(struct node *node, int count) {
    if (node->left != NULL) {
        return(_left_path(node->left, ++count));
    } else {
        return(++count);
    }
}

struct node *btree_insert(struct node *node, int key, int data) {
    if (node == NULL) {
        return(_new_node(key, data));
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

