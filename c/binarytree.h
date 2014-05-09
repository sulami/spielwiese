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
    } else {
        return(NULL);
    }
}

static struct node *_parent(struct node *node, int key) {
    while ((node->left->key != key) && (node->right->key != key)) {
        if (key <= node->key) {
            node = node->left;
        } else {
            node = node->right;
        }
    }
    return(node);
}

static int _left_path(struct node *node) {
    if (node->right != NULL) {
        return(1 + _left_path(node->right));
    }
    return(0);
}

static int _right_path(struct node *node) {
    if (node->left != NULL) {
        return(1 + _right_path(node->left));
    }
    return(0);
}

static struct node *_remove_node(struct node *node) {
    struct node *new_node = malloc(sizeof(node));
    if ((node->left != NULL) && (node->right != NULL)) {
        int left_side = _left_path(node->left);
        int right_side = _right_path(node->right);
        if (left_side <= right_side) {
            new_node = node->left;
            struct node *leaf = node->left;
            while (leaf->right != NULL) {
                leaf = leaf->right;
            }
            leaf->right = node->right;
        } else {
            new_node = node->right;
            struct node *leaf = node->right;
            while (leaf->left != NULL) {
                leaf = leaf->left;
            }
            leaf->left = node->left;
        }
    } else if ((node->left != NULL) && (node->right == NULL)) {
        new_node = node->left;
    } else if ((node->left == NULL) && (node->right != NULL)) {
        new_node = node->right;
    }
    free(node);
    return(new_node);
}

struct node *btree_insert(struct node *node, int key, int data) {
    if (node == NULL) {
        return(_new_node(key, data));
    }
    if (key <= node->key) {
        node->left = btree_insert(node->left, key, data);
    } else {
        node->right = btree_insert(node->right, key, data);
    }
    return(node);
}

int btree_lookup(struct node *node, int target) {
    if (node == NULL) {
        return(false);
    }
    if (target == node->key) {
        return(node->data);
    }
    if (target < node->key) {
        return(btree_lookup(node->left, target));
    }
    return(btree_lookup(node->right, target));
}

struct node *btree_remove(struct node *node, int key) {
    if (node == NULL) { /* No node */
        return(NULL);
    } else if (node->key == key) { /* Root node */
        struct node *new_node = _remove_node(node);
        return(new_node);
    } else if (btree_lookup(node, key)) { /* Child/leaf node */
        struct node *parent = _parent(node, key);
        struct node *old = malloc(sizeof(node));
        if (parent->left->key == key) {
            old = parent->left;
            parent->left = _remove_node(old);
        } else {
            old = parent->right;
            parent->right = _remove_node(old);
        }
    }
    return(node);
}

int btree_size(struct node *node) {
    if (node == NULL) {
        return(0);
    }
    return(btree_size(node->left) + 1 + btree_size(node->right));
}

int btree_depth(struct node *node) {
    if (node == NULL) {
        return(0);
    }
    int depth_left = btree_depth(node->left);
    int depth_right = btree_depth(node->right);
    if (depth_left >= depth_right) {
        return(depth_left + 1);
    } else {
        return(depth_right + 1);
    }
}

