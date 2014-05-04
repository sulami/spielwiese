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

static int _left_path(struct node *node) {
    if (node->right != NULL) {
        return(1 + _left_path(node->right));
    } else {
        return(0);
    }
}

static int _right_path(struct node *node) {
    if (node->left != NULL) {
        return(1 + _right_path(node->left));
    } else {
        return(0);
    }
}

static struct node *_remove_node(struct node *node) {
    struct node *new_node = malloc(sizeof(node));
    if ((node->left != NULL) && (node->right != NULL)) {
        int left_side = _left_path(node->left);
        int right_side = _right_path(node->right);
        if (left_side <= right_side) {
            /* left side is shorter, attach right to left */
            new_node = node->left;
            struct node *leaf = node->left;
            while (leaf->right != NULL) {
                leaf = leaf->right;
            }
            leaf->right = node->right;
        } else {
            /* right side is shorter, attach left to right */
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
    return(new_node);
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
        /* No node */
        return(NULL);
    }
    if (node->key == key) {
        /* Root node */
        struct node *new_node = _remove_node(node);
        free(node);
        return(new_node);
    } else if (btree_lookup(node, key)) {
        /* Child node */
        struct node *parent = _parent(node, key);
        struct node *old = malloc(sizeof(node));
        if (parent->left->key == key) {
            old = parent->left;
            parent->left = _remove_node(old);
        } else {
            old = parent->right;
            parent->right = _remove_node(old);
        }
        free(old);
        return(node);
    }
}

int btree_size(struct node *node) {
    if (node == NULL) {
        return(0);
    } else {
        return(btree_size(node->left) + 1 + btree_size(node->right));
    }
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

