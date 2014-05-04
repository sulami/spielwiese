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
        /* No node */
        return(NULL);
    }
    if (node->key == key) {
        /* Root node */
        struct node *new_node = malloc(sizeof(node));
        if ((node->left != NULL) && (node->right != NULL)) {
            int left_side = _left_path(node->left, 0);
            int right_side = _right_path(node->right, 0);
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
        free(node);
        return(new_node);
    } else if (btree_lookup(node, key)) {
        /* Child node */
        struct node *parent = _parent(node, key);
        struct node *old = malloc(sizeof(node));
        if (parent->left->key == key) {
            old = parent->left;
        } else {
            old = parent->right;
        }
        if ((old->left != NULL) && (old->right != NULL)){
            int left_side = _left_path(old->right, 0);
            int right_side = _right_path(old->left, 0);
            if (left_side <= right_side) {
                /* left side is shorter, attach right to left */
                if (parent->left == old) {
                    /* deleted node is left of parent */
                    struct node *old = parent->left;
                    parent->left = old->left;
                    struct node *leaf = old->left;
                    while (leaf->right != NULL) {
                        leaf = leaf->right;
                    }
                    leaf->right = old->right;
                } else {
                    /* deleted node is right of parent */
                    struct node *old = parent->right;
                    parent->right = old->left;
                    struct node *leaf = old->left;
                    while (leaf->right != NULL) {
                        leaf = leaf->right;
                    }
                    leaf->right = old->right;
                }
            } else {
                /* right side is shorter, attach left to right */
                if (parent->left == old) {
                    /* deleted node is left of parent */
                    struct node *old = parent->left;
                    parent->left = old->right;
                    struct node *leaf = old->right;
                    while (leaf->left != NULL) {
                        leaf = leaf->left;
                    }
                    leaf->left = old->left;
                } else {
                    /* deleted node is right of parent */
                    struct node *old = parent->right;
                    parent->right = old->right;
                    struct node *leaf = old->right;
                    while (leaf->left != NULL) {
                        leaf = leaf->left;
                    }
                    leaf->left = old->left;
                }
            }
        } else if ((old->left != NULL) && (old->right == NULL)) {
            if (parent->left == old) {
                parent->left = old->left;
            } else {
                parent->right = old->left;
            }
        } else if ((old->left == NULL) && (old->right != NULL)) {
            if (parent->left == old) {
                parent->left = old->right;
            } else {
                parent->right = old->right;
            }
        }
        free(old);
        return(node);
    }
}

