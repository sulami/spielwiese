#!/usr/bin/python3

"""
Tests the binarytree class.
"""

import random, binarytree

list = []
for i in range(1000):
    list.append((i, random.random()))
random.shuffle(list)

tree = binarytree.Node(500, "MARKER")
for item in list:
    tree.insert(item[0], item[1])

print("Left:", len(tree._get_left([])))
print("Right:", len(tree._get_right([])))

print(tree.lookup(493))
print(tree.remove(493))

