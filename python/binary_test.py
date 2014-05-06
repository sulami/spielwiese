#!/usr/bin/python3

"""
Tests the binarytree class.
"""

import unittest
import random
import binarytree

class TestCases(unittest.TestCase):
    def test_first_node(self):
        tree = binarytree.Node(5, 'Five')
        self.assertEqual(tree.key, 5)
        self.assertEqual(tree.data, 'Five')

    def test_second_node(self):
        tree = binarytree.Node(5, 'Five')
        tree.insert(3, 'Three')
        self.assertEqual(tree.key, 5)
        self.assertEqual(tree.data, 'Five')
        self.assertEqual(tree.left.key, 3)

    def test_third_node(self):
        tree = binarytree.Node(5, 'Five')
        tree.insert(3, 'Three')
        tree.insert(1, 'One')
        self.assertEqual(tree.key, 5)
        self.assertEqual(tree.data, 'Five')
        self.assertEqual(tree.left.key, 3)
        self.assertEqual(tree.left.left.key, 1)

    def test_equal_node(self):
        tree = binarytree.Node(5, 'Five')
        tree.insert(5, 'FiveTwo')
        tree.insert(5, 'FiveThree')
        self.assertEqual(tree.data, 'Five')
        self.assertEqual(tree.left.data, 'FiveTwo')
        self.assertEqual(tree.left.left.data, 'FiveThree')

    def test_lookup_nodes(self):
        tree = binarytree.Node(5, 'Five')
        tree.insert(3, 'Three')
        tree.insert(6, 'Six')
        self.assertEqual(tree.lookup(5), 'Five')
        self.assertEqual(tree.lookup(3), 'Three')
        self.assertEqual(tree.lookup(6), 'Six')

    def test_multi_lookup(self):
        tree = binarytree.Node(5, 'Five')
        tree.insert(5, 'FiveTwo')
        tree.insert(5, 'FiveThree')
        self.assertIn('Five', tree.multi_lookup(5))
        self.assertIn('FiveTwo', tree.multi_lookup(5))
        self.assertIn('FiveThree', tree.multi_lookup(5))

    def test_remove_leaf(self):
        tree = binarytree.Node(5, 'Five')
        tree.insert(6, 'Six')
        tree.remove(6)
        self.assertEqual(tree.left, None)

if __name__ == "__main__":
    unittest.main()

