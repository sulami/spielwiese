#!/usr/bin/python

"""
Implements binary search trees that can hold arbitrary data connected to
keys. Trees are identified by their root-node object. Double keys are
inserted on the left side.
"""

class Node:
    def __init__(self, key, data, left=None, right=None):
        self.key = key
        self.data = data
        self.left = left
        self.right = right

    def __get__(self, key):
        """Returns Node object for a given key"""
        if key == self.key:
            return self
        if key < self.key:
            if self.left is None:
                return None
            return self.left.__get__(key)
        else:
            if self.right is None:
                return None
            return self.right.__get__(key)

    def __multi_get__(self, key, results):
        """Actually fetches for multi_lookup"""
        me = self.__get__(key)
        if me is None:
            return None
        results.append(me.data)
        left = me.left
        if left is not None:
            left_key = left.__get__(key)
            if left_key is not None:
                results.append(left_key.data)
        right = me.right
        if right is not None:
            right_key = right.__get__(key)
            if right_key is not None:
                results.append(right_key.data)
        return results

    def __ordered_get__(self, results):
        """Actually fetches for ordered_lookup"""
        if self.left is not None:
            self.left.__ordered_get__(results)
        results.append((self.key, self.data))
        if self.right is not None:
            self.right.__ordered_get__(results)
        return results

    def lookup(self, key):
        """Returns data for the first matching key"""
        result = self.__get__(key)
        if result is not None:
            return result.data
        return None

    def multi_lookup(self, key):
        """Returns a list of all data matching the key"""
        return self.__multi_get__(key, [])

    def ordered_lookup(self):
        """Returns all items ordered by key in a list of tuples"""
        return self.__ordered_get__([])

    def insert(self, key, data):
        """Inserts key-data pair into tree"""
        if key <= self.key:
            if self.left is None:
                self.left = Node(key, data)
            else:
                self.left.insert(key, data)
        else:
            if self.right is None:
                self.right = Node(key, data)
            else:
                self.right.insert(key, data)

