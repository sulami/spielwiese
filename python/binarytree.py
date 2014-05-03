#!/usr/bin/python3

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

    def _get(self, key):
        """Returns Node object for a given key"""
        if key == self.key:
            return self
        if key < self.key:
            if self.left is None:
                return None
            return self.left._get(key)
        else:
            if self.right is None:
                return None
            return self.right._get(key)

    def _del_get(self, key):
        """Returns the parent of a node for tree reordering on deletion"""
        if self.left is not None:
            if self.left.key == key:
                return self
        if self.right is not None:
            if self.right.key == key:
                return self
        if key < self.key:
            if self.left is None:
                return None
            return self.left._del_get(key)
        else:
            if self.right is None:
                return None
            return self.right._del_get(key)

    def _get_left(self, results):
        """Returns the path when always going left"""
        results.append(self)
        left = self.left
        if left is None:
            return results
        return self.left._get_left(results)

    def _get_right(self, results):
        """Returns the path when always going right"""
        results.append(self)
        right = self.right
        if right is None:
            return results
        return self.right._get_right(results)

    def _multi_get(self, key, results):
        """Actually fetches for multi_lookup"""
        me = self._get(key)
        if me is None:
            return None
        results.append(me)
        left = me.left
        if left is not None:
            left_key = left._get(key)
            if left_key is not None:
                results.append(left_key)
        right = me.right
        if right is not None:
            right_key = right._get(key)
            if right_key is not None:
                results.append(right_key)
        return results

    def _ordered_get(self, results):
        """Actually fetches for ordered_lookup"""
        if self.left is not None:
            self.left._ordered_get(results)
        results.append((self.key, self.data))
        if self.right is not None:
            self.right._ordered_get(results)
        return results

    def _delete(self):
        """Reorders the tree and deletes node"""
        pass

    def lookup(self, key):
        """Returns data for the first matching key"""
        result = self._get(key)
        if result is not None:
            return result.data
        return None

    def multi_lookup(self, key):
        """Returns a list of all data matching the key"""
        results = self._multi_get(key, [])
        if results is None:
            return None
        datas = []
        for result in results:
            datas.append(result.data)
        return datas

    def ordered_lookup(self):
        """Returns all items ordered by key in a list of tuples"""
        return self._ordered_get([])

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

    def remove(self, key):
        """Removes an entry from the tree, returns None on failure"""
        if self.key == key:
            # TODO delete root node
            pass
        parent = self._del_get(key)
        if parent is None:
            return None
        return parent

    def multi_remove(self, key):
        """Removes all occurences of an entry, returns None on failure"""
        results = self._multi_get(key, [])
        if results is not None:
            for result in results:
                result._delete()

