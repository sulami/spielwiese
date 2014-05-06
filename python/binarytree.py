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

    def _get_parent(self, key):
        """Returns the parent of a node for deletion"""
        if self.left is not None:
            if self.left.key == key:
                return self
        if self.right is not None:
            if self.right.key == key:
                return self
        if key < self.key:
            if self.left is None:
                return None
            return self.left._get_parent(key)
        else:
            if self.right is None:
                return None
            return self.right._get_parent(key)

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

    def _del_left(self):
        """Removes entry on the left side"""
        obj = self.left
        if obj.left is None and obj.right is None:
            self.left = None
        elif obj.left is not None and obj.right is None:
            self.left = obj.left
        elif obj.left is None and obj.right is not None:
            self.left = obj.right
        else:
            if len(obj.left._get_right([])) <= len(obj.right._get_left([])):
                self.left = obj.left
                self.left._get_left([])[-1].left = obj.right
            else:
                self.left = obj.right
                self.left._get_right([])[-1].right = obj.left

    def _del_right(self):
        """Removes entry on the right side"""
        obj = self.right
        if obj.left is None and obj.right is None:
            self.right = None
        elif obj.left is not None and obj.right is None:
            self.right = obj.left
        elif obj.left is None and obj.right is not None:
            self.right = obj.right
        else:
            if len(obj.left._get_right([])) <= len(obj.right._get_left([])):
                self.right = obj.left
                self.right._get_left([])[-1].left = obj.right
            else:
                self.right = obj.right
                self.right._get_right([])[-1].right = obj.left

    def _del_self(self):
        """Removes an entry itself"""
        if self.left is None and self.right is None:
            return None
        elif self.left is not None and self.right is None:
            return self.right
        elif self.left is None and self.right is not None:
            return self.left

    def remove(self, key):
        """Removes an entry, returns new root node or None"""
        if self.key == key:
            return self._del_self()
        else:
            parent = self._get_parent(key)
            if parent is None:
                return None
        if parent.left is not None:
            if parent.left.key == key:
                parent._del_left()
            else:
                parent._del_right()
        else:
            parent._del_right()
        return parent

    def multi_remove(self, key):
        """Removes all matching entries, returns None on failure"""
        results = self._multi_get(key, [])
        if results is not None:
            for result in results:
                result._delete()

