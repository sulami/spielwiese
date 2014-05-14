from unittest import TestCase
from hal import MemoryController

class MemoryTests(TestCase):
    def setUp(self):
        self.c = MemoryController()

    def test_memories_can_be_added(self):
        m = self.c.new_memory()

    def test_memories_are_remembered(self):
        m = self.c.new_memory()
        self.assertIn(m, self.c.memories)

    def test_memories_can_have_axises(self):
        m = self.c.new_memory(x=3, y=6)
        self.assertEqual(m.x, 3)
        self.assertEqual(m.y, 6)

    def test_memories_inherit_existent_axises(self):
        m = self.c.new_memory(a=1)
        n = self.c.new_memory(b=2)
        self.assertEqual(n.a, 0)

    def test_adding_new_axises_applies_to_all_memories(self):
        m = self.c.new_memory(a=1)
        n = self.c.new_memory(b=2)
        self.assertEqual(m.b, 0)

if __name__ == '__main__':
    unittest.main()

