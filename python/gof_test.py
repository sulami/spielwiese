#!/usr/bin/env python3

import unittest
from gol import Game

class golTestCase(unittest.TestCase):
    def setUp(self):
        self.game = Game()

    def test_uses_default_arguments(self):
        self.assertEqual(self.game.width, 100)
        self.assertEqual(self.game.height, 52)
        self.assertEqual(self.game.length, 50)
        self.assertEqual(self.game.interval, .1)

    def test_takes_arguments(self):
        tmp = Game(16, 9, 5, .5)
        self.assertEqual(tmp.width, 36)
        self.assertEqual(tmp.height, 29)
        self.assertEqual(tmp.length, 5)
        self.assertEqual(tmp.interval, .5)

    def test_generates_a_valid_random_start(self):
        self.assertIsNotNone(self.game.screen)
        self.assertIsNotNone(self.game.screen[3])
        self.assertTrue(self.game.screen[5][8] == 0 or
                        self.game.screen[5][8] == 1)

    def Dtest_prints_screen(self):
        self.game.print_screen()

    def test_generates_next_generation(self):
        old = self.game.screen
        self.game.gen_next_generation()
        self.assertNotEqual(old, self.game.screen)

    def test_lives(self):
        # game = Game(width=119, height=60)
        game = Game(width=239, height=65, length=1000, interval=0)
        # game = Game(width=23, height=35, length=50, interval=.1)
        game.live()

if __name__ == '__main__':
    unittest.main()

