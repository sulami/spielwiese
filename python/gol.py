#!/usr/bin/env python3

import os
import random
import time

def clear():
    if os.name == 'nt':
        os.system('cls')
    else:
        os.system('clear')

class Game():
    def __init__(self, width=80, height=32, length=10, interval=.3):
        self.width = width
        self.height = height
        self.length = length
        self.interval = interval
        self.screen = self.generate_world()

    def live(self):
        while self.length:
            self.print_screen()
            self.gen_next_generation()
            self.length -= 1
            time.sleep(self.interval)

    def test(self):
        self.print_screen()
        self.gen_next_generation()
        self.print_screen()

    def generate_world(self):
        screen = {}
        weight = [
            1,
            0, 0, 0, 0,
            ]
        for y in range(self.height):
            screen[y] = {}
            for x in range(self.width):
                screen[y][x] = random.choice(weight)
        return screen

    def print_screen(self):
        clear()
        for y in range(self.height):
            for x in range(self.width):
                if self.screen[y][x]:
                    print('█', end='')
                else:
                    print(' ', end='')
            print('')

    def gen_next_generation(self):
        nextgen = {}
        for y in range(self.height):
            nextgen[y] = {}
            for x in range(self.width):
                n_alive = 0
                try:
                    if self.screen[y-1][x-1]:
                        n_alive += 1
                except:
                    pass
                try:
                    if self.screen[y-1][x]:
                        n_alive += 1
                except:
                    pass
                try:
                    if self.screen[y-1][x+1]:
                        n_alive += 1
                except:
                    pass
                try:
                    if self.screen[y][x-1]:
                        n_alive += 1
                except:
                    pass
                try:
                    if self.screen[y][x+1]:
                        n_alive += 1
                except:
                    pass
                try:
                    if self.screen[y+1][x-1]:
                        n_alive += 1
                except:
                    pass
                try:
                    if self.screen[y+1][x]:
                        n_alive += 1
                except:
                    pass
                try:
                    if self.screen[y+1][x+1]:
                        n_alive += 1
                except:
                    pass

                if not n_alive:
                    nextgen[y][x] = 0
                elif (n_alive == 2 and self.screen[y][x] == 1) or n_alive == 3:
                    nextgen[y][x] = 1
                else:
                    nextgen[y][x] = 0
        self.screen = nextgen

