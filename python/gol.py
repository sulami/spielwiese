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
    def __init__(self, width=80, height=32, length=100, interval=.1):
        self.width = width + 20
        self.height = height + 19
        self.length = length
        self.interval = interval
        self.screen = self.generate_world()

    def live(self):
        while self.length:
            self.print_screen()
            self.gen_next_generation()
            self.length -= 1
            time.sleep(self.interval)

    def generate_world(self):
        screen = {}
        weight = [
            1,
            0, 0, 0, 0, 0, 0, 0,
            ]
        for y in range(self.height):
            screen[y] = {}
            for x in range(self.width):
                screen[y][x] = random.choice(weight)
        return screen

    def print_screen(self):
        clear()
        alive = 0
        for y in range(10, self.height - 10):
            for x in range(10, self.width - 10):
                if self.screen[y][x]:
                    print('█', end='')
                    alive += 1
                else:
                    print(' ', end='')
            print('')
        print('Stats: {} frames left, {} of {} dots alive ({}%)'.format(
            self.length,
            alive,
            self.width * self.height,
            round(alive / self.width * self.height / 100, 1)
            ))

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

