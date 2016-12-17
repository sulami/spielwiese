from sys import stdin

class Screen:
    def __init__(self):
        self.cells = [[False for _ in range(50)] for _ in range(6)]

    def run(self, cmds):
        while cmds:
            self.exec_cmd(cmds.pop(0))

    def exec_cmd(self, cmd):
        words = cmd.split(' ')
        if words[0] == 'rect':
            a, b = [int(c) for c in words[1].split('x')]
            for y in range(b):
                for x in range(a):
                    self.cells[y][x] = True
        elif words[1] == 'row':
            a = int(words[2].split('=')[1])
            b = int(words[4])
            old = list(self.cells[a])
            for x in range(50):
                self.cells[a][x] = old[x-(b%50)]
        elif words[1] == 'column':
            a = int(words[2].split('=')[1])
            a = int(words[2].split('=')[1])
            b = int(words[4])
            old = [row[a] for row in self.cells]
            for y in range(6):
                self.cells[y][a] = old[y-(b%6)]

    def show(self):
        for y in range(6):
            for x in range(50):
                print('{}'.format('X' if self.cells[y][x] else '.'), end='')
            print()

def main():
    indata = stdin.read().splitlines()
    screen = Screen()
    screen.run(indata)
    print(sum([len([cell for cell in row if cell]) for row in screen.cells]))

if __name__ == '__main__':
    main()
