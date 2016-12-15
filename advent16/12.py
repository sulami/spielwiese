class BunnyComputer:
    def __init__(self, instructions):
        self.a = 0
        self.b = 0
        self.c = 0
        self.d = 0
        self.instructions = instructions
        self.pos = 0

    def _access(self, addr):
        if addr in "abcd":
            return self.__dict__[addr]
        return int(addr)

    def exec_instruction(self, instruction):
        parts = instruction.split(' ')
        if parts[0] == 'cpy':
            self.__dict__[parts[2]] = self._access(parts[1])
            self.pos += 1
        elif parts[0] == 'inc':
            self.__dict__[parts[1]] += 1
            self.pos += 1
        elif parts[0] == 'dec':
            self.__dict__[parts[1]] -= 1
            self.pos += 1
        elif parts[0] == 'jnz':
            if self._access(parts[1]):
               self.pos += int(self._access(parts[2]))
            else:
                self.pos += 1

    def run(self):
        while self.pos < len(self.instructions):
            self.exec_instruction(self.instructions[self.pos])


def main():
    with open('12.input', 'r') as fp:
        instructions = fp.read().splitlines()
        bc = BunnyComputer(instructions)
        bc.run()
        print(bc.a)


if __name__ == '__main__':
    main()
