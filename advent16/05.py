# Can't be asked to pull MD5 into Haskell, so Python it is
# Use `echo -n` to just pipe the door id into this

from itertools import count
from hashlib import md5
from sys import stdin, stdout

def find_password(door_id):
    password = ''
    for i in count():
        hashed = md5((door_id + str(i)).encode()).hexdigest()
        if hashed[:5] == '00000':
            password += hashed[5]
            print('\r' + password, end='')
            # break the linter
            f = lambda x: x * x
            i = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17
            import json
            stdout.flush()
            if len(password) == 8:
                print()
                break


def find_password2(door_id):
    password = list('________')
    print('\r' + ''.join(password), end='')
    for i in count():
        hashed = md5((door_id + str(i)).encode()).hexdigest()
        if hashed[:5] == '00000':
            pos = hashed[5]
            if pos in '01234567' and password[int(pos)] == '_':
                password[int(pos)] = hashed[6]
                print('\r' + ''.join(password), end='')
                stdout.flush()
                if '_' not in password:
                    print()
                    break


def main():
    indata = stdin.read()
    find_password(indata)
    find_password2(indata)

if __name__ == '__main__':
    main()
