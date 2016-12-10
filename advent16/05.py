# Can't be asked to pull MD5 into Haskell, so Python it is
# Use `echo -n` to just pipe the door id into this

from itertools import count
from hashlib import md5
from sys import stdin

def find_password(door_id):
    password = ''
    for i in count():
        hashed = md5((door_id + str(i)).encode()).hexdigest()
        if hashed[:5] == '00000':
            password += hashed[5]
            if len(password) == 8:
                print(password)
                break


def find_password2(door_id):
    password = list('________')
    for i in count():
        hashed = md5((door_id + str(i)).encode()).hexdigest()
        if hashed[:5] == '00000':
            pos = hashed[5]
            if pos in '01234567' and password[int(pos)] == '_':
                password[int(pos)] = hashed[6]
                if '_' not in password:
                    print(''.join(password))
                    break


def main():
    indata = stdin.read()
    find_password(indata)
    find_password2(indata)

if __name__ == '__main__':
    main()
