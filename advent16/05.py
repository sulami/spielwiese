# Can't be asked to pull MD5 into Haskell, so Python it is
# Use `echo -n` to just pipe the door id into this

from itertools import count
from hashlib import md5
from sys import stdin

def main():
    indata = stdin.read()
    password = ''
    for i in count():
        hashed = md5((indata + str(i)).encode()).hexdigest()
        if hashed[:5] == '00000':
            password += hashed[5]
            if len(password) == 8:
                print(password)
                break

if __name__ == '__main__':
    main()
