from itertools import count
from hashlib import md5


def hash(plaintext):
    """Just hash it"""
    return md5(plaintext.encode()).hexdigest()


def search_hashes(salt):
    """Iterator to hash a salt with an increasing integer"""
    for i in count(0, 1):
        yield (i, hash(salt + str(i)))


def solve():
    findings = {}
    for i, cypher in search_hashes('abc'):
        findings[i] = []

        # repeaters2 = [''.join([c for _ in range(5)]) for c in set(cypher)]
        # for rep in repeaters2:
        #     if rep in cypher:
        #         findings[i] += rep[0]

        repeaters = [''.join([c for _ in range(3)]) for c in set(cypher)]
        for rep in repeaters:
            if rep in cypher:
                findings[i] += rep[0]


def main():
    print(solve())


if __name__ == '__main__':
    main()
