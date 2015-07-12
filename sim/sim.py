# Evolution in about 30 LOCs.

from random import seed, randint

def mutate(base, n=4, prob=0.1, rang=0.1):
    newGen = [base, ]
    for i in range(n):
        newEnt = {}
        for aspect in base.keys():
            if randint(0, 100) / 100 <= prob:
                m = 1 + randint(-prob * 100, prob * 100) / 100
                newEnt[aspect] = base[aspect] * m
            else:
                newEnt[aspect] = base[aspect]
        newGen.append(newEnt)
    return newGen

def test(gen, score):
    best = (None, -99999999)
    for ent in gen:
        s = score(ent)
        if s >= best[1]:
            best = (ent, s)
    return best

def run(base, score, c=10, n=4, prob=0.2, rang=0.1):
    seed()
    line = []
    nbase = base
    for i in range(c):
        gen = mutate(nbase, n=n, prob=prob, rang=rang)
        champ = test(gen, score)
        nbase = champ[0]
        line.append(champ)
    return line

def main():
    origin = {
        'height': 180,
        'weight': 75,
    }

    def score(d):
        return 100 - abs(100 - d['weight']) - abs(200 - d['height'])

    results = run(origin, score, c=10, n=10)
    for i in range(len(results)):
        print("Generation", i, results[i])

if __name__ == "__main__":
    main()

