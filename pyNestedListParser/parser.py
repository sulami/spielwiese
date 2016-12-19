# nested lists
# process(“[1,[2,3]]”) => [1, [2, 3]]
from itertools import dropwhile, takewhile

LIST_TOKENS = {
    '[': 'LIST_START',
    ']': 'LIST_END',
    ',': 'COMMA',
}

def process_token(t):
    if t in LIST_TOKENS:
        return LIST_TOKENS[t]
    else:
        return int(t)
    
def build_list(s, l):
    if not s:
        return '', l

    next_token = rest = None
    if s[0] not in LIST_TOKENS:
        next_token = ''.join(list(takewhile(lambda c: c not in LIST_TOKENS, s)))
        rest = ''.join(list(dropwhile(lambda c: c not in LIST_TOKENS, s)))
    else:
        next_token = s[0]
        rest = s[1:]
    token = process_token(next_token)

    if token == 'LIST_START':
        new_rest, inner_list, = build_list(rest, [])
        l.append(inner_list)
        return build_list(new_rest, l)
    elif token == 'LIST_END':
        return rest, l
    elif token == 'COMMA':
        pass
    else:
        l.append(token)
    if s:
        return build_list(rest, l)

def process(s):
    return build_list(s.replace(' ', ''), [])[1][0]

def test(s):
    print(s, end=' => ')
    print(process(s))

test("[1,[2,3]]")
test("[1,[24,3]]")
test("[]")
test("[[]]")
test("[[],[]]")
test("[12345]")
test("[1,[], 2]")
test("[1,[1,[2,3],5],2]")
test("[1, 2, [3, [4, 5], [], 6, 7], 8")
