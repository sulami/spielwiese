# Crossword Validation
#
# Crossword puzzles are played on a grid of white and black squares.
# There are traditional rules about what configuration of white and
# black squares make a legal grid - for example, one such rule is
# that the four corners of the grid must be white.
#
# A rule that is more interesting is that no group of white squares
# may be isolated from any other white square.  More specifically,
# that means that:
#   a) From any white square,
#   b) travelling in the four cardinal directions (up, down, left,
#      and right),
#   c) travelling *only* on white squares,
#   d) any other white square may be reached.
#
# For example, the following configuration is valid:
#
#   0 1 2
# 0 w w w
# 1 b w b
# 2 w w w
#
# While this configuration is not:
#   0 1 2
# 0 w b w
# 1 b w w
# 2 w w w
#
#   w b w
#   w b w
#   w b w
#
# Please write a function that validates this property of a
# crossword grid.

w = 1
b = 0

small_valid_board = [
  [w, w, w],
  [b, w, b],
  [w, w, w]
]

small_invalid_board = [
  [w, b, w],
  [b, w, w],
  [w, w, w]
]

def visit(board, x, y, visited):
    if (x,y) not in visited and board[x][y] == w:
        visited.add((x, y))
        if x > 0:
            visit(board, x-1, y, visited)
        if x < len(board) - 1:
            visit(board, x+1, y, visited)
        if y > 0:
            visit(board, x, y-1, visited)
        if y < len(board) - 1:
            visit(board, x, y+1, visited)
    elif board[x][y] != w:
        pass
    else: # in visited
        pass

def is_valid(board):
    visited = set() # set of (x,y) pairs

    visit(board, 0, 0, visited)
    i = 0
    for line in board:
        for square in line:
            if square == w:
                i += 1
    return i == len(visited)



large_valid_board = [
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [b, b, b, b, b, b, b, b, b, b, b, b, b, w, b, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
  [b, w, b, b, w, w, b, b, b, w, b, b, b, w, w, b],
  [w, w, b, w, b, b, w, w, b, b, w, w, b, w, b, w],
  [w, b, w, w, w, w, w, w, w, w, w, w, w, b, w, w],
  [w, b, w, b, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, b, b, w, b, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
  [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
]



print(is_valid(small_valid_board))
print(is_valid(small_invalid_board))
print(is_valid(large_valid_board))
