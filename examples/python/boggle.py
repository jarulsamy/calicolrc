# Example for searching a Boggle Board to find words
# Recursive solution
# D.S. Blank, Spring 2012

def search(word, board):
    """
    Function to search for a word in a Boggle Board
    which is represented by a 1D list of letters.
    """
    # first, get all of the first letter matches in board:
    starts = [(x,y) for x in range(4) for y in range(4) if board[x + y * 4] == word[0]]
    # Now, for each first letter matches, search around for rest of word:
    for (x,y) in starts:
        if search_around(word, 1, board, x, y, [(x, y)]):
            return True
    # if not found for all possibilities, return False
    return False

def search_around(word, pos, board, x, y, avoid):
    """
    Recursive function to find a letter around a (x,y) in a boggle
    board, given a list of positions not to use again.
        word - word to search for
        pos - position in word of letter to search for
        board - the 1D boogle board
        x, y - the 0 through 3 of the column, row to search around
        avoid - list of (x,y) locations to not consider
    """
    if pos >= len(word): # if no more letters, success!
        return True
    for dx in [-1, 0, 1]: # check letters around on row
        for dy in [-1, 0, 1]: # check letters around on col
            if 0 <= dx + x < 4 and 0 <= dy + y < 4: # in range?
                if board[(dx + x) + (dy + y) * 4] == word[pos]: # correct letter?
                    if ((dx + x), (dy + y)) not in avoid: # not used before?
                        return search_around(word,
                                             pos + 1,
                                             board,
                                             dx + x,
                                             dy + y,
                                             avoid + [(dx + x, dy + y)]) # continue
    # If not found in everything around (x,y) return False:
    return False

## Test:
## b = ['a', 'b', 'e', 'd',
##      'o', 'g', 'l', 'o',
##      'b', 't', 'f', 't',
##      'z', 'g', 'g', 'h']
##
## search("glob", b)