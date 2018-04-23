


'''A Python implementation of the Smith-Waterman algorithm for local alignment
of nucleotide sequences.
'''


import argparse
import os
import re
import sys
import unittest


# These scores are taken from Wikipedia.
# en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm
match    = 2
mismatch = -1
gap      = -1
                    
def calltoMain(seq1,seq2):

    # The scoring matrix contains an extra row and column for the gap (-), hence
    # the +1 here.
    rows = len(seq1) + 1
    cols = len(seq2) + 1

    # Initialize the scoring matrix.
    score_matrix, start_pos = create_score_matrix(rows, cols,seq1,seq2)

    # Traceback. Find the optimal path through the scoring matrix. This path
    # corresponds to the optimal local sequence alignment.
    seq1_aligned, seq2_aligned = traceback(score_matrix, start_pos,seq1,seq2)
    assert len(seq1_aligned) == len(seq2_aligned), 'aligned strings are not the same size'

    # Pretty print the results. The printing follows the format of BLAST results
    # as closely as possible.
    alignment_str, idents, gaps, mismatches = alignment_string(seq1_aligned, seq2_aligned)
    alength = len(seq1_aligned)
   
    '''print(' Identities = {0}/{1} ({2:.1%}), Gaps = {3}/{4} ({5:.1%})'.format(idents,
          alength, idents / alength, gaps, alength, gaps / alength))
    print()
    for i in range(0, alength, 60):
        seq1_slice = seq1_aligned[i:i+60]
        print('Query  {0:<4}  {1}  {2:<4}'.format(i + 1, seq1_slice, i + len(seq1_slice)))
        print('             {0}'.format(alignment_str[i:i+60]))
        seq2_slice = seq2_aligned[i:i+60]
        print('Sbjct  {0:<4}  {1}  {2:<4}'.format(i + 1, seq2_slice, i + len(seq2_slice)))
        print()'''
        
    g = -0.4
    s = 1
    d = -0.6
    similarity_score=gaps*g + idents*s + mismatches*d
    return similarity_score

def create_score_matrix(rows, cols,seq1,seq2):
    score_matrix = [[0 for col in range(cols)] for row in range(rows)]

    # Fill the scoring matrix.
    max_score = 0
    max_pos   = (0,0)    # The row and columbn of the highest score in matrix.
    
    for i in range(1, rows):
        for j in range(1, cols):
            score = calc_score(score_matrix, i, j,seq1,seq2)
            if score > max_score:
                max_score = score
                max_pos   = (i, j)

            score_matrix[i][j] = score

    return score_matrix, max_pos


def calc_score(matrix, x, y,seq1,seq2):
    '''Calculate score for a given x, y position in the scoring matrix.
    The score is based on the up, left, and upper-left neighbors.
    '''
    
    similarity = match if seq1[x - 1] == seq2[y - 1] else mismatch

    diag_score = matrix[x - 1][y - 1] + similarity
    up_score   = matrix[x - 1][y] + gap
    left_score = matrix[x][y - 1] + gap
    max_score=max(0, diag_score, up_score, left_score)

    return max_score


def traceback(score_matrix, start_pos,seq1,seq2):
    END, DIAG, UP, LEFT = range(4)    
    aligned_seq1 = []
    aligned_seq2 = []
    x, y         = start_pos
    move         = next_move(score_matrix, x, y)
    while move != END:
        if move == DIAG:
            aligned_seq1.append(seq1[x - 1])
            aligned_seq2.append(seq2[y - 1])
            x -= 1
            y -= 1
        elif move == UP:
            aligned_seq1.append(seq1[x - 1])
            aligned_seq2.append('-')
            x -= 1
        else:
            aligned_seq1.append('-')
            aligned_seq2.append(seq2[y - 1])
            y -= 1

        move = next_move(score_matrix, x, y)

    aligned_seq1.append(seq1[x - 1])
    aligned_seq2.append(seq2[y - 1])

    return aligned_seq1,aligned_seq2


def next_move(score_matrix, x, y):
    diag = score_matrix[x - 1][y - 1]
    up   = score_matrix[x - 1][y]
    left = score_matrix[x][y - 1]
    if diag >= up and diag >= left:     # Tie goes to the DIAG move.
        return 1 if diag != 0 else 0    # 1 signals a DIAG move. 0 signals the end.
    elif up > diag and up >= left:      # Tie goes to UP move.
        return 2 if up != 0 else 0      # UP move or end.
    elif left > diag and left > up:
        return 3 if left != 0 else 0    # LEFT move or end.
    else:
        # Execution should not reach here.
        raise ValueError('invalid move during traceback')


def alignment_string(aligned_seq1, aligned_seq2):
    # Build the string as a list of characters to avoid costly string
    # concatenation.
    idents, gaps, mismatches = 0, 0, 0
    alignment_string = []
   
    for base1, base2 in zip(aligned_seq1, aligned_seq2):
        if base1 == base2:
            alignment_string.append('|')
            idents += 1
        elif '-' in (base1, base2):
            alignment_string.append(' ')
            gaps += 1
        else:
            alignment_string.append(':')
            mismatches += 1

    
    return ''.join(alignment_string), idents, gaps, mismatches
