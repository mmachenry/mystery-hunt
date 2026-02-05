# This program was written to solve Knights of the Square Table
# from Mystery Hunt 2025
# https://www.two-pi-noir.agency/puzzles/knights_of_the_square_table

import constraint

arrows = [
  ((0,7),(1,7)),
  ((2,3),(2,2)),
  ((3,6),(3,7)),
  ((4,2),(5,2)),
  ((5,5),(4,5)),
  ((5,4),(5,5)),
  ((5,5),(5,6)),
  ((7,8),(6,8)),
  ]

def not_arrowed(p1,p2):
    return (p1,p2) not in arrows and (p2,p1) not in arrows

def in_bounds(p):
    (row,col) = p
    return row >= 0 and row < 9 and col >= 0 and col < 9

def not_off_by_one(c1, c2):
    return c1 != c2+1 and c1 != c2-1

def solver():
    all_cells = [(r, c) for r in range(9) for c in range(9)]
    possible_val = [1,2,4,5,6,8,9,10,11]
    p = constraint.Problem(constraint.RecursiveBacktrackingSolver())
    p.addVariables(all_cells, possible_val)

    # Regular Sudoku all differnt rows and columns.
    for i in range(9):
        row = [(i, l) for l in range(9)]
        column = [(l, i) for l in range(9)]
        p.addConstraint(constraint.AllDifferentConstraint(), row)
        p.addConstraint(constraint.AllDifferentConstraint(), column)
    
    # Regular Sudoku all different boxes
    box_range = [(0, 3), (3, 6), (6, 9)]
    for i in box_range:
        for l in box_range:
            box = [(a, b) for a in range(*i) for b in range(*l)]
            p.addConstraint(constraint.AllDifferentConstraint(), box)

    # Enforce that none of the adjacent cells have values that are off
    # by one unless they are listed among the arrows.
    for i in range(9):
        for j in range(9):
            this = (i,j)
            to_right = (i,j+1)
            bellow = (i+1,j)

            if in_bounds(to_right) and not_arrowed(this,to_right):
                p.addConstraint(not_off_by_one, (this, to_right))

            if in_bounds(bellow) and not_arrowed(this,bellow):
                p.addConstraint(not_off_by_one, (this, bellow))

    # Enforce that the arrows are all one letter off from the
    # adjacent.
    for arrow in arrows:
        p.addConstraint(lambda c1, c2: c1+1==c2, arrow)

    # Enforce that the one indicated knight move cell pair are the same.
    knight_pair = ((4,7),(6,6))
    p.addConstraint(lambda c1, c2: c1 == c2, knight_pair)

    # Enforce that no cells are a knights move away from a cell
    # with the same number except the one that should be.
    for i in range(9):
        for j in range(9):
            this = (i,j)
            down1_left2 = (i+1,j-2)
            down2_left1 = (i+2,j-1)
            down2_right1 = (i+2, j+1)
            down1_right2 = (i+1, j+2)

            if in_bounds(down1_left2):
                p.addConstraint(lambda c1,c2: c1 != c2, (this,down1_left2))

            if in_bounds(down2_left1) and (this,down2_left1) != knight_pair:
                p.addConstraint(lambda c1,c2: c1 != c2, (this,down2_left1))

            if in_bounds(down2_right1):
                p.addConstraint(lambda c1,c2: c1 != c2, (this,down2_right1))

            if in_bounds(down1_right2):
                p.addConstraint(lambda c1,c2: c1 != c2, (this,down1_right2))

    return p.getSolution()

def convert_to_list(s):
    t = {i: {} for i in range(9)}
    for k, v in s.items():
        t[k[0]][k[1]] = v

    result_table = []
    for k, l in t.items():
        result_table.append([l[i] for i in range(9)])
    return result_table

def print_for_sheets(solution):
    for r in solution:
        print("\t".join(map(lambda v: chr(v+ord('A')-1), r)))

if __name__ == "__main__":
    l = convert_to_list(solver())
    print_for_sheets(l)

