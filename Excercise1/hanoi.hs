{-
The Towers of Hanoi is a classic puzzle with a solution
that can be described recursively. Disks of different sizes are stacked
on three pegs; the goal is to get from a starting configuration with
all disks stacked on the first peg to an ending configuration with all
disks stacked on the last peg.

The only rules are
1 you may only move one disk at a time, and
2 a larger disk may never be stacked on top of a smaller one.
For example, as the first move all you can do is move the topmost,
smallest disk onto a different peg, since only one disk may be moved
at a time.
From this point, it is illegal to move to the configuration, 
because you are not allowed to put the green disk on top of
the smaller blue one.
To move n discs (stacked in increasing size) from peg a to peg b
using peg c as temporary storage,
1. move n - 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n - 1 discs from c to b using a as temporary storage.
-}

main = print $ hanoi 3 "a" "b" "c"

type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi num pS pD pT
	| num == 1 = [(pS, pD)]
	| otherwise = ( hanoi (num - 1) pS pT pD ) ++ ((pS, pD) : ( hanoi (num-1) pT pD pS ))