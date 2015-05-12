main = print $ hanoi 3 "a" "b" "c"

type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi num pS pD pT
	| num == 1 = [(pS, pD)]
	| otherwise = ( hanoi (num - 1) pS pT pD ) ++ ((pS, pD) : ( hanoi (num-1) pT pD pS ))