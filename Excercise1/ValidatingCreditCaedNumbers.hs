{-
Have you ever wondered how websites validate your credit card
number when you shop online? They don’t check a massive database
of numbers, and they don’t use magic. In fact, most credit providers
rely on a checksum formula for distinguishing valid numbers from
random collections of digits (or typing mistakes).


Using the following steps:

Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled;
the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].
Add the digits of the doubled values and the undoubled digits
from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18.
Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid.
-}


main = print $ isCreditCardValid 4012888888881882


toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev num 
	| num < 0 = []
	| otherwise = mod num 10 : (toDigitsRev $ div num 10)

	
doubleEveryOtherL :: Integer ->  [Integer] -> [Integer]
doubleEveryOtherL _ [] = []
doubleEveryOtherL n (x:xs) 
	| (mod n 2) == 0 = 2*x : (doubleEveryOtherL (n+1) xs )
	| otherwise = x : doubleEveryOtherL (n+1) xs 
	
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) 
	| x >= 10 = (sumDigits $ toDigitsRev x) + sumDigits xs
	| otherwise = x + sumDigits xs
	
isCreditCardValid :: Integer -> Bool
isCreditCardValid cardNum = (mod (sumDigits $ doubleEveryOtherL 1 $ toDigitsRev cardNum) 10) == 0