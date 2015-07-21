module Week01 where

lastDigit :: Integer -> Integer
lastDigit = (`rem` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`quot` 10)

toRevDigits :: Integer -> [Integer]
toRevDigits i
  | i <= 0    = []
  | otherwise = lastDigit i : toRevDigits (dropLastDigit i)

toDigits :: Integer -> [Integer]
toDigits = go []
  where go acc i
          | i <= 0    = acc
          | otherwise = let (q, r) = i `quotRem` 10
                        in go (r : acc) q

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (*2)])

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toRevDigits

luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst tmp
  | n == 0    = []
  | n == 1    = [(src, dst)]
  | otherwise = hanoi (n - 1) src tmp dst ++
                hanoi 1 src dst tmp ++
                hanoi (n - 1) tmp dst src
