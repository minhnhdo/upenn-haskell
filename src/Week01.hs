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

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src dst tmp1 tmp2
  | n == 0    = []
  | n == 1    = [(src, dst)]
  | otherwise = let disksToMove = snd (hanoi4Moves !! fromInteger n)
                in hanoi4 disksToMove src tmp1 dst tmp2 ++
                   hanoi (n - disksToMove) src dst tmp2 ++
                   hanoi4 disksToMove tmp1 dst src tmp2
  where hanoi3MoveCount :: Integer -> Integer
        hanoi3MoveCount ndisk = 2^ndisk - 1
        hanoi4Moves :: [(Integer, Integer)]
        hanoi4Moves = (0, 0) : (1, 1) : map calculateHanoi4MoveFor [2..]
        hanoi4MoveCount :: Integer -> Integer -> Integer
        hanoi4MoveCount ndisk x = 2*(fst $ hanoi4Moves !! fromInteger x) + hanoi3MoveCount (ndisk-x)
        calculateHanoi4MoveFor :: Integer -> (Integer, Integer)
        calculateHanoi4MoveFor ndisk = minimum $ map (\x -> (hanoi4MoveCount ndisk x, x)) [1..ndisk-1]
