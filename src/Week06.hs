{-# OPTIONS_GHC -Wall #-}
module Week06 where

import Data.List
import Data.Bits (shiftR, testBit)

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat v = Cons v (sRepeat v)

sIterate :: (a -> a) -> a -> Stream a
sIterate f i = let i' = f i
               in Cons i (sIterate f i')

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons h t) st = Cons h (sInterleave st t)

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (fmap divisibleByPowerOfTwo (sIterate (+2) 2))
  where divisibleByPowerOfTwo :: Integer -> Integer
        divisibleByPowerOfTwo 0 = 1
        divisibleByPowerOfTwo n = go 0 n
        go acc 0 = acc
        go acc n = if not (testBit n 0) then go (acc + 1) (shiftR n 1) else acc

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate (\r -> (1103515245*r + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (h:t) = Just $ foldl updateMinMax (h, h) t
  where updateMinMax (mi, ma) x = if x < mi
                                    then (x,  ma)
                                    else if x > ma
                                      then (mi, x)
                                      else (mi, ma)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = M { topLeft :: Integer
                , topRight :: Integer
                , bottomLeft :: Integer
                , bottomRight :: Integer
                }

instance Num Matrix where
  (+) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined
  m1 * m2 = M { topLeft = (topLeft m1) * (topLeft m2) + (topRight m1) * (bottomLeft m2)
              , topRight = (topLeft m1) * (topRight m2) + (topRight m1) * (bottomRight m2)
              , bottomLeft = (bottomLeft m1) * (topLeft m2) + (bottomRight m1) * (bottomLeft m2)
              , bottomRight = (bottomLeft m1) * (topRight m2) + (bottomRight m1) * (bottomRight m2)
              }

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = topRight $ seed ^ n
  where seed = M {topLeft = 1, topRight = 1, bottomLeft = 1, bottomRight = 0}
