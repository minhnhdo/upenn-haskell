module Main where

import Control.Monad.Random (evalRandIO)
import HW07 (State (..), newDeck, repl)

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
