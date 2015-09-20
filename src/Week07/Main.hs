module Main where

import Control.Monad.Random (evalRandIO)
import Week07 (State (..), newDeck, repl)

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
