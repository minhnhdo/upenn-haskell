module Main where

import System.Environment (getArgs)

import HW05

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "resources/HW05/dog-original.jpg"
                        "resources/HW05/dog.jpg"
                        "resources/HW05/transactions.json"
                        "resources/HW05/victims.json"
                        "resources/HW05/new-ids.json"
                        "resources/HW05/new-transactions.json"
  putStrLn crim
