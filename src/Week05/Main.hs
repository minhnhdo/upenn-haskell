module Main where

import System.Environment (getArgs)

import Week05

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "resources/Week05/dog-original.jpg"
                        "resources/Week05/dog.jpg"
                        "resources/Week05/transactions.json"
                        "resources/Week05/victims.json"
                        "resources/Week05/new-ids.json"
                        "resources/Week05/new-transactions.json"
  putStrLn crim
