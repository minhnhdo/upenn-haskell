{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Week05 where

import Control.Monad ((>=>))
import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.Map.Strict (Map)

import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Week05.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret encrypted original = do
  encryptedContent <- BS.readFile encrypted
  originalContent <- BS.readFile original
  return . BS.filter (/= 0) . BS.pack $ BS.zipWith xor encryptedContent originalContent

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key filePath = do
  encryptedContent <- BS.readFile (filePath ++ ".enc")
  BS.writeFile filePath (BS.pack $ BS.zipWith xor (BS.cycle key) encryptedContent)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = BS.readFile >=> return . decode

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFilePath transactionFilePath = do
  maybeVictims <- (parseFile victimFilePath) ::IO (Maybe [TId])
  maybeTransactions <- parseFile transactionFilePath
  return $ do
    victims <- maybeVictims
    transactions <- maybeTransactions
    return $ filter (flip elem victims . tid) transactions

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr flows Map.empty
  where flows t = Map.insertWith (+) (to t) (amount t) .
                  Map.insertWith (+) (from t) (negate $ amount t)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . L.maximumBy (compare `on` snd) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m = zipWith mkTransaction (go [] (reverse $ L.sortBy (compare `on` snd) payers) (reverse $ L.sortBy (compare `on` snd) payees))
  where (payers, payees) = L.partition ((< 0) . snd) . filter ((/= 0) . snd) $ Map.toList m
        go acc _ [] = acc
        go acc [] _ = acc
        go acc ((payer,negativeAmount):sortedPayers) ((payee,positiveAmount):sortedPayees) =
          let amt = min (negate negativeAmount) positiveAmount
          in if amt == positiveAmount
               then go ((payer, payee, amt):acc) ((payer,negativeAmount+amt):sortedPayers) sortedPayees
               else go ((payee, payer, amt):acc) sortedPayers ((payee, positiveAmount-amt):sortedPayees)
        mkTransaction (payer, payee, amt) i = Transaction { from = payer
                                                          , to = payee
                                                          , amount = amt
                                                          , tid = i
                                                          }

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON filePath = BS.writeFile filePath . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)
