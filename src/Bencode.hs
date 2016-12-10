{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Module      : Bencode 
  Description : Implementation of Bencoding for bittorrent as described at http://www.bittorrent.org/beps/bep_0003.html
  Copyright   : (c) Christopher Sasarak, 2014
  License     : GPL-3
  Maintainer  : cms5347@rit.edu
  Stability   : experimental
  
 -}

module Bencode where

import Text.Parsec
import Text.Parsec.ByteString
import qualified Text.Parsec.Error as PE
import Data.Char
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad as Mon
import qualified Control.Applicative


-- BUG: This type doesn't yet reject improper Bencoded values, e.g.
-- BMap should only accept Bstrs as key values.
-- | A map from Bencode data to Bencode data
type BMap = M.Map Bencode Bencode

-- | Type representing a Bencoded value
data Bencode =  -- |Constructor for a Bencoded Integer
                Bint Integer
                -- |Constructor for a Bencoded String
              | Bstr String
                -- |Constructor for a list of Bencoded items
              | Blist [Bencode]
                -- |Constructor for a Bencoded Map (dictionary)
              | Bdict BMap
              deriving (Eq, Ord)

instance Show Bencode where
    show (Bint i) = "i" ++ show i ++ "e"
    show (Bstr s) = (show . length) s ++ ":" ++ s
    show (Blist bs) = 'l':concatMap show bs ++ "e"
    show (Bdict bm) = (foldl (\a (k, v) -> a ++ show k ++ show v) "d" . M.toAscList $ bm) ++ "e"

-- | Class for things which can be represented as a 'Bencode'
class Bencodable a where
    -- | Convert an a to a Bencode
    toBencoding :: a -> Bencode

-- | Inverse of 'Bencodable'
class Bdecodable a where
    -- | Convert an Bencode datum into an a
    fromBencoding :: Bencode -> a

instance Bencodable String where
    toBencoding s = Bstr s

instance Bencodable Integer where
    toBencoding i = Bint i

instance Bencodable [Bencode] where
    toBencoding l = Blist l

-- |Parser for a Bencoded Integer
bInt :: Parser Bencode
bInt = Bint <$> (char 'i' *> validNum <* char 'e' )
       -- This parser parses valid integers in Bencodings 
       where validNum = do neg <- option ' ' (char '-')
                           d <- digit
                           case digitToInt d of
                                -- Only time the first digit == 0 is "i0e"
                                0 -> if neg == ' ' then 
                                        -- "i0e" allowed but NOT "i-0e" or zero padded integer
                                        lookAhead (char 'e') >> return 0 
                                     else
                                        parserFail "Can't have a negative zero"
                                _ -> many digit >>= \xs -> return $ read (neg:d:xs)
       
-- |Parser for a Bencoded String
bString :: Parser Bencode
bString = do ss <- (many1 digit <* char ':')
             let size = read ss
             Mon.liftM Bstr $ count size anyChar
             
-- |Parser for a Bencoded list
bList :: Parser Bencode
bList = Blist <$> p
  where list_items = many (bInt <|> bString <|> bList <|> bMap)
        p = char 'l' *> list_items <* char 'e'

-- |Parser for a Bencoded dictionary
bMap :: Parser Bencode
bMap = (Bdict . M.fromList) <$> entries
  where entries = char 'd' *> many dictEntry <* char 'e'

-- |Parser for a key-value pair
dictEntry :: Parser (Bencode, Bencode)
dictEntry = (,) <$> key <*> value
  where key = bString
        value = bString <|> bList <|> bInt <|> bMap

parseBencodedByteString :: BS.ByteString -> Either ParseError Bencode 
parseBencodedByteString = parse bMap "" 

-- |Read a Bencoded dictionary from filename
readBencodedFile :: String -> IO (Either PE.ParseError Bencode)
readBencodedFile = parseFromFile bMap
