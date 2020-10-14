{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
  Module      : Bencode 
  Description : Implementation of Bencoding for bittorrent as described at http://www.bittorrent.org/beps/bep_0003.html
  Copyright   : (c) Christopher Sasarak, 2014
  License     : GPL-3
  Maintainer  : cms5347@rit.edu
  Stability   : experimental
  
 -}

module HsDHT.Bencode where

-- CMS: I really have a lot of problems with the code in this file. Will add commentary
-- and areas for improvement later.

import Text.Parsec
import Text.Parsec.ByteString
import qualified Text.Parsec.Error as PE
import Data.Char
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder
import qualified Data.Map as M


-- BUG: This type doesn't yet reject improper Bencoded values, e.g.
-- BMap should only accept Bstrs as key values.
-- | A map from Bencode data to Bencode data
type BMap = M.Map Bencode Bencode

-- | Abstract Bencoded Value
data Bencode =  -- |Constructor for a Bencoded Integer
                Bint Integer
                -- |Constructor for a Bencoded String
              | Bstr BS.ByteString
                -- |Constructor for a list of Bencoded items
              | Blist [Bencode]
                -- |Constructor for a Bencoded Map (dictionary)
              | Bdict BMap
             deriving (Eq, Ord)

bencodeToBuilder :: Bencode -> Builder
bencodeToBuilder (Bint i)   = char8 'i' <> integerDec i <> char8 'e'
bencodeToBuilder (Bstr s)   = intDec (BS.length s) <> char8 ':' <> byteString s
bencodeToBuilder (Blist bs) = char8 'l' <> foldMap bencodeToBuilder bs <> char8 'e'
bencodeToBuilder (Bdict bd) = char8 'd' <> body <> char8 'e'
  where ordered = M.toAscList bd
        body    = foldMap (\(k, v) -> bencodeToBuilder k <> bencodeToBuilder v) ordered

-- | Convert a bencoded value to a lazy ByteString
bencodeToByteString :: Bencode -> LBS.ByteString
bencodeToByteString = toLazyByteString . bencodeToBuilder

-- This is a bit messed up because Strings are not necessarily ByteStrings
instance Show Bencode where
    show (Bint i) = "i" ++ show i ++ "e"
    show (Bstr s) = (show . length) s' ++ ":" ++ s'
      where s'    = BS.unpack s
    show (Blist bs) = 'l':concatMap show bs ++ "e"
    show (Bdict bm) = (foldl (\a (k, v) -> a ++ show k ++ show v) "d" . M.toAscList $ bm) ++ "e"

-- | Class for things which can be represented as a 'Bencode'
class Bencodable a where
    -- | Convert an a to a Bencode
    toBencoding :: a -> Bencode

-- | Inverse of 'Bencodable'
class Bdecodable e a where
    -- | Convert an Bencode datum into an a
    fromBencoding :: Bencode -> Either e a

instance Bencodable String where
    toBencoding s = Bstr . BS.pack $ s

instance Bencodable Integer where
    toBencoding i = Bint i

instance Bencodable [Bencode] where
    toBencoding l = Blist l

instance Bencodable BMap where
  toBencoding bm = Bdict bm

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
             Bstr <$> BS.pack <$> count size anyChar
             
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
