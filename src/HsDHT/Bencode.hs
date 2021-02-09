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
import Text.Parsec.ByteString.Lazy
import qualified Text.Parsec.Error as PE
import Data.Char
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSChar
import qualified Data.Map as M
import GHC.List (foldl')

-- BUG: This type doesn't yet reject improper Bencoded values, e.g.
-- BMap should only accept Bstrs as key values.
-- | A map from Bencode data to Bencode data
type BMap = M.Map BS.ByteString Bencode

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

bencodeToBuilder :: Bencode -> BB.Builder
bencodeToBuilder (Bint i)   = BB.char8 'i' <> BB.integerDec i <> BB.char8 'e'
bencodeToBuilder (Bstr s)   = BB.int64Dec (BS.length s) <> BB.char8 ':' <> BB.lazyByteString s
bencodeToBuilder (Blist bs) = BB.char8 'l' <> foldMap bencodeToBuilder bs <> BB.char8 'e'
bencodeToBuilder (Bdict bd) = BB.char8 'd' <> body <> BB.char8 'e'
  where ordered = M.toAscList bd
        body    = foldMap (\(k, v) -> BB.lazyByteStringHex k <> bencodeToBuilder v) ordered

-- | Convert a bencoded value to a lazy ByteString
bencodeToByteString :: Bencode -> BS.ByteString 
bencodeToByteString =  BB.toLazyByteString . bencodeToBuilder

-- This is a bit messed up because Strings are not necessarily ByteStrings
instance Show Bencode where
    show (Bint i) = "i" ++ show i ++ "e"
    show (Bstr s) = (show . length) s' ++ ":" ++ s'
      where s'    = BSChar.unpack s
    show (Blist bs) = 'l':concatMap show bs ++ "e"
    show (Bdict bm) = (foldl' (\a (k, v) -> a ++ show k ++ show v) "d" . M.toAscList $ bm) ++ "e"

-- | Class for things which can be represented as a 'Bencode'
class Bencodable a where
    -- | Convert an a to a Bencode
    toBencoding :: a -> Bencode

-- | Inverse of 'Bencodable'
class Bdecodable e a where
    -- | Convert an Bencode datum into an a
    fromBencoding :: Bencode -> Either e a

instance Bencodable String where
    toBencoding s = Bstr . BSChar.pack $ s

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
       
-- | Parser for a bencoded bytestring
parseStr :: Parser BS.ByteString
parseStr = do ss <- many1 digit <* char ':'
              BSChar.pack <$> count (read ss) anyChar

-- | Parser for a bencoded string
bString :: Parser Bencode
bString = Bstr <$> parseStr
             
-- |Parser for a Bencoded list
bList :: Parser Bencode
bList = Blist <$> p
  where list_items = many (bInt <|> bString <|> bList <|> bMap)
        p = char 'l' *> list_items <* char 'e'

-- |Parser for a key-value pair
dictEntry :: Parser (BS.ByteString, Bencode)
dictEntry = (,) <$> parseStr <*> value
  where value = bString <|> bList <|> bInt <|> bMap

-- |Parser for a Bencoded dictionary
bMap :: Parser Bencode
bMap = Bdict . M.fromList <$> entries
  where entries = char 'd' *> many dictEntry <* char 'e'

parseBencodedDict :: BS.ByteString -> Either ParseError Bencode 
parseBencodedDict = parse bMap ""

-- |Read a Bencoded dictionary from filename
readBencodedFile :: String -> IO (Either PE.ParseError Bencode)
readBencodedFile = parseFromFile bMap
