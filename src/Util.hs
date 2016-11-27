-- These are basic utility functions which don't really have a home
module Util where

import Data.Word
import Data.Bits
import System.Random
import Control.Monad.State

-- | Removes the first instance of a list for which the supplied predicate
-- returns true
removeElem :: (a -> Bool) -> [a] -> [a]
removeElem p xs = a ++ as
                where (a, _:as) = span p xs

wordListToInteger :: [Word8] -> Integer
wordListToInteger = foldl (\acc n -> (acc `shiftL` 8) .|. (toInteger n)) 0

statefulRng :: (RandomGen g, Random v) => State g v
statefulRng = do g <- get
                 let (v, rng') = random g 
                 put rng'
                 return v

