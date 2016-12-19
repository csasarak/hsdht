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

-- | Turn a list of Word8s into an Integer. Word8 bits will appear
-- in the Integer in the same order as they appear in the list.
wordListToInteger :: [Word8] -> Integer
wordListToInteger = foldl (\acc n -> (acc `shiftL` 8) .|. (toInteger n)) 0

-- | a 'State' action which retrieves a 'Random' value from a RandomGen stored as the State.
-- The Random value is the result of the action. This is mainly useful for generating many random
-- values (see the source of 'Node.genNode' for an example) from the same RandomGen
-- while being able to access the final state.
statefulRng :: (RandomGen g, Random v) => State g v
statefulRng = do g <- get
                 let (v, rng') = random g 
                 put rng'
                 return v

-- | Takes a Maybe and an error message. Converts (Just a) into (Right a). 
--   In case of Nothing, returns (Left e) 
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e
  
