{-# LANGUAGE DeriveFunctor #-}

module Node where 

import Control.Applicative
import System.Random
import Data.Word
import Data.Bits

data Node = Node { nodeId :: Integer }
     deriving (Eq, Ord)

data RatedItem a = Good a
                 | Bad a
                 | Questionable a
                 deriving (Functor)

-- if you decide to make RatedNode more general
instance Applicative RatedItem where
    (Good f) <*> n         = fmap f n
    (Bad f) <*> n          = fmap f n 
    (Questionable f) <*> n = fmap f n 

    -- Just use Good for pure
    pure a = Good a

-- Generates a node with a new ByteString given a random number generator
genNode :: RandomGen g => g -> Node
genNode gen = Node id
              where id = wordListToInteger . take 20 . randoms $ gen 

-- Base hash compare function, 
compareHashes :: Integer -> Integer -> Integer
compareHashes i1 i2 = i1 `xor` i2

-- Distance between two nodes
nodeDistance :: Node -> Node -> Integer
nodeDistance n1 n2 = compareHashes (nodeId n1) (nodeId n2)

-- Distance between a node and an arbitrary hash
nodeHashCmp :: Node -> Integer -> Integer
nodeHashCmp n h = compareHashes (nodeId n) h

-- Generates a new node using the System RNG
newNode :: IO Node
newNode = genNode <$> getStdGen

wordListToInteger :: [Word8] -> Integer
wordListToInteger = foldl (\acc n -> (acc `shiftL` 8) .|. (toInteger n)) 0
