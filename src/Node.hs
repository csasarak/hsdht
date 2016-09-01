{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

-- Will need to lock this down a bit
module Node where 

import Bencode
import Control.Applicative
import System.Random
import Data.Word
import Data.Bits

type NodeId = Integer

data Node = Node { nodeId :: NodeId,
                   nodeStatus :: NodeRating }
     deriving (Eq)

instance Ord Node where 
    a <= b = (nodeId a) <= (nodeId b)

instance Bencodable Node where
    toBencoding (nodeId -> i) = Bint i

data NodeRating = Good 
                | Bad 
                | Questionable 
     deriving (Eq, Show)

isGood :: Node -> Bool
isGood (nodeStatus -> Good) = True
isGood otherwise = False

isBad :: Node -> Bool
isBad (nodeStatus -> Bad) = True
isBad otherwise = False

isQuestionable :: Node -> Bool
isQuestionable (nodeStatus -> Questionable) = True
isQuestionable otherwise = False

-- Generates a node with a new ByteString given a random number generator
genNode :: RandomGen g => g -> Node
genNode gen = Node id Good
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
