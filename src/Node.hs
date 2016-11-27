{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

-- Will need to lock this down a bit
module Node where 

import Bencode
import Control.Applicative
import Control.Monad.State
import System.Random
import Data.Word
import Data.Bits
import Util

type NodeId = Integer

-- Might want to put this in a config file or something
nodeIdBytes :: Int
nodeIdBytes = 20

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

-- |Generates a node with a new NodeId given a random number generator,
-- also returns the advanced rng.
genNode :: (RandomGen g) => g -> (Node, g)
genNode gen = (Node id Good, gen')
              where id = wordListToInteger . take (nodeIdBytes `div` 2) $ rs
                    (rs, gen') = runState (replicateM nodeIdBytes statefulRng) $ gen
                    
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
newNode = fst . genNode <$> getStdGen

