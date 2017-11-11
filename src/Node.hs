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

-- Should probably find a way to enforce the following, also, naming?
-- | NodeIds are 20 bit integer
type NodeId = Integer

-- | DHTHashes are the same as NodeIds, this type is more for documentation.
type DHTHash = Integer

-- Might want to put this in a config file or something
-- | The number of bytes a NodeId should be
nodeIdBytes :: Int
nodeIdBytes = 20

nodeIdBits = 8 * nodeIdBytes

maxIdSpace :: Integer
maxIdSpace = 2 ^ (fromIntegral nodeIdBits)

-- CMS: Still up in the air about whether nodeStatus should be
-- another type, or inside the Node.
-- | Type Representing a Node
data Node =
  Node { -- | The 160 bit globally unique ID for this Node
         nodeId :: NodeId, 
         -- | Node status, see 'NodeRating'
         nodeStatus :: NodeRating
       }
          deriving (Eq, Show)

instance Ord Node where 
    a <= b = (nodeId a) <= (nodeId b)

instance Bencodable Node where
    toBencoding (nodeId -> i) = Bint i

-- Maybe add data for when we've contacted a Node once with no response?
-- | The rating of a node
data NodeRating =
  -- | This node has been contacted in the last 15 minutes
  Good 
  -- | This node has not responded to multiple queries in a row
  | Bad 
  -- | 15 minutes of inactivity, this node's status is Questionable
  | Questionable 
     deriving (Eq, Show)

-- These seem to be of questionable utility
-- | Report if a Node is Good
isGood :: Node -> Bool
isGood (nodeStatus -> Good) = True
isGood otherwise = False

-- | Report if a Node is Bad
isBad :: Node -> Bool
isBad (nodeStatus -> Bad) = True
isBad otherwise = False

-- | Report if a Node is Questionable
isQuestionable :: Node -> Bool
isQuestionable (nodeStatus -> Questionable) = True
isQuestionable otherwise = False

-- |Generates a node with a new NodeId given a random number generator,
-- also returns the advanced rng.
genNode :: (RandomGen g) => g -> (Node, g)
genNode gen = (Node id Good, gen')
              where id = wordListToInteger . take (nodeIdBytes `div` 2) $ rs -- I question this div 2 part
                    (rs, gen') = runState (replicateM nodeIdBytes statefulRng) $ gen
                    
-- | Base hash compare function, simply XORs two Integers/NodeIds together
compareHashes :: Integer -> Integer -> Integer
compareHashes i1 i2 = i1 `xor` i2

-- | Distance between two nodes
nodeDistance :: Node -> Node -> DHTHash
nodeDistance n1 n2 = compareHashes (nodeId n1) (nodeId n2)

-- | Distance between a node and an arbitrary hash
nodeHashCmp :: Node -> DHTHash -> DHTHash
nodeHashCmp n h = compareHashes (nodeId n) h

-- | Generates a new node using the System RNG
newNode :: IO Node
newNode = fst . genNode <$> getStdGen

