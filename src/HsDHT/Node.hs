{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

-- Will need to lock this down a bit
module HsDHT.Node where 

import HsDHT.Bencode
import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import System.Random
import HsDHT.Util

-- Should probably find a way to enforce the following, also, naming?
-- | NodeIds are 20 bit integer
type NodeId = Integer

-- | DHTHashes are the same as NodeIds, this type is more for documentation.
type DHTHash = Integer

-- Might want to put this in a config file or something
-- | The number of bytes a NodeId should be
nodeIdBytes :: Int
nodeIdBytes = 20

nodeIdBits :: Int
nodeIdBits = 8 * nodeIdBytes

-- CMS: Still up in the air about whether nodeStatus should be
-- another type, or inside the Node.
-- | Type Representing a Node
data Node =
  Node { -- | The 160 bit globally unique ID for this Node
         nodeId :: NodeId, 
         -- | Node status, see 'NodeRating'
         nodeStatus :: NodeRating
       }
          deriving (Show)

-- | Nodes are equal if their ids are equal
instance Eq Node where
  Node {nodeId=i1} == Node {nodeId=i2} = i1 == i2

instance Ord Node where 
    a <= b = nodeId a <= nodeId b

instance Bencodable Node where
    toBencoding (nodeId -> i) = Bint i

-- | Generate a new good node
newNode :: NodeId -> Node
newNode nId = Node nId Good

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
isGood _ = False

-- | Report if a Node is Bad
isBad :: Node -> Bool
isBad (nodeStatus -> Bad) = True
isBad _ = False

-- | Report if a Node is Questionable
isQuestionable :: Node -> Bool
isQuestionable (nodeStatus -> Questionable) = True
isQuestionable _ = False

updateRating :: NodeRating -> Node -> Node
updateRating r node = node {nodeStatus = r}

-- |Generates a node with a new NodeId given a random number generator,
-- also returns the advanced rng.
genNode :: (RandomGen g) => g -> (Node, g)
genNode gen = (Node nId Good, gen')
              where nId = wordListToInteger . take (nodeIdBytes `div` 2) $ rs -- I question this div 2 part
                    (rs, gen') = runState (replicateM nodeIdBytes statefulRng) gen
                    
-- | Base hash compare function, simply XORs two Integers/NodeIds together
compareHashes :: Integer -> Integer -> Integer
compareHashes = xor

-- | Distance between two nodes
nodeDistance :: Node -> Node -> DHTHash
nodeDistance n1 n2 = compareHashes (nodeId n1) (nodeId n2)

-- | Distance between a node and an arbitrary hash
nodeHashCmp :: Node -> DHTHash -> DHTHash
nodeHashCmp n = compareHashes (nodeId n)

