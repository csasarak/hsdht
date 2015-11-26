{-# LANGUAGE DeriveFunctor #-}

module Node where 

import qualified Data.ByteString as BS
-- This is mostly for debugging
import Data.ByteString.Base16 (encode)
import Control.Applicative
import System.Random
import Data.Word
import Data.Bits

data Node = Node { nodeId :: BS.ByteString }
     deriving (Eq, Ord)

data RatedNode a = Good Node
                 | Bad Node
                 | Questionable Node

-- if you decide to make RatedNode more general
--instance Applicative RatedItem where
--    (Good f) <*> n         = fmap f n
--    (Bad f) <*> n          = fmap f n 
--    (Questionable f) <*> n = fmap f n 
--
--    -- Just use Good for pure
--    pure a = Good a

-- Generates a node with a new ByteString given a random number generator
genNode :: RandomGen g => g -> Node
genNode gen = Node id
              where id = BS.pack . take 20 . randoms $ gen 

-- Base hash compare function, 
compareHashes :: BS.ByteString -> BS.ByteString -> BS.ByteString
compareHashes bs1 bs2 = BS.pack $ BS.zipWith xor bs1 bs2

-- Distance between two nodes
nodeDistance :: Node -> Node -> BS.ByteString
nodeDistance n1 n2 = compareHashes (nodeId n1) (nodeId n2)

-- Distance between a node and an arbitrary hash
nodeHashCmp :: Node -> BS.ByteString -> BS.ByteString
nodeHashCmp n h = compareHashes (nodeId n) h

-- Generates a new node using the System RNG
newNode :: IO Node
newNode = genNode <$> getStdGen
