
module RoutingTable (  newRoutingTable
                     , getLimits 
                     , RoutingTable)
                     where 

import qualified Data.ByteString as BS
import Control.Applicative
import Node
import qualified Util 
import Data.List

maxBucketNodes :: Int
maxBucketNodes = 8

maxIDSpace = 2^160

type BucketLimit = (Integer, Integer)

-- Limits should be compared thusly: min <= hash < max
data Bucket = Bucket { getLimits :: BucketLimit,
                       getNodes :: [Node],
                       getSize :: Int}

data RoutingTable = RoutingTable { getBuckets :: [Bucket] }

-- Returns a bucket with the given limits
emptyBucket :: Integer -> Integer -> Bucket 
emptyBucket lower upper = Bucket (lower, upper) [] 0

-- Generate a new routing table with a single bucket
newRoutingTable :: RoutingTable
newRoutingTable = RoutingTable [emptyBucket 0 $ 2^160]

-- This function determines if a node belongs in a 
-- particular bucket
nodeBelongs :: Bucket -> Node -> Bool
nodeBelongs b n = withinLimits l1 l2 nodeHash 
                  where nodeHash = nodeId n
                        (l1, l2) = getLimits b

withinLimits :: Integer -> Integer -> Integer -> Bool
withinLimits l1 l2 nodeHash = l1 <= nodeHash && nodeHash < l2 

ratedNodeBelongs :: Bucket -> Node -> Bool
ratedNodeBelongs b = (nodeBelongs b) 

bucketIsFull :: Bucket -> Bool
bucketIsFull = (< maxBucketNodes) . getSize 

allGood :: [Node] -> Bool
allGood = all isGood 

-- This function will either:
-- 1. Add node to a bucket
--    Might need to split into multiple buckets, or replace a bad node
-- 2. Return bucket unchanged (if node doesn't belong there)
-- 3. Return two buckets if the bucket was split
addNodeToBucket :: Node -> Bucket -> [Bucket]
addNodeToBucket n b@(Bucket lim@(l, h) ns s)
    -- node doesn't belong
    | not $ ratedNodeBelongs b n = [b] 
    -- node belongs, bucket isn't full 
    | not $ bucketIsFull b = [Bucket lim (n:ns) (s + 1)] 
    -- node belongs doesn't fit, all nodes are good
    | allGood ns = concatMap (addNodeToBucket n) (splitBucket b) 
    -- node belongs, doesn't fit, some nodes not good
    | otherwise = addNodeToBucket n smallerBucket 
        where h1 = h `div` 2 
              l2 = h1
              l1 = l
              h2 = h
              smallerBucket = Bucket lim (Util.removeElem Node.isBad ns) (s - 1)

-- splits a bucket in half evenly generates two 
splitBucket :: Bucket -> [Bucket]
splitBucket (Bucket (l, h) rn s) = [Bucket (l1, h1) n1s $ length n1s, 
                                    Bucket (l2, h2) n2s $ length n2s]
        where h1 = h `div` 2 
              l2 = h1
              l1 = l
              h2 = h
              filterFn l h = withinLimits l h . nodeId 
              n1s = filter (filterFn l1 h1) rn
              n2s = filter (filterFn l2 h2) rn
