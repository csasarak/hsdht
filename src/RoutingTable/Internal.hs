
module RoutingTable.Internal where

import Node
import Util

-- | The limits for a particular bucket. The first item in the pair is the min
--   ID for the bucket, the second is the max id.
-- Limits should be compared thusly: min <= some_hash < max
type BucketLimit = (Integer, Integer)

data Bucket =
  Bucket { getLimits :: BucketLimit, -- ^ 'BucketLimit' for this Bucket.
           getNodes :: [Node], -- ^ List of nodes in the Bucket
           getSize :: Int -- ^ The number of nodes currently in the bucket
         }
  deriving (Show)

-- | A table of buckets which contain the nodes
data RoutingTable =
  RoutingTable { getBuckets :: [Bucket] -- ^ The Buckets currently in this RoutingTable
  }

-- | This function determines if a Node belongs in a particular Bucket
nodeBelongs :: Bucket -> Node -> Bool
nodeBelongs b n = betweenLimits l1 l2 nodeHash 
                  where nodeHash = nodeId n
                        (l1, l2) = getLimits b

-- | Check that two NodeHashes are between each other
betweenLimits :: Integer -> Integer -> Integer -> Bool
betweenLimits l1 l2 nodeHash = l1 <= nodeHash && nodeHash < l2 

-- | Like betweenLimits, but accepting a BucketLimit
withinLimits :: BucketLimit -> Integer -> Bool
withinLimits = uncurry betweenLimits

-- | The maximum number of Nodes per Bucket
maxBucketNodes :: Int
maxBucketNodes = 8

-- | Checks whether a the number of 'Node's in a Bucket is within 'maxBucketNodes'
bucketIsFull :: Bucket -> Bool
bucketIsFull = (< maxBucketNodes) . getSize 

-- | Checks whether all 'Node's in a 'Bucket' are marked as Good.
allGood :: [Node] -> Bool
allGood = all isGood 

-- | Create an empty bucket with limits 0 and 'maxIdSpace'
defaultEmptyBucket :: Bucket
defaultEmptyBucket = emptyBucket 0 maxIdSpace
  
-- | Returns an empty bucket with the given 'BucketLimit's
emptyBucket :: Integer -> Integer -> Bucket 
emptyBucket lower upper = Bucket (lower, upper) [] 0

-- | This function will either:
-- 1. Add node to a 'Bucket'
--    Might need to split into multiple 'Bucket's, or replace a bad node
-- 2. Return 'Bucket' unchanged (if node doesn't belong there)
-- 3. Return two 'Bucket's if the 'Bucket' was split
addNodeToBucket :: Node -> Bucket -> [Bucket]
addNodeToBucket n b@(Bucket lim@(l, h) ns s)
    -- node doesn't belong
    | not $ nodeBelongs b n = [b] 
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

-- | splits a bucket in half evenly and reassign 'Node's, Returning two new buckets.
splitBucket :: Bucket -> [Bucket]
splitBucket (Bucket (l, h) rn s) = [Bucket (l1, h1) n1s $ length n1s, 
                                    Bucket (l2, h2) n2s $ length n2s]
        where h1 = h `div` 2 
              l2 = h1
              l1 = l
              h2 = h
              filterFn l h = betweenLimits l h . nodeId
              n1s = filter (filterFn l1 h1) rn
              n2s = filter (filterFn l2 h2) rn
