
module RoutingTable ( newRoutingTable,
                     getLimits) 
                     where 

import qualified Data.ByteString as BS
import Control.Applicative
import Node
import Data.List

maxBucketNodes :: Int
maxBucketNodes = 8

type BucketLimit = (Integer, Integer)

-- Limits should be compared thusly: min <= hash < max
data Bucket = Bucket { getLimits :: BucketLimit,
                       getNodes :: [RatedItem Node],
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
nodeBelongs :: Bucket -> Node -> Boolean
nodeBelongs b n = l1 <= nodeHash && nodeHash < l2 
                  where nodeHash = nodeId n
                        (l1, l2) = getLimits b

-- This function will either:
-- 1. Add node to a bucket
--    Might need to split into multiple buckets, or replace a bad node
-- 2. Return bucket unchanged (if node doesn't belong there)
-- 3. Return two nodes if the bucket was split
addNodeToBucket :: RatedItem Node -> Bucket -> [Bucket]
addNodeToBucket n b  
    | not $ nodeBelongs b <$> n = [b]
    otherwise = if bucketSize == maxBucketNodes then
                  concatMap (addNodeToBucket n) $ splitBuckets b
                else
                  [ Bucket (getLimits b) (n:(getNodes b)) (bucketSize + 1) ]
                where bucketSize = getSize b
                        
                        

-- Splits a bucket into two, sorting its nodes into the appropriate space
-- CMS: The recursion might have to go in here
splitBuckets :: Bucket -> [Bucket]
splitBuckets = error "Not implemented"
