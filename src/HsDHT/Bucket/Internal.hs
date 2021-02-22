
{-|
  Module      : HsDHT.Bucket.Internal
  Description : Internal functions and types for Buckets.
  Copyright   : (c) Christopher Sasarak, 2021
  Stability   : experimental

The types and functions in this module can break invariants enforced by the
non-internal 'HsDHT.Bucket'. You should avoid this module if possible.
 -}

module HsDHT.Bucket.Internal where

import HsDHT.Node

-- | The limits for a particular bucket. The first item in the pair is the min
--   ID for the bucket, the second is the max id.
-- Limits should be compared thusly: min <= some_hash < max
type BucketLimit = (Integer, Integer)

data Bucket =
  Bucket { getLimits :: BucketLimit, -- ^ 'BucketLimit' for this Bucket.
           getNodes :: [Node], -- ^ List of nodes in the Bucket
           getSize :: Int -- ^ The number of nodes currently in the bucket
         }
  deriving (Show, Eq)
