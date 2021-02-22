
module HsDHT.RoutingTable ( newRoutingTable
                          , getBuckets
                          , RoutingTable
                          , addNode)
where 

import HsDHT.Node 
import HsDHT.Bucket

-- | Generate a new routing table with a single bucket
newRoutingTable :: RoutingTable
newRoutingTable = RoutingTable [emptyBucket maxIdSpace]

-- | A table of buckets which contain the nodes
newtype RoutingTable =
  RoutingTable { getBuckets :: [Bucket] -- ^ The Buckets currently in this RoutingTable
               }

addNode :: RoutingTable -> Node -> RoutingTable
addNode (RoutingTable bs) node = RoutingTable $ bs >>= addNodeToBucket node

