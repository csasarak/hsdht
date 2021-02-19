
module HsDHT.RoutingTable ( newRoutingTable
                          , getBuckets
                          , getLimits 
                          , RoutingTable
                          , bucketLimits
                          , bucketSize
                          , addNode)
where 

import HsDHT.RoutingTable.Internal

-- | Generate a new routing table with a single bucket
newRoutingTable :: RoutingTable
newRoutingTable = RoutingTable [emptyBucket maxIdSpace]

