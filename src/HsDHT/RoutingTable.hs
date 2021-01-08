
module HsDHT.RoutingTable (  newRoutingTable
                     , getLimits 
                     , RoutingTable)
                     where 

import HsDHT.RoutingTable.Internal

-- | Generate a new routing table with a single bucket
newRoutingTable :: RoutingTable
newRoutingTable = RoutingTable [emptyBucket 0 $ 2^(160 :: Integer)]

