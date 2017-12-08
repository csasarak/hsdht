
module HsDHT.RoutingTable (  newRoutingTable
                     , getLimits 
                     , RoutingTable)
                     where 

import HsDHT.RoutingTable.Internal
import qualified Data.ByteString as BS
import Control.Applicative
import HsDHT.Node
import qualified HsDHT.Util as U
import Data.List

-- | Generate a new routing table with a single bucket
newRoutingTable :: RoutingTable
newRoutingTable = RoutingTable [emptyBucket 0 $ 2^160]

