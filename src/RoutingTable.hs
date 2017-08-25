
module RoutingTable (  newRoutingTable
                     , getLimits 
                     , RoutingTable)
                     where 

import RoutingTable.Internal
import qualified Data.ByteString as BS
import Control.Applicative
import Node
import qualified Util 
import Data.List

-- | Generate a new routing table with a single bucket
newRoutingTable :: RoutingTable
newRoutingTable = RoutingTable [emptyBucket 0 $ 2^160]

