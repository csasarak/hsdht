
import Test.Hspec

import HsDHT.BencodeSpec
import HsDHT.RoutingTableSpec

main :: IO ()
main = mapM_ hspec [bencodeTests
                   , routingTableSpecs]
