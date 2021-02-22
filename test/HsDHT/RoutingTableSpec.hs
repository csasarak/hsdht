module HsDHT.RoutingTableSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Set as Set
import HsDHT.RoutingTable
import HsDHT.Node

spec :: SpecWith ()
spec =
  parallel $ sequence_ []
 
-- it "Adds one node to the routing table" $ do
--     let RoutingTable{getBuckets=bs} = addNode newRoutingTable (Node 1 Good)
--     (length . foldMap getNodes $ bs) `shouldBe` 1
