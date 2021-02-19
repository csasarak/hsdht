module HsDHT.RoutingTableSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Set as Set
import HsDHT.RoutingTable
import HsDHT.RoutingTable.Internal
import HsDHT.Node

spec :: SpecWith ()
spec =
  parallel $ sequence_ [addNodeToBucketSpec
                       , nodeBelongsSpec
                       , splitBucketSpec]
                       

-- | Generate a node with id in lim
randomNode :: MonadGen m => BucketLimit -> m Node
randomNode (l1, l2) =
  Node
  <$> Gen.integral (Range.constant l1 l2)
  <*> Gen.constant Good

-- | Return a bucket where n nodes are in lim
randomBucket :: MonadGen m => BucketLimit -> Int -> m Bucket
randomBucket lim n =
  do nodes <- Gen.list (Range.singleton n) (randomNode lim)
     return $ defaultEmptyBucket {getNodes = nodes
                                 , getSize = length nodes}

nodeBelongsSpec :: SpecWith ()
nodeBelongsSpec =
  describe "nodeBelongs check" $ do
  let bucket = emptyBucket (1, 10)
  it "Node in limits belongs" $ do
    newNode 1 `shouldSatisfy` nodeBelongs bucket
  it "Node outside upper limit doesn't belong" $ do
    newNode 10 `shouldNotSatisfy` nodeBelongs bucket
  it "Node outside lower limit doesn't belong" $ do
    newNode 0 `shouldNotSatisfy` nodeBelongs bucket
  
splitBucketSpec :: SpecWith ()
splitBucketSpec =
  describe "Splitting buckets works" $ do
  it "Splitting a bucket gets bounds correct and distributes nodes" $ hedgehog $ do
    fullBucket <- forAll $ randomBucket maxIdSpace maxBucketNodes
    let buckets = splitBucket fullBucket
    annotate "There are two buckets"
    length buckets === 2
    annotate "All nodes are unique"
    let nodes = buckets >>= getNodes
    (Set.size . Set.fromList) nodes === length nodes
    annotate "All nodes are within their bucket limits"
    assert $ and (buckets >>= \b -> nodeBelongs b <$> getNodes b)

addNodeToBucketSpec :: SpecWith ()
addNodeToBucketSpec =
  describe "Bucket size" $ do
  
  it "Adds one node to the routing table" $ do
    let RoutingTable{getBuckets=bs} = addNode newRoutingTable (Node 1 Good)
    (length . foldMap getNodes $ bs) `shouldBe` 1

  it "Doesn't add a node to an incorrect bucket" $ do 
    let empty = emptyBucket (0, 10)
    addNodeToBucket (newNode 15) empty `shouldBe` [empty]
  -- it "Real size matches Bucket size parameter" $ hedgehog $ do
  --   nodes <- forAll $ Gen.list (Range.linear 0 50) randomNode
  --   let RoutingTable{getBuckets=bs} = foldr (flip addNode) newRoutingTable nodes
  --   assert $ foldr (\b acc -> acc && length (getNodes b) == bucketSize b) True bs
