module HsDHT.BucketSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import HsDHT.Bucket
import HsDHT.Bucket.Internal
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

-- The following can generate Buckets that aren't normally valid
-- | Return a bucket where n unique nodes are in lim
randomBucket :: MonadGen m => BucketLimit -> Int -> m Bucket
randomBucket lim n =
  do nodes <- Set.toList <$> Gen.set (Range.singleton n) (randomNode lim)
     return $ Bucket lim nodes (length nodes)

bucketSizeMatchesNodes :: Bucket -> Bool
bucketSizeMatchesNodes Bucket {getSize=s
                              , getNodes=ns}
  = length ns == s

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
    let [lowLim, hiLim] = getLimits <$> buckets
    annotate "Lower bucket's limits are as expected"
    lowLim === (fst maxIdSpace, snd maxIdSpace `quot` 2)
    annotate "Higher bucket's limits are as expected"
    hiLim === (snd maxIdSpace `quot` 2, snd maxIdSpace)
    annotate "Bucket size matches the number of nodes"
    assert $ all bucketSizeMatchesNodes buckets

addNodeToBucketSpec :: SpecWith ()
addNodeToBucketSpec =
  describe "Bucket size" $ do
  it "Doesn't add a node to an incorrect bucket" $ do 
    let empty = emptyBucket (0, 10)
    addNodeToBucket (newNode 15) empty `shouldBe` [empty]
  it "Adds node to a non-full bucket" $ do
    let node = newNode 10
    let [b]  = addNodeToBucket node defaultEmptyBucket
    node `shouldSatisfy`  (`elem` getNodes b)
    
  -- it "Real size matches Bucket size parameter" $ hedgehog $ do
    -- nodes <- forAll $ Gen.list (Range.linear 0 50) (randomNode maxIdSpace)
    -- let bs = foldl' (\bs n -> bs >>= addNodeToBucket n) [emptyBucket maxIdSpace] nodes
    -- assert $ all (\b -> length (bucketNodes b) == bucketSize b) bs
