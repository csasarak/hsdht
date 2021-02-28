module HsDHT.BucketSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import HsDHT.Bucket
import HsDHT.Bucket.Internal
import HsDHT.Node
import Data.Bifunctor (second)
import Data.List (foldl')

spec :: SpecWith ()
spec =
  parallel $ sequence_ [addNodeToBucketSpec
                       , nodeBelongsSpec
                       , splitBucketSpec]

-- | Generate a node with id in lim
randomNode :: MonadGen m => BucketLimit -> m Node
randomNode (l1, l2) =
  Node
  <$> Gen.integral (Range.constant l1 (l2 - 1)) -- lims are [low, high)
  <*> Gen.choice (Gen.constant <$> [Good
                                   , Questionable
                                   , Bad])

-- | Generate a good node with id in lim
randomGoodNode :: MonadGen m => BucketLimit -> m Node
randomGoodNode lim = updateRating Good <$> randomNode lim

randomBucket :: MonadGen m => m Node -> BucketLimit -> Int -> m Bucket
randomBucket nodeGen lim n =
  do nodes <- Gen.list (Range.singleton n) nodeGen 
     return $ Bucket lim nodes (length nodes)

-- The following can generate Buckets that aren't normally valid
-- | Return a bucket where n nodes are in lim, the caller should make sure
-- the 'BucketLimit' is big enough to avoid collisions.
randomFullGoodBucket :: MonadGen m => BucketLimit -> Int -> m Bucket
randomFullGoodBucket lim = randomBucket (randomGoodNode lim) lim

randomAnyBucket :: MonadGen m => BucketLimit -> m Bucket
randomAnyBucket lim = do
  n <- Gen.integral (Range.linear 0 maxBucketNodes)
  randomBucket (randomNode lim) lim n
  
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

bucketProps :: [Bucket] -> PropertyT IO ()
bucketProps buckets = do
  annotate "All nodes are unique"
  let nodes = buckets >>= getNodes
  (Set.size . Set.fromList) nodes === length nodes
  annotate "All nodes are within their bucket limits"
  assert $ and (buckets >>= \b -> nodeBelongs b <$> getNodes b)
  annotate "Bucket size matches the number of nodes"
  assert $ all bucketSizeMatchesNodes buckets
  where bucketSizeMatchesNodes Bucket {getSize=s
                                      , getNodes=ns}
                               = length ns == s
splitBucketSpec :: SpecWith ()
splitBucketSpec =
  describe "Splitting buckets works" $ do
  it "Splitting buckets correctly sets limits" $ do
    let [  Bucket{getLimits = (l1, h1)}
         , Bucket{getLimits = (l2, h2)}] = splitBucket (emptyBucket (0, 100))
    l1 `shouldBe` 0
    h1 `shouldBe` 50
    l2 `shouldBe` 50
    h2 `shouldBe` 100
  it "Splitting a bucket gets bounds correct and distributes nodes" $ hedgehog $ do
    fullBucket <- forAll $ randomFullGoodBucket maxIdSpace maxBucketNodes
    let buckets = splitBucket fullBucket
    annotate "There are two buckets"
    length buckets === 2
    let [lowLim, hiLim] = getLimits <$> buckets
    annotate "Lower bucket's limits are as expected"
    lowLim === (fst maxIdSpace, snd maxIdSpace `quot` 2)
    annotate "Higher bucket's limits are as expected"
    hiLim === (snd maxIdSpace `quot` 2, snd maxIdSpace)
    bucketProps buckets

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
  it "Buckets split recursively if necessary" $ hedgehog $ do
    let halfSpace = second (`div` 2) maxIdSpace
    bucket  <- forAll $ randomFullGoodBucket halfSpace maxBucketNodes
    newBuckets <- forAll $ addNodeToBucket
                           <$> randomGoodNode maxIdSpace
                           <*> pure (bucket{getLimits = maxIdSpace})
    assert $ length newBuckets >= 2
    bucketProps newBuckets
  it "Keeps bucket properties when adding nodes to buckets" $ hedgehog $ do
    bucket <- forAll $ randomAnyBucket maxIdSpace
    nodes <- forAll $ Gen.list (Range.linear 0 500) (randomGoodNode maxIdSpace)
    let newBuckets = foldl' (\bs n -> bs >>= addNodeToBucket n) [bucket] nodes
    bucketProps newBuckets
