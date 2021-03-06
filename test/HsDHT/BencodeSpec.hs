{-# LANGUAGE OverloadedStrings #-}
module HsDHT.BencodeSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Parsec

import  HsDHT.Bencode as Ben       
import Data.Either (isLeft)
import Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.Map as Map

spec :: SpecWith ()
spec = parallel $ sequence_ [bIntTests
                            , bStrTests
                            , bListTests
                            , bDictTests]

randomAny :: MonadGen m => m Bencode
randomAny =
  Gen.frequency
    [ (100, randomBint)
    , (75, randomBstr)
    , (5, randomBlist)
    , (5, randomBdict)]

randomBint :: (MonadGen m) => m Bencode
randomBint = do
    i <- Gen.integral (Range.linear (-10000) 10000)
    return $ Bint i

randomBstr :: (MonadGen m) => m Bencode
randomBstr = Bstr . BS.fromStrict <$> Gen.bytes (Range.linear 20 200)

randomBlist :: (MonadGen m) => m Bencode
randomBlist = fmap Blist
              . Gen.list (Range.linear 0 20)
              $ randomAny

randomBdict :: MonadGen m => m Bencode
randomBdict =
  let dictEntries =
        (,)
          <$> (BS.fromStrict <$> Gen.bytes (Range.linear 5 50))
          <*> randomAny
   in Bdict . Map.fromList <$> Gen.list (Range.linear 1 25) dictEntries

bIntTests :: SpecWith ()
bIntTests =
  describe "bInt parser" $ do
  it "Parses i0e" $ do
    parse Ben.bInt "" "i0e" `shouldBe` Right (Bint 0)
  it "Parses an integer" $ do
    parse Ben.bInt "" "i1e" `shouldBe` Right (Bint 1)
  it "Parses a negative integer" $ do
    parse Ben.bInt "" "i-1e" `shouldBe` Right (Bint (-1))
  it "Disallows i01e" $ do
    parse Ben.bInt "" "i01e" `shouldSatisfy` isLeft
  it "Disallows i-0e" $ do
    parse Ben.bInt "" "i-0e" `shouldSatisfy` isLeft
  it "Parses arbitrary integers" $ hedgehog $ do
    i <- forAll randomBint
    let parsed = case parse Ben.bInt "" (bencodeToByteString i) of
                   Left _ -> error $ "Error parsing " <> show i 
                   Right b -> b
    parsed === i

bStrTests :: SpecWith ()
bStrTests =
  describe "bStr parsing" $ do
  it "Parses arbitrary strings" $ hedgehog $ do
    s <- forAll randomBstr
    let s' = case parse bString "" . bencodeToByteString $ s of
               Left e  -> error $ "Couldn't parse " <> show s <> " because of " <> show e
               Right v -> v
    s' === s
  it "Fails to parse incorrect lengths" $ hedgehog $ do
    s <- forAll $ BS.fromStrict <$> Gen.bytes (Range.linear 5 1000)
    let len = BB.toLazyByteString . BB.int64Dec $ (BS.length s - 1)
    let s' = case parse bString "" (len <> ":" <> s) of
               Left e -> error  $ "Couldn't parse " <> show s <> " because of " <> show e
               Right v -> v
    s' /== Bstr s

bListTests :: SpecWith ()
bListTests =
  describe "bList parsing" $ do
  it "Parses a list" $ do
    case parse Ben.bList "" "li10e2:aae" of
      Left e -> error $ "parse error" <> show e
      Right v -> v `shouldBe` Blist [Bint 10, Bstr "aa"]
  it "Parses arbitrary lists" $ hedgehog $ do
    ls <- forAll randomBlist
    let ls' = case parse bList "" . bencodeToByteString $ ls of
                Left e  ->  error $ "Couldn't parse " <> show ls <> " because of " <> show e
                Right v -> v
    ls === ls'

bDictTests :: SpecWith ()
bDictTests =
  describe "bDict parsing" $ do
  it "Parses a dict" $ do
    let parsedMap = Ben.Bdict (Map.fromList [ ("key1", Bint 2)
                                            , ("key2", Blist [Bint 1
                                                           , Bstr "hello"])
                                            , ("key3", Bdict $ Map.fromList [("a", Bstr "b")])])
    case parse Ben.bDict "" "d4:key1i2e4:key2li1e5:helloe4:key3d1:a1:bee" of
      Left e -> error $ "parse error" <> show e
      Right v -> v `shouldBe` parsedMap
  it "Parses arbitrary dicts" $ hedgehog $ do
    d <- forAll randomBdict
    case Ben.parseBencodedDict . bencodeToByteString $ d of
      Left e -> error $ "Couldn't parse " <> show d <> " because of " <> show e
      Right d' -> d === d'

