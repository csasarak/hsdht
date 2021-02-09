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
import Data.Char (ord)

bencodeTests :: [SpecWith ()]
bencodeTests = [bIntTests
               , bStrTests]

randomBInt :: (MonadGen m) => m Bencode
randomBInt = do
    i <- Gen.integral (Range.linear (-10000) 10000)
    return $ Bint i

randomBstr :: (MonadGen m) => m Bencode
randomBstr = Bstr . BS.fromStrict <$> Gen.bytes (Range.linear 20 5000)

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
  it "Parses arbitrary integers" $ hedgehog $ do
    i <- forAll randomBInt
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
