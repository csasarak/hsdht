{-# LANGUAGE OverloadedStrings #-}
module HsDHT.BencodeSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Parsec
import Data.ByteString.Char8 as BS

import HsDHT.Bencode
import Data.Either (isLeft)

bencodeTests :: [SpecWith ()]
bencodeTests = [bIntTests]

bIntTests :: (SpecWith ())
bIntTests =
  describe "bInt parser" $ do
  it "Parses i0e" $ do
    parse bInt "" "i0e" `shouldBe` Right (Bint 0)
  it "Parses an integer" $ do
    parse bInt "" "i1e" `shouldBe` Right (Bint 1)
  it "Parses a negative integer" $ do
    parse bInt "" "i-1e" `shouldBe` Right (Bint (-1))
  it "Disallows i01e" $ do
    parse bInt "" "i01e" `shouldSatisfy` isLeft
  it "Parses arbitrary integers" $ hedgehog $ do
    i <- forAll $ Gen.integral (Range.linear (-10000) 10000)
    let s = BS.pack $ "i" <> show i <> "e"
    let parsed = case parse bInt "" s of
                   Left _ -> error $ "Error parsing " <> show i 
                   Right b -> b

    parsed === Bint i

