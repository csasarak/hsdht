import Test.Hspec

import HsDHT.BencodeSpec

main :: IO ()
main = mapM_ hspec bencodeTests
