-- CMS: Why does this exist?
module HsDHT.Lib
    ( someFunc
    ) where

import HsDHT.Node
import HsDHT.RoutingTable 
import HsDHT.Bencode
import HsDHT.DHTSocket
import HsDHT.DHTContext
import HsDHT.DHTMessage

someFunc :: IO ()
someFunc = putStrLn "someFunc"
