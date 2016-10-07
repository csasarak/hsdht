module Lib
    ( someFunc
    ) where

import Node
import RoutingTable 
import Bencode
import DHTSocket
import DHTContext
import DHTMessage

someFunc :: IO ()
someFunc = putStrLn "someFunc"
