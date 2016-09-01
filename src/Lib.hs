module Lib
    ( someFunc
    ) where

import Node
import RoutingTable 
import Bencode
import DHTSocket
import DHTMessage

someFunc :: IO ()
someFunc = putStrLn "someFunc"
