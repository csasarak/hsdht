{-# LANGUAGE ViewPatterns #-}
{-|
  Module      : DHTSocket 
  Description : Helper types/functions for interacting with a DHT
  Copyright   : (c) Christopher Sasarak, 2016
  License     : GPL-3
  Maintainer  : chris.sasarak@gmail.com
  Stability   : experimental
  
 -}

-- CMS: This is how I send a message to localhost on port 7900.
-- as an experiment I had netcat running with:
-- nc -ul 127.0.0.1 7900
-- 
-- s <- udpSocket
-- let address = inet_addr "127.0.0.1"
-- address <- inet_addr "127.0.0.1"
-- let sockAddr = SockAddrInet 7900 address
-- sendTo s "hello" sockAddr 

module DHTSocket where

import Network.Socket
import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Char
import System.Random
import Node
import RoutingTable
import Bencode
import DHTContext

-- Return a Socket which can be used to communicate over UDP
udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

