{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
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

module HsDHT.DHTSocket ( newUDPSocket
                 , sendDHTMessage
                 ) where

import HsDHT.Bencode
import Control.Applicative
import HsDHT.DHTContext
import HsDHT.DHTMessage
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BL
import Data.Char
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import HsDHT.Node
import HsDHT.RoutingTable
import System.Random

-- CMS: May eventually need a type to hold and get some data from a context?

-- | The maximum response size to accept from a Socket
maxResponseSize :: Int
maxResponseSize = 4096

-- | Create a 'Socket' for sending messaged via UDP.
newUDPSocket :: IO Socket
newUDPSocket = socket AF_INET Datagram defaultProtocol

-- | Sends a 'DHTMessage' via UDP using a 'Socket' to 'SockAddr'. 
sendDHTMessage :: Socket -> SockAddr -> DHTMessage -> IO ()
sendDHTMessage s addr dhtMsg = sendAllTo s dhtBS addr
  where dhtBS = BL.toStrict . bencodeToByteString . toBencoding $ dhtMsg

-- CMS: My goal here is to eventually hide the UDP Socket pieces and have message emitter/receiver
-- pairs and possibly do the validation/queuing of transactions in here. But more importantly,
-- do the management of sending multiple messages and receiving multiple responses on a single socket
-- without accidentally crossing the wires. 
-- | Returns a pair of functions generating IO actions, one can be used to send
-- a 'DHTMessage' on a particular UDP 'Socket' to a particular address. The other can be used to
-- receive a DHTMessage on a particular UDP Socket from a particular address.
--emissionsActions :: 
