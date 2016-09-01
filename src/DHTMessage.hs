{-# LANGUAGE GADTs #-}
module DHTMessage where

import qualified Data.Map as M
import Data.Maybe
import Bencode
import Node

type TransactionId = Integer
type InfoHash = Integer
type Port = Int

-- |MessageType represents a type of DHT message along with any
-- necessary parameters for that type of query
data MessageType = Query QueryMethod NodeId -- ^ NodeId is the querying Node's ID
                 | Response 
                 | Error 

-- |Method for querying the DHT, along with data specific to that
-- method
data QueryMethod = Ping 
                 | FindNode NodeId -- ^ NodeId is the target ID to find
                 | GetPeers InfoHash -- ^ The InfoHash to search for
                 | AnnouncePeer InfoHash Port Bool -- ^ Probably won't use this too much

instance Show MessageType where
    show (Query _ _)  = "q"
    show Response = "r"
    show Error     = "e"
    
data DHTMessage = DHTMessage 
                   { transactionId :: TransactionId,
                     messageType :: MessageType
                   }

--instance Bencodable DHTMessage where
--    toBencoding DHTMessage { queryingNode = nId, 
--                             messageType = mT, 
--                             contents = c } = undefined
--                           where nId' = toBencoding nId
--                                 mT' = toBencoding mT
