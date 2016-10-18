{-# LANGUAGE GADTs, RecordWildCards #-}
module DHTMessage where

import Numeric (showHex)
import Bencode
import qualified Data.Map as M
import Node

type TransactionId = String
type TransactionIdI = Int -- Integer form of TransactionId
type InfoHash = Integer
type Port = Int
type ImpliedPort = Bool

maxTransactionId :: Int
maxTransactionId = 2^16

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

instance Bencodable DHTMessage where
  toBencoding msg@DHTMessage{..} =
    let msgBase = [(Bstr "t", Bstr transactionId)]
        queryBase = (Bstr "y", Bstr $ show messageType):msgBase
        pingQuery = (Bstr "q", Bstr "ping"):queryBase
        queryArgDict nodeId = Bdict $ M.fromList [(Bstr "id", Bstr $ showHex nodeId "")]
    in
      case messageType of
        (Query Ping nodeId) -> Bdict $ M.fromList $ (Bstr "a", queryArgDict nodeId):pingQuery
