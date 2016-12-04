{-# LANGUAGE GADTs, RecordWildCards #-}
module DHTMessage where

import Numeric (showHex)
import Bencode
import qualified Data.Map as M
import Node

-- | A short (generally two-chars) hexadecimal string transaction id
type TransactionId = String
-- | An Integral form of the transaction id
type TransactionIdI = Int -- Integer form of TransactionId
-- CMS: Think about turning these hashes into ByteStrings
-- | 160-bit InfoHash
type InfoHash = Integer
type Port = Int
-- | BEP-0005 implied_port, if true use UDP packet port and not Node's port
type ImpliedPort = Bool

-- | The number of allowed outstanding transactions at any time. Higher
-- numbered transactions will wrap back around to 0
maxTransactionId :: Int
maxTransactionId = 2^16

-- |MessageType represents a type of DHT message along with any
-- necessary parameters for that type of query
data MessageType =
  -- | A Query message. NodeId is the querying Node's ID
  Query QueryMethod NodeId 
  -- | A message from a foreign node
  | Response  -- payload somewhere in here?
  -- | A message indicating an error
  | Error 

-- |Methods for querying the DHT, along with data specific to that
-- method. See BEP-0005 for more info.
data QueryMethod =
  Ping -- ^ Ping to check if a node is alive still
  | FindNode NodeId -- ^ NodeId is the target ID to find
  | GetPeers InfoHash -- ^ The InfoHash to search for
  | AnnouncePeer InfoHash Port Bool -- ^ Probably won't use this too much

instance Show MessageType where
    show (Query _ _)  = "q"
    show Response = "r"
    show Error     = "e"
    
data DHTMessage =
  DHTMessage {
  -- | The id for this transaction
  transactionId :: TransactionId,
  -- | The type of message this is
  messageType :: MessageType -- there might be a better name for this
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
