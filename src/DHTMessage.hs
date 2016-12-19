{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module DHTMessage where


import Numeric (showHex)
import Bencode
import qualified Data.Map.Lazy as Map
import Text.Parsec 
import Data.ByteString as BS
import Data.Maybe
import Control.Applicative
import Data.Bifunctor
import Data.String
import Node
import Util

-- | A String describing an error
type ErrorString = String
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
  -- | A message from a foreign node, contains a Bencoded dictionary
  -- of response key-value pairs. 
  | Response Bencode 
  -- | A message indicating an error
  | Error 

-- |Methods for querying the DHT, along with data specific to that
-- method. See BEP-0005 for more info.
data QueryMethod =
    Ping -- ^ Ping to check if a node is alive still
  | FindNode NodeId -- ^ NodeId is the target ID to find
  | GetPeers InfoHash -- ^ The InfoHash to search for
  | AnnouncePeer InfoHash Port Bool -- ^ Probably won't use this too much

-- | Return the key for a particular MessageType
messageTypeChar :: MessageType -> String
messageTypeChar (Query _ _)  = "q"
messageTypeChar (Response _) = "r"
messageTypeChar DHTMessage.Error     = "e"
    
data DHTMessage =
  DHTMessage {
  -- | The id for this transaction
  transactionId :: TransactionId,
  -- | The type of message this is
  messageType :: MessageType -- there might be a better name for this
  }

parseDHTMessage :: BS.ByteString -> Either ParseError DHTMessage
parseDHTMessage = undefined
  where bencodedStructure = parse bMap ""

-- This is not yet implemented for all message types
instance Bencodable DHTMessage where
  toBencoding msg@DHTMessage{..} =
    let msgBase = [(Bstr "t", Bstr transactionId)]
        queryBase = (Bstr "y", Bstr $ messageTypeChar messageType):msgBase
        pingQuery = (Bstr "q", Bstr "ping"):queryBase
        queryArgDict nodeId = Bdict $ Map.fromList [(Bstr "id", Bstr $ showHex nodeId "")]
    in
      case messageType of
        (Query Ping nodeId) -> Bdict $ Map.fromList $ (Bstr "a", queryArgDict nodeId):pingQuery

-- This is not yet implemented for all message types
instance Bdecodable ErrorString DHTMessage where
  fromBencoding (Bdict bMsg) = DHTMessage <$> transactionId <*> (genMsg msgType)
    where unwrapString (Bstr s) = s
          -- These values inspect/query the Becoded value for message pieces
          transactionIdB = maybeToEither "No transaction ID" $ Map.lookup (Bstr "t") bMsg 
          msgTypeB = maybeToEither "No message type param" $ Map.lookup (Bstr "y") bMsg 
          respD = maybeToEither "No Response in dictionary" $ Map.lookup (Bstr "r") bMsg
          -- These are values used to construct the response
          transactionId = unwrapString <$> transactionIdB
          msgType = unwrapString <$> msgTypeB
          genMsg msgType = 
            case msgType of
              "r" -> Response respD -- id Response $ transactionId 
              "e" -> undefined
              "q" -> undefined
              _   -> (Left "Unknown Message type") :: Either ErrorString MessageType
          
  fromBencoding _ = Left "Only Bdict's can be decoded to a DHTMessage"
