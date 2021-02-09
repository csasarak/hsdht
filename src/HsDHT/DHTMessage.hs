{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HsDHT.DHTMessage where

import qualified Data.ByteString.Lazy.Char8 as BSChar
import           Data.ByteString.Lazy as BS
import qualified Data.Map.Lazy as Map
import           HsDHT.Bencode
import           HsDHT.Node
import           HsDHT.Util
import           Numeric (showHex)
import Data.Word (Word16)

-- | A String describing an error
type ErrorString = String
-- | A short (generally two-chars) hexadecimal string transaction id
type TransactionId = BS.ByteString
-- | An Integral form of the transaction id
type TransactionIdI = Word16 -- Integer form of TransactionId
-- CMS: Think about turning these hashes into ByteStrings
-- | 160-bit InfoHash
type InfoHash = Integer
type Port = Int
-- | BEP-0005 implied_port, if true use UDP packet port and not Node's port
type ImpliedPort = Bool

-- | The number of allowed outstanding transactions at any time. Higher
-- numbered transactions will wrap back around to 0
maxTransactionId :: Word16
maxTransactionId = maxBound

-- |MessageType represents a type of DHT message along with any
-- necessary parameters for that type of query.
-- There's some asymmetry here, but Responses take Bencoded data because
-- it's not really possible to predict what ever response will have, or what
-- Responses are returned by foreign nodes. 
data MessageType =
  -- | A Query message. NodeId is the querying Node's ID
  Query QueryMethod NodeId 
  -- | A message from a foreign node, contains a Bencoded dictionary
  -- of response key-value pairs. 
  | Response Bencode 
  -- | A message indicating an error
  | Error 

instance Show MessageType where
  show msgT = case msgT of
    (Query qMeth nId) -> "Query (" ++ show qMeth ++ ") " ++ show nId
    (Response bEnc)   -> "Response (" ++ show bEnc ++ ")"
    Error  -> "Error"

-- |Methods for querying the DHT, along with data specific to that
-- method. See BEP-0005 for more info.
data QueryMethod =
    Ping -- ^ Ping to check if a node is alive still
  | FindNode NodeId -- ^ NodeId is the target ID to find
  | GetPeers InfoHash -- ^ The InfoHash to search for
  | AnnouncePeer InfoHash Port Bool -- ^ Probably won't use this too much

instance Show QueryMethod where
  show qm = case qm of
    Ping -> "Ping"
    (FindNode nId) -> "FindNode " ++ show nId
    (GetPeers iHash) -> "GetPeers " ++ show iHash
    (AnnouncePeer iHash port bool) -> "AnnouncePeer " ++
      unwords [show iHash, show port, show bool]
  

-- | Return the key for a particular MessageType
messageTypeCode :: MessageType -> BS.ByteString
messageTypeCode (Query _ _)  = "q"
messageTypeCode (Response _) = "r"
messageTypeCode Error     = "e"
    
data DHTMessage =
  DHTMessage {
  -- | The id for this transaction
  transactionId :: TransactionId,
  -- | The type of message this is
  messageType :: MessageType -- there might be a better name for this
  }

instance Show DHTMessage where
  show DHTMessage{..} = "DHTMessage { transactionId: " ++ show transactionId ++
    "\nmessageType: " ++ show messageType ++ " }"
    
decodeDHTMessage :: BS.ByteString -> Either ErrorString DHTMessage
decodeDHTMessage bm = case parsedE of
                          (Left err) -> Left $ show err
                          (Right bEnc) -> fromBencoding bEnc
  where parsedE = parseBencodedDict bm 

-- | Convenience function for converting integral representations to hex ByteStrings
hexByteString :: (Integral a, Show a) => a -> BS.ByteString
hexByteString n = BSChar.pack $ showHex n ""

-- This is not yet implemented for all message types
instance Bencodable DHTMessage where
  toBencoding DHTMessage{..} =
    let msgBase = [("t", Bstr transactionId)]
        queryBase = ("y", Bstr $ messageTypeCode messageType):msgBase
        pingQuery = ("q", Bstr "ping"):queryBase
        queryArgDict nodeId = Bdict $ Map.fromList [("id", Bstr $ hexByteString nodeId)]
    in
      case messageType of
        (Query Ping nodeId) -> Bdict $ Map.fromList $ ("a", queryArgDict nodeId):pingQuery

-- This is not yet implemented for all message types
instance Bdecodable ErrorString DHTMessage where
  fromBencoding (Bdict bMsg) = DHTMessage <$> transactionId <*> (genMsg =<< msgType)
    where unwrapString (Bstr s) = s
          -- These values inspect/query the Becoded value for message pieces
          transactionIdB = maybeToEither "No transaction ID" $ Map.lookup "t" bMsg 
          msgTypeB = maybeToEither "No message type param"  $ Map.lookup "y" bMsg 
          respD = maybeToEither "No Response in dictionary" $ Map.lookup "r" bMsg
          -- These are values used to construct the response
          transactionId = unwrapString <$> transactionIdB
          msgType = unwrapString <$> msgTypeB
          genMsg typeChar = 
            case typeChar of
              "r" -> Response <$> respD -- id Response $ transactionId 
              "e" -> undefined
              "q" -> undefined
              _   -> Left "Unknown Message type" :: Either ErrorString MessageType
          
  fromBencoding _ = Left "Only Bdict's can be decoded to a DHTMessage"
