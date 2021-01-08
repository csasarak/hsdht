{-# LANGUAGE GADTs, ExistentialQuantification, RecordWildCards #-}
-- This file has types/functions for a context in which
-- DHT stuff can be done e.g. local node, routing table, RNG etc.

module HsDHT.DHTContext ( DHTContext(..)
                        , newDHTContext
                        , initializeDHTContext
                        , newPing
                        ) where

import Control.Monad.Trans.State
import Data.ByteString.Char8 as BS
import HsDHT.DHTMessage
import HsDHT.Node
import HsDHT.RoutingTable
import Numeric
import System.Random

-- Might have to put an rng somewhere in here...
data DHTContext =
  forall g. RandomGen g => DHTContext { 
      -- | Node data for this 'Node'
      localNode :: Node -- this node in the DHT
       -- | This Node's 'RoutingTable'
    , routingTable :: RoutingTable 
      -- | The 'TransactionIdI' which will be assigned for the next transaction.
    , nextTid :: TransactionIdI
      -- | A 'RandomGen' which can be used for generating data
    , contextRng :: g
    }

-- | Will generate a new DHTContext with a fresh node id
newDHTContext :: RandomGen g => g -> DHTContext
newDHTContext rng = DHTContext node newRoutingTable 00 rng'
                    where (node, rng') = genNode rng

-- | This is a helper for getting a random value out of a DHTContext
getRandomFromDHTContext :: (Integral a, Random a) => State DHTContext a -- Could I replace the rng with this action?
getRandomFromDHTContext = do dhtC <- get
                             case dhtC of
                               DHTContext{..} -> let (v, rng') = random contextRng in
                                                   do put DHTContext{localNode = localNode
                                                                    , routingTable = routingTable
                                                                    , nextTid = nextTid
                                                                    , contextRng = rng'}
                                                      return v
                             
-- | Given an action which produces and instance of RandomGen, initialize a
-- DHTContext from the RandomGen
initializeDHTContext :: (Functor f, RandomGen g) => f g -> f DHTContext
initializeDHTContext = fmap newDHTContext

-- Computations with a DHTContext maintaining State

-- | Given a DHTContext, get the next TransactionId and return as a String, updating
-- the transaction counter as well. 
getNextTransactionId :: State DHTContext BS.ByteString
getNextTransactionId = do dhtCon <- get 
                          let tId = nextTid dhtCon
                          let newId = (tId + 1) `mod` maxTransactionId
                          put $ dhtCon { nextTid = newId }
                          return $ BS.pack $ showHex tId "" 

-- CMS: Should probably rename
-- | Given a 'QueryMethod', generate a 'State' action producing
-- 'DHTMessage's given a particular 'DHTContext' when run
newQuery :: QueryMethod -> State DHTContext DHTMessage
newQuery qm = do tId <- getNextTransactionId
                 nId <- gets (nodeId . localNode)
                 return DHTMessage {transactionId=tId, messageType = Query qm nId }

-- | Generate a Ping query message
newPing :: State DHTContext DHTMessage
newPing = newQuery Ping
