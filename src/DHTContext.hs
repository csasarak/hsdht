{-# LANGUAGE GADTs, ExistentialQuantification, RecordWildCards #-}
-- This file has types/functions for a context in which
-- DHT stuff can be done e.g. local node, routing table, RNG etc.

module DHTContext ( DHTContext(..)
                  , newDHTContext
                  , initializeContext
                  , newQuery
                  ) where

import RoutingTable
import Node
import DHTMessage
import Numeric
import Data.Bits
import System.Random
import Control.Monad.State

-- Might have to put an rng somewhere in here...
data DHTContext = forall g. RandomGen g => DHTContext { 
      localNode :: Node -- this node in the DHT
    , routingTable :: RoutingTable 
    , nextTid :: TransactionIdI
    , contextRng :: g
    }

-- | Will generate a new DHTContext with a fresh node id
newDHTContext :: RandomGen g => g -> DHTContext
newDHTContext rng = DHTContext (genNode rng1) newRoutingTable 00 rng2
                    where (rng1, rng2) = split rng -- split because I'm lazy


-- | This is a helper for getting a random value out of a DHTContext
-- contextRng is a polymorphic variable, this function prevents it from escaping
getRandomFromDHTContext :: (Integral a, Random a) => DHTContext -> (a, DHTContext)
getRandomFromDHTContext DHTContext{..} = (n, newc)
                                         where (n, rng') = random contextRng
                                               newc = DHTContext localNode routingTable nextTid rng' 

-- Computations with a DHTContext maintaining State

initializeContext :: (Monad m, RandomGen g) => m g -> m DHTContext
initializeContext = fmap newDHTContext

getTransactionId :: State DHTContext String
getTransactionId = do dhtCon <- get 
                      let tId = nextTid dhtCon
                      let newId = (tId + 1) `mod` maxTransactionId
                      put $ dhtCon { nextTid = newId }
                      return $ showHex newId "" 

-- | Given a QueryMethod, generate a new Query Message 
newQuery :: QueryMethod -> State DHTContext DHTMessage
newQuery qm = do tId <- getTransactionId
                 nId <- gets (nodeId . localNode)
                 return DHTMessage {transactionId=tId, messageType = Query qm nId }
