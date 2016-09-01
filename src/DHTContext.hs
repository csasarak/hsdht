{-# LANGUAGE GADTs, ExistentialQuantification, RecordWildCards #-}
-- This file has types/functions for a context in which
-- DHT stuff can be done e.g. local node, routing table, RNG etc.

module DHTContext ( DHTContext(..)
                  , newDHTContext
                  ) where

import RoutingTable
import Node
import System.Random
import Control.Monad.State

-- Might have to put an rng somewhere in here...
data DHTContext = forall g. RandomGen g => DHTContext { 
      getLocalNode :: Node -- this node in the DHT
    , getRoutingTable :: RoutingTable 
    , getRng :: g
    }

-- Will generate a new DHTContext with a fresh node name
newDHTContext :: RandomGen g => g -> DHTContext
newDHTContext rng = DHTContext (genNode rng) newRoutingTable rng'
                    where (_, rng') = random rng

-- | This is a helper for getting a random value out of a DHTContext
-- getRng is a polymorphic variable, this function prevents it from escaping
getRandomFromDHTContext :: (Integral a) => DHTContext -> (DHTContext, a)
getRandomFromDHTContext DHTContext{..} = (newc, n)
                                         where (n, rng') = random getRng
                                               newc = DHTContext getLocalNode getRoutingTable rng' 

--getTransactionId:: DHTContext -> Int
--getTransactionId DHTContext{getRng=rng}  = let (n, rng') = next rng in n
getTransactionId :: State DHTContext Int
getTransactionId = do (newContext, n) <- gets getRandomFromDHTContext
                      put newContext
                      return n
