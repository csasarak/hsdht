
module Node where 

import qualified Data.ByteString as BS
-- This is mostly for debugging
import Data.ByteString.Base16 (encode)
import Control.Applicative
import System.Random
import Data.Word

data Node = Node { nodeId :: BS.ByteString }

-- Generates a node with a new ByteString given a random number generator
genNode :: RandomGen g => g -> Node
genNode gen = Node id
              where id = BS.pack . take 20 . randoms $ gen 


