-- Main.hs
-- Author: Christopher Sasarak
module Main where 

import Data.Maybe
import System.IO
import System.Environment
import System.Random
import Control.Monad.State
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.ByteString.Char8 as BS
import DHTSocket
import DHTContext
import DHTMessage

main :: IO ()
main = do bsData <- getSampleResponse
          case bsData of
            Left e -> Prelude.putStrLn e
            Right m -> Prelude.putStrLn . show $ m

getSampleResponse :: IO (Either String DHTMessage) 
getSampleResponse =
  do context <- initializeDHTContext getStdGen
     socket <- newUDPSocket
     bootstrapAddr <- SockAddrInet 6881 <$> inet_addr "67.215.246.10" -- "router.bittorrent.com"
     let msgEmitter = sendDHTMessage socket bootstrapAddr
     let (pingMsg, context') = runState newPing context
     msgEmitter pingMsg
     (bsData, sAddr) <- recvFrom socket 4096
     close socket
     return $ decodeDHTMessage bsData


createOrReadConfig :: IO (Maybe Handle)
createOrReadConfig = error "NI" -- do path <- configPath

configPath :: IO FilePath
configPath = (++ "/.hsdht") <$> getEnv "HOME"
