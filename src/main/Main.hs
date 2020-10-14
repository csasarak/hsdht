-- Main.hs
-- Author: Christopher Sasarak
module Main where 

import Control.Monad.Trans.State
import Data.ByteString.Char8 as BS
import Data.Maybe
import HsDHT.DHTContext
import HsDHT.DHTMessage
import HsDHT.DHTSocket
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Environment
import System.IO
import System.Random

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
