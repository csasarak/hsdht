-- Main.hs
-- Author: Christopher Sasarak
module Main where 

import Control.Monad.Trans.State
import Data.ByteString.Char8 as BS
import HsDHT.DHTContext
import HsDHT.DHTMessage
import HsDHT.DHTSocket
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.IO
import System.Random

main :: IO ()
main = do bsData <- getSampleResponse
          case bsData of
            Left e -> Prelude.putStrLn e
            Right m -> Prelude.print m

getSampleResponse :: IO (Either String DHTMessage) 
getSampleResponse =
  do context <- initializeDHTContext getStdGen
     sock <- newUDPSocket
     let bootstrapAddr = SockAddrInet 6881 $ tupleToHostAddress (67, 215, 246, 10) -- "router.bittorrent.com"
     let msgEmitter = sendDHTMessage sock bootstrapAddr
     let (pingMsg, context') = runState newPing context
     msgEmitter pingMsg
     (bsData, sAddr) <- recvFrom sock 4096
     close sock
     return . decodeDHTMessage . BS.fromStrict $ bsData


createOrReadConfig :: IO (Maybe Handle)
createOrReadConfig = error "NI" -- do path <- configPath

configPath :: IO FilePath
configPath = (++ "/.hsdht") <$> getEnv "HOME"
