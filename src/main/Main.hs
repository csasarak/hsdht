-- Main.hs
-- Author: Christopher Sasarak
module Main where 

import Data.Maybe
import System.IO
import System.Environment
import System.Random
import Control.Monad
import DHTContext
import DHTMessage

main :: IO ()
main = do initializeContext getStdGen
          return ()
          
createOrReadConfig :: IO (Maybe Handle)
createOrReadConfig = error "NI" -- do path <- configPath
                        

configPath :: IO FilePath
configPath = (++ "/.hsdht") <$> getEnv "HOME"
