module Main where

import Control.Concurrent (forkIO)
import Control.Exception (onException)
import qualified Data.ByteString as BS (length)
import Network (withSocketsDo, listenOn, PortID(..))
import Network.Socket (Socket, sClose, accept)
import Network.Socket.ByteString (recv, send)
import Prelude hiding (catch)

main :: IO ()
main = withSocketsDo $ do
    let port = 5000
    soc <- listenOn $ PortNumber port
    putStrLn $ "start server, listening on: " ++ show port
    acceptLoop soc `onException` sClose soc

acceptLoop :: Socket -> IO ()
acceptLoop soc = do
  (nsoc, _) <- accept soc
  forkIO (echoLoop nsoc `onException` sClose nsoc)
  acceptLoop soc

echoLoop :: Socket -> IO ()
echoLoop soc = do
    bs <- recv soc 4096;
    if BS.length bs == 0
       then sClose soc
       else do
        send soc bs
        echoLoop soc
