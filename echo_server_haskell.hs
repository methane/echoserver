module Main where
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO
import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)

main = withSocketsDo $ do
         let port = fromIntegral 5000
         soc <- socket AF_INET Stream 0
         addr <- inet_addr "0.0.0.0"
         let sockaddr = SockAddrInet port addr
         bindSocket soc sockaddr
         listen soc 1024
         putStrLn $ "start server, listening on: " ++ show port
         acceptLoop soc `finally` sClose soc

acceptLoop soc = do
  (nsoc, addr) <- accept soc
  forkIO $ echoLoop nsoc
  acceptLoop soc

echoLoop soc = do
  sequence_ (repeat (do { -- ioアクションの無限リスト
                          (buff,_) <- recvFrom soc 4096;
                          send soc buff
                     }))
  `catch` (\(SomeException e) -> return ())
  `finally` sClose soc
