module Socket
  ( Socket
  , withSocket
  , sendBytes
  , receiveBytes
  , closeSocket
  )
where

import           Data.IP
import           Data.ByteString
import qualified Network.Socket.ByteString     as BS
import           Network.Socket

convertIP :: String -> HostAddress
convertIP publicIP = toHostAddress $ (read publicIP :: IPv4)

withSocket :: PortNumber -> HostAddress -> (Socket -> IO ()) -> IO ()
withSocket portNumber hostAddress sockComputation = withSocketsDo $ do
  socket <- socket AF_INET Stream 0
  setSocketOption socket ReuseAddr 1
  bind socket (SockAddrInet portNumber hostAddress)
  sockComputation socket

sendBytes :: Socket -> ByteString -> IO ()
sendBytes socket someByteString = BS.sendAll socket someByteString

receiveBytes :: Socket -> IO ByteString
receiveBytes socket = BS.recv socket 2

closeSocket :: Socket -> IO ()
closeSocket = close
