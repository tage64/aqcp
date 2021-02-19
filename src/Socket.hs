module Socket
  ( Socket
  , initSocket
  , sendBytes
  , receiveBytes
  , closeSocket
  )
where

import           Data.ByteString
import qualified Network.Socket.ByteString as BS
import           Network.Socket

sockType :: SocketType
sockType = Stream

convertIP :: String -> HostAddress
convertIP publicIP = undefined

initSocket :: SocketType -> IO ()
initSocket sockType = withSocketsDo $ do
  socket <- socket AF_INET sockType 0
  setSocketOption socket ReuseAddr 1
  bind socket (SockAddrInet 5050 convertIP (someIP))

sendBytes :: Socket -> ByteString -> IO ()
sendBytes socket = BS.sendAll socket someByteString

receiveBytes :: Socket -> IO ByteString
receiveBytes socket = BS.recv socket 2

closeSocket :: Socket -> IO ()
closeSocket = close
