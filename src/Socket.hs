module Socket
  ( Socket
  , initSocket
  , sendBytes
  , receiveBytes
  , closeSocket
  )
where

import           Data.ByteString
import           Network.Socket


initSocket :: SocketType -> IO Socket
initSocket = undefined

sendBytes :: Socket -> ByteString -> IO ()
sendBytes = undefined

receiveBytes :: Socket -> IO ByteString
receiveBytes = undefined

closeSocket :: Socket -> IO ()
closeSocket = undefined
