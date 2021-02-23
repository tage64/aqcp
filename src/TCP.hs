module TCP
  ( Socket
  , withClient
  , withServer
  , sendBytes
  , receiveBytes
  )
where

import           Data.IP
import           Data.ByteString
import           Network.Simple.TCP

{- withClient hostname servicename
   Initializes a client end of a TCP connection, where hostname is the server domain name
   or IP address and servicename is the server port name or port number.
   PRE: In order to connect two systems on different networks,
        servicename must be a forwarded port which can be configured via the router.
   EXAMPLES:
-}
withClient :: HostName -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
withClient = connect

{-withServer hostpreference servicename
  Initializes a server end of a TCP connection, where hostpreference is the host to bind and
  servicename is the port name or the port number to bind.
  PRE: In order to connect two systems connected on different networks,
       servicename must be a forwarded port which can be configured via the router.
  EXAMPLES: 
-}
withServer
  :: HostPreference -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
withServer hostPreference serviceName computation =
  listen hostPreference serviceName (\(socket, _) -> accept socket computation)

{-sendBytes socket bytestring
  Sends bytestrings through a bounded socket.
  PRE: socket must be bound and connected
  EXAMPLES:
-}
sendBytes :: Socket -> ByteString -> IO ()
sendBytes socket someByteString = send socket someByteString

{-receiveBytes socket bytestring
  Receive bytestrings through a bounded socket.
  PRE: socket must be bound and connected
  EXAMPLES:
-}
receiveBytes :: Socket -> IO (Maybe ByteString)
receiveBytes socket = recv socket 4096
