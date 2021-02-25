module TCP
  ( Socket
  , withClient
  , withServer
  , sendBytes
  , receiveBytes
  )
where

import           Data.ByteString
import           Data.IP
import           Network.Simple.TCP

{- Since these function requires two systems in order to work it's hard to visually
   demonstrate any command line based examples. Please see TCPTest.hs to see examples.-}


{- withClient hostname servicename
   Initializes a client end of a TCP connection, where hostname is the server domain name
   or IP address and servicename is the server port name or port number. In order to connect
   two systems on different networks, servicename must be a forwarded port which can be configured
   through the router in WAN services.
   SIDE EFFECTS: hostname must be a valid address and servicename a valid port or it will
                 raise an exception.
-}
withClient :: HostName -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
withClient = connect

{- withServer hostpreference servicename
   Initializes a server end of a TCP connection, where hostpreference is the host to bind and
   servicename is the port name or the port number to bind. In order to connect
   two systems on different networks, servicename must be a forwarded port which can be configured
   through the router in WAN services.
   SIDE EFFECTS: hostname must be a valid address and servicename a valid port or it will
                 raise an exception.
-}
withServer
  :: String -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
withServer hostPreference serviceName computation =
  listen (Host hostPreference) serviceName (\(socket, _) -> accept socket computation)

{-sendBytes socket bytestring
  Send bytestring over a TCP connection where socket must be bounded to a server or client.
-}
sendBytes :: Socket -> ByteString -> IO ()
sendBytes socket someByteString = send socket someByteString

{-receiveBytes socket bytestring
  Receive bytestring over a TCP connection where socket must be bounded to a server or client.
  RETURNS: Nothing or Just bytestring
-}
receiveBytes :: Socket -> IO (Maybe ByteString)
receiveBytes socket = recv socket 4096
