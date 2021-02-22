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

withClient :: HostName -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
withClient = connect

withServer
  :: HostPreference -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
withServer hostPreference serviceName computation =
  listen hostPreference serviceName (\(socket, _) -> accept socket computation)

sendBytes :: Socket -> ByteString -> IO ()
sendBytes socket someByteString = send socket someByteString

receiveBytes :: Socket -> IO (Maybe ByteString)
receiveBytes socket = recv socket 4096
