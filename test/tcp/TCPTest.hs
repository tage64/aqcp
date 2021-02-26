import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently )
import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8         as Char8
import           TCP
import           Test.HUnit

port = "5050"

localHost = "127.0.0.1"

serverMessage = "Hi, server here."
clientMessage = "Hi, client here."

{- server address port message
   Initializes a TCP server end point where address is the domain name or hostname IP address,
   port is the port name or port number. In order to connect two systems on different networks,
   servicename must be a forwarded port which can be configured through the router in WAN services.
   SIDE EFFECTS: address must be a valid address and port a valid port or it will raise an exception.
   EXAMPLES: server system: server "192.168.1.118" "5050" "Hello, Client!" == Just "Hello, Server!"
             client system: client "192.169.1.118" "5050" "Hello, Server!" == Just "Hello, Client!"
-}
server :: String -> String -> String -> IO (MVar (Maybe Char8.ByteString))
server address port message = do
  result <- newEmptyMVar
  let terminate = readMVar result >> return ()
  withServer
    address
    port
    terminate
    (\(socket, socketAddress) -> do
      sendBytes socket $ Char8.pack message
      received <- receiveBytes socket
      putMVar result received
    )
  return result

{- client address port message
   Initializes a TCP client end point where address is the server domain name or hostname IP address,
   port is the port name or port number. In order to connect two systems on different networks,
   servicename must be a forwarded port which can be configured through the router in WAN services.
   SIDE EFFECTS: address must be a valid address and port a valid port or it will raise an exception.
   EXAMPLES: server system: server "192.168.1.118" "5050" "Hello, Client!" == Just "Hello, Server!"
             client system: client "192.169.1.118" "5050" "Hello, Server!" == Just "Hello, Client!"
-}
client :: String -> String -> String -> IO (Maybe Char8.ByteString)
client address port message = withClient
  address
  port
  (\(socket, socketAddress) -> do
    sendBytes socket $ Char8.pack message
    receiveBytes socket
  )

main :: IO ()
main = do
  (serverResult, clientReceived) <-
    concurrently (server "0.0.0.0" port serverMessage)
    $  (threadDelay 250000)  -- To make sure server is loaded before client.
    *> (client localHost port clientMessage)
  serverReceived <- takeMVar serverResult
  putStrLn $ "Client received: " ++ show clientReceived
  putStrLn $ "Server received: " ++ show serverReceived
  assertEqual "Bad server message" clientReceived $ Just $ Char8.pack
    serverMessage
  assertEqual "Bad client message" serverReceived $ Just $ Char8.pack
    clientMessage
