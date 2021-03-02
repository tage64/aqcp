import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently )
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString.Char8         as Char8
import           TCP
import           Test.HUnit

port = "5050"

localHost = "127.0.0.1"

serverMessage = "Hi, server here."
clientMessages =
  ["Hi, client here.", "Hi again from client.", "Client says good buy."]

{- server address port message
   Initializes a TCP server end point where address is the domain name or hostname IP address,
   port is the port name or port number. In order to connect two systems on different networks,
   it must be a forwarded port which can be configured through the router in WAN services.
   message is a message to send to the client.
   SIDE EFFECTS: address must be a valid address and port a valid port or it will raise an exception.
   RETURNS: A list of all messages received from the client until it disconnects in a MVar in an IO action.
-}
server :: String -> String -> String -> IO (MVar [Char8.ByteString])
server address port message = do
  result <- newEmptyMVar
  let terminate = readMVar result >> return ()
  withServer
    address
    port
    terminate
    (\(socket, socketAddress) -> do
      sendBytes socket $ Char8.pack message
      let -- Receive all messages on the socket.
          receiveAll = do
            received <- receiveBytes socket
            case received of
              Just x -> do
                tail <- receiveAll
                return (x : tail)
              Nothing -> return []
      received <- receiveAll
      putMVar result received
    )
  return result

{- client address port messages
   Initializes a TCP client end point where address is the server domain name or hostname IP address,
   port is the port name or port number. In order to connect two systems on different networks,
   servicename must be a forwarded port which can be configured through the router in WAN services.
   Sends all messages over the socket and receives one message.
   SIDE EFFECTS: address must be a valid address and port a valid port or it will raise an exception.
   RETURNS: In an IO computation:
              Just message if a message was received
              Nothing otherwise
-}
client :: String -> String -> [String] -> IO (Maybe Char8.ByteString)
client address port messages = withClient
  address
  port
  (\(socket, socketAddress) -> do
    forM_ messages (\message -> sendBytes socket $ Char8.pack message)
    receiveBytes socket
  )

main :: IO ()
main = do
  (serverResult, clientReceived) <-
    concurrently (server "0.0.0.0" port serverMessage)
    $  (threadDelay 250000)  -- To make sure server is loaded before client.
    *> (client localHost port clientMessages)
  serverReceived <- takeMVar serverResult
  putStrLn $ "Client received: " ++ show clientReceived
  putStrLn $ "Server received: " ++ show serverReceived
  assertEqual "Bad server message" clientReceived $ Just $ Char8.pack
    serverMessage
  assertEqual "Bad client message" serverReceived
    $ map Char8.pack clientMessages
