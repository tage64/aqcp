import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Data.ByteString.Char8         as Char8
import           Network.Simple.TCP
import           TCP
import           Test.HUnit

port = "5050"

localHost = "127.0.0.1"

serverMessage = "Hi, server here."
clientMessage = "Hi, client here."

server :: String -> String -> String -> IO (Maybe Char8.ByteString)
server address port message = withServer
  (Host address)
  port
  (\(socket, socketAddress) -> do
    sendBytes socket $ Char8.pack message
    receiveBytes socket
  )

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
  (serverReceived, clientReceived) <-
    concurrently (server "0.0.0.0" port serverMessage)
    $  (threadDelay 250000)  -- To make sure server is loaded before client.
    *> (client localHost port clientMessage)
  putStrLn $ "Client received: " ++ show clientReceived
  putStrLn $ "Server received: " ++ show serverReceived
  assertEqual "Bad server message" clientReceived $ Just $ Char8.pack
    serverMessage
  assertEqual "Bad client message" serverReceived $ Just $ Char8.pack
    clientMessage
