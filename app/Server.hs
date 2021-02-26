module Server
  ( runServer
  )
where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Crypto
import           StreamAudio
import           TCP

{- runServer terminate ipAddr serviceName code
 - Run the program as a server talking to a client.
 - Arguments:
 -    terminate: An IO computation which when completed terminates the server.
 -               It can for example be set to a timer or wait for the user to type "exit".
 -    ipAddr: The local ip address of the server.
 -    sirviceName: The port number or service name to connect to.
 -    code: A code to verify with the client.
 -          Will only connect to a client with the same code.
 - RETURNS: () in an IO computation.
 - SIDE_EFFECTS: TODO
 -}
runServer :: IO () -> String -> String -> Code -> IO ()
runServer terminate ipAddr sirviceName code = TCP.withServer ipAddr
                                                             sirviceName
                                                             terminate
                                                             connection
 where
  {- connection (socket, _)
   - Handshake with the client on a socket and start send and receive audio.
   - Will run forever. The only way to quit is via Async.cancel on this thread which will be done when the server terminates.
   - RETURNS: IO (), but will in practise never return.
   - SIDE_EFFECTS: Throw an exception if any irrecoverable error occurres.
   -                  It can for example be that the client disconnects.
   -}
  connection :: (Socket, SockAddr) -> IO ()
  connection (socket, _) = do
    -- We start by handshaking with the client.
    --
    -- Initialize the crypto machinery.
    crypto                       <- Crypto.initCrypto
    -- Generate a secret key and a handshake.
    (secretKey, serverHandshake) <- newServerHandshake crypto code
    -- Convert the handshake to a byte string so that we can send it over tcp.
    let serverHandshakeAsBytes =
          Crypto.serverHandshakeToByteString serverHandshake
    -- Send the handshake over tcp to the client.
    TCP.sendBytes socket serverHandshakeAsBytes
    -- Wait for a handshake from the client.
    maybeReceived <- receiveBytes socket
    let clientHandshake = case maybeReceived of
          Just bytes -> Crypto.clientHandshakeFromByteString bytes
          nothing    -> error "Client disconnected while handshaking."
    -- Handshake with the client.
    let (encrypter, decrypter) =
          case createServer secretKey serverHandshake clientHandshake of
            Just x  -> x
            Nothing -> error $ "Refuced a client with a bad code: " ++ show
              (clientsCode clientHandshake)

    -- Start the audio streaming.
    -- It'll terminate when terminate completes, (as runServer will do).
    streamAudio terminate socket encrypter decrypter
