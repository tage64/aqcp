-- Code for running a client in an aqcp-call.
--
-- This is at a high level. The functions here ties many underlying modules and interfaces together.
module Client
  ( runClient
  )
where

import           AudioIO
import           Crypto
import           StreamAudio
import           TCP

{- runClient terminate ipAddr serviceName code inputAudioDevice outputAudioDevice
 - Run a client.
 - The client will connect to a server at ipAddr and port equal to serviceName.
 - It'll then handshake with the server using the provided code.
 - The handshake will fail if the code from the client and server mismatch.
 - But be aware that the server can see the client's code before it decides on its own code, so it might just pick the same code as the client.
 - Therefore, the code should be seen as a unique call id rather than a secure password.
 -
 - The client will terminate when the provided IO action `terminate` completes.
 - That means that if terminate is set to a timer, the client will run for the specified time.
 -
 - RETURNS: () in an IO computation.
 - SIDE_EFFECTS: Lots of side effects, some of which are:
 -                - Connect to ipAddr:serviceName.
 -                - Initialize portaudio, listen on the microphone and play to the speakers.
 -                - Throw an exception if something goes wrong or if the server disconnects.
 -}
runClient
  :: IO () -> String -> String -> Code -> DeviceInfo -> DeviceInfo -> IO ()
runClient terminate ipAddr serviceName code inputDevice outputDevice =
  TCP.withClient ipAddr serviceName connection
 where
  {- connection (socket, _)
   - Handshake with a server on a socket and start send and receive audio.
   - Will run forever. The only way to quit is via Async.cancel on this thread which will be done when the client terminates.
   - RETURNS: IO (), but will in practise never return.
   - SIDE_EFFECTS: Throw an exception if any irrecoverable error occurres.
   -                  It can for example be that the server disconnects.
   -}
  connection :: (Socket, SockAddr) -> IO ()
  connection (socket, _) = do
    -- We start by handshaking with the server.
    --
    -- Initialize the crypto machinery.
    crypto                       <- Crypto.initCrypto
    -- Generate a secret key and a handshake.
    putStrLn "Generating public and private key pair."
    (secretKey, clientHandshake) <- newClientHandshake crypto code
    -- Convert the handshake to a byte string so that we can send it over tcp.
    let clientHandshakeAsBytes =
          Crypto.clientHandshakeToByteString clientHandshake
    -- Send the handshake over tcp to the server.
    putStrLn "Sending public key to server."
    TCP.sendBytes socket clientHandshakeAsBytes
    -- Wait for a handshake from the server.
    putStrLn "Waiting for server's public key."
    maybeReceived <- receiveBytes socket
    let serverHandshake = case maybeReceived of
          Just bytes -> Crypto.serverHandshakeFromByteString bytes
          nothing    -> error "Server disconnected while handshaking."
    -- Handshake with the server.
    putStrLn "Handshakeing with the server."
    let (encrypter, decrypter) =
          case createClient secretKey serverHandshake clientHandshake of
            Just x -> x
            Nothing ->
              error $ "Rejected server due to bad code from server: " ++ show
                (serversCode serverHandshake)

    -- Start the audio streaming.
    -- It'll terminate when terminate completes, (as runClient will do).
    putStrLn "Start audio streaming."
    streamAudio terminate socket encrypter decrypter inputDevice outputDevice
