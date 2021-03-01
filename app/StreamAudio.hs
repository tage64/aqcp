-- This module contains a function which reads and writes audio and sends it over a socket.
-- It is common for both a server and client.
module StreamAudio
  ( streamAudio
  )
where

import           AudioCompress
import           AudioIO
import           Control.Concurrent.Async
import           Control.Exception              ( bracketOnError )
import           Control.Monad
import           Crypto
import           TCP

{- streamAudio terminate socket encrypter decrypter inputAudioDevice outputAudioDevice
 - Initialize AudioIO, read/write audio simultainiously followed by compression/decompression, encryption/decryption and sending/receiving over a socket.
 - Basically, handle all audio streaming.
 - terminate is an IO computation witch will terminate the audio streaming when completed.
 - So if it is set to a timer, this function will complete after the specified time.
 - RETURNS: IO ()
 - SIDE_EFFECTS: Will throw an exception if an unrecoverable error occurres at any point in the initialization or streaming.
 -}
streamAudio
  :: IO ()
  -> Socket
  -> Encrypter
  -> Decrypter
  -> DeviceInfo
  -> DeviceInfo
  -> IO ()
streamAudio terminate socket encrypter decrypter inputDevice outputDevice = do
  result <- withAudioIO
    inputDevice
    outputDevice
    (\(inputStream, outputStream) -> do
      let -- Wait until terminate, readerThread or writerThread completes.
          -- Since reader and writer should run forever, it'll in practise wait until terminates completes or reader or writer throws an exception.
          waitForTerminate readerThread writerThread = do
            race terminate $ waitEither readerThread writerThread
            return ()
          readerThread = async $ reader socket encrypter inputStream
          writerThread = async $ writer socket decrypter outputStream
      -- We'll use bracketOnError from Control.Exception to make sure reader and writer are canceled in case of an exception.
      bracketOnError
        readerThread
        cancel
        (\readerThread -> bracketOnError
          writerThread
          cancel
          (\writerThread -> waitForTerminate readerThread writerThread)
        )
    )
  case result of
    Left err ->
      error $ "Error while initializing or terminating portaudio: " ++ show err
    Right () -> return ()
 where
  {- reader socket encrypter inputStream
   - Read audio from inputStream,
   - compress it,
   - encrypt it with encrypter and generate a new encrypter,
   - send it over the socket,
   - and call the function recursively with the new encrypter.
   - RETURNS: IO (), but will actually never return.
   - SIDE_EFFECTS: Throw an exception if an irrecoverable error would occur.
   -}
  reader :: Socket -> Encrypter -> InputStream -> IO ()
  reader socket encrypter inputStream = do
    (audio, _) <- readAudio inputStream
    let compressed                = compress audio
        (newEncrypter, encrypted) = encrypt encrypter compressed
    sendBytes socket encrypted
    -- Since we generated a new encrypter, we will call this function recursively with the new encrypter.
    reader socket newEncrypter inputStream

  {- writer socket decrypter outputStream
   - Receive bytes from the socket,
   - decrypt it with decrypter and generate a new decrypter,
   - decompress it,
   - write it to outputStream,
   - and call this function recursively with the new decrypter.
   - RETURNS: IO (), but will actually never return.
   - SIDE_EFFECTS: Throw an exception if an irrecoverable error would occur.
   -}
  writer :: Socket -> Decrypter -> OutputStream -> IO ()
  writer socket decrypter outputStream = do
    maybeReceived <- receiveBytes socket
    let received = case maybeReceived of
          Just x  -> x
          Nothing -> error "The other end has disconnected."
        (newDecrypter, decrypted) = case decrypt decrypter received of
          Just x  -> x
          Nothing -> error "Failed to decrypt."
        decompressed = decompress decrypted
    writeAudio outputStream decompressed
    writer socket newDecrypter outputStream
