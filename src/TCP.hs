module TCP
  ( Socket
  , SockAddr
  , withClient
  , withServer
  , sendBytes
  , receiveBytes
  )
where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Binary                   as Binary
import           Data.Binary.Get               as BGet
import           Data.ByteString
import           Data.IORef
import           Data.IP
import           Debug.Trace
import           Network.Simple.TCP      hiding ( Socket )
import qualified Network.Simple.TCP            as SimpleTCP
import qualified Network.Socket.ByteString.Lazy
                                               as SocketLazy

{- Since these function requires two systems in order to work it's hard to visually
   demonstrate any command line based examples. Please see TCPTest.hs to see examples.-}

{- A socket represents a connection over tcp.
 - Represented by Socket SimpleTCP.Socket (IORef ByteString), where the ByteString represents received but unread bytes.
 - The byte string with unused bytes is in an IORef to allow for mutable updates without having to generate a new socket.
 -}
data Socket = Socket SimpleTCP.Socket (IORef ByteString)

{- newSocket socket
 - Create a new Socket from a SimpleTCP.Socket.
 - The byte string with unused bytes will be set to empty.
 - RETURNS: A new socket.
 -}
newSocket :: SimpleTCP.Socket -> IO Socket
newSocket socket = do
  unusedBytes <- newIORef empty
  return $ Socket socket unusedBytes

{- withClient hostname servicename
   Initializes a client end of a TCP connection, where hostname is the server domain name
   or IP address and servicename is the server port name or port number. In order to connect
   two systems on different networks, servicename must be a forwarded port which can be configured
   through the router in WAN services.
   SIDE EFFECTS: hostname must be a valid address and servicename a valid port or it will
                 raise an exception.
-}
withClient :: HostName -> ServiceName -> ((Socket, SockAddr) -> IO a) -> IO a
withClient hostName serviceName computation = connect
  hostName
  serviceName
  (\(sock, sockAddr) -> do
    socket <- newSocket sock
    computation (socket, sockAddr)
  )

{- withServer hostPreference servicename terminate computation
   Initializes a server end of a TCP connection, where hostPreference is the host to bind and
   servicename is the port name or the port number to bind. In order to connect
   two systems on different networks, servicename must be a forwarded port which can be configured
   through the router in WAN services.

   terminate is an IO action which when done signals to the server to terminate.
   So if terminate is set to threadDelay 5000000 (us), the server will run for 5 seconds.
   When terminate completes, all connection computations will be canceled with Async.cancel.

   computation is a function that will be run every time a new client connects to the server in a separate thread.
   Any exception thrown by the computation will be cought and printed to stdout or stderr.

   RETURNS: IO ()
   SIDE EFFECTS: - hostname must be a valid address and servicename a valid port or it will
                   raise an exception.
                 - All exceptions thrown from computation for any connection will be printed to stdout or stderr.
                 - The returned IO () will complete first after the server is terminated.
-}
withServer
  :: String -> ServiceName -> IO () -> ((Socket, SockAddr) -> IO ()) -> IO ()
withServer hostPreference serviceName terminate computation = listen
  (Host hostPreference)
  serviceName
  (\(socket, _) -> do
    -- A mutable variable which holds a list of all connection threads.
    connectionThreads <- newIORef []
    let -- A new thread where we listen for incoming connections.
        createListenThread = async $ forever $ accept
          socket
          (\(sock, sockAddr) -> do
            socket <- newSocket sock
            let -- A new thread for this connection.
                -- Here we'll run the provided computation and catch and print any exception.
                createThread = async
                  (       computation (socket, sockAddr)
                  `catch` (\e ->
                            traceIO
                              $  "Error at connection "
                              ++ show sockAddr
                              ++ ": "
                              ++ show (e :: SomeException)
                          )
                  )
                -- Update the list of connection threads.
                updateThreadList newThread = atomicModifyIORef'
                  connectionThreads
                  (\threads -> (newThread : threads, ()))
            -- We'll use a bracket to make sure the created thread is canceled if an exception is thrown somewhere.
            -- cancel will be called if an exception is raised after the thread is created but during the thread list is updated.
            bracketOnError createThread cancel updateThreadList
          )
        -- Wait for the provided terminate computation to complete or the listen thread to throw an exception.
        waitForTerminate listenThread = do
          -- Convert terminate to an async object.
          terminateThread <- async terminate
          -- Wait until terminate completes or listenThread races an exception.
          waitEither terminateThread listenThread
        -- Cancel listen thread and all connection threads.
        cancelThreads listenThread = do
          cancel listenThread
          -- Cancel all connection threads.
          threads <- readIORef connectionThreads
          forM_ threads cancel
    -- Here we use a bracket to make sure all threads are canceled even if an exception is thrown at some point.
    bracket createListenThread cancelThreads waitForTerminate
    return ()
  )

{-sendBytes socket bytestring
  Send bytestring over a TCP connection where socket must be bounded to a server or client.
  The byte string will be tagged with its length so that the recipient will get this exact byte string.
  RETURNS: An IO computation over ().
-}
sendBytes :: Socket -> ByteString -> IO ()
sendBytes (Socket sock _) someByteString =
  let encoded = Binary.encode someByteString in SocketLazy.sendAll sock encoded

{-receiveBytes socket bytestring
  Receive bytestring over a TCP connection where socket must be bounded to a server or client.
  RETURNS: Nothing or Just bytestring
-}
receiveBytes :: Socket -> IO (Maybe ByteString)
receiveBytes (Socket sock unusedBytesRef) = do
  -- Get the unused bytes from the previous call to receiveBytes.
  -- Or to put it another way, the received but unconsumed bytes.
  -- If the last call for example forced us to fetch 512 bytes from the socket.
  -- But we only consumed 12 bytes of those 512 then unusedBytes will contain those 500 remaining bytes.
  unusedBytes <- readIORef unusedBytesRef
  -- Create a ByteString decoder from the Data.Binary module.
  let newDecoder = BGet.runGetIncremental Binary.get
  -- Run the decode loop with the unused bytes put into the decoder.
  decodeLoop $ BGet.pushChunk newDecoder unusedBytes
 where
  {- decodeLoop decoder
   - Check the state of the decoder and take a propper action based on that.
   - If the decoder has failed, then throw an error.
   - If the decoder is done, update unusedBytes with the remaining unused bytes and return Just result.
   - If the decoder needs more bytes, fetch more bytes from the socket, push them into the decoder and run decodeLoop with the new decoder again.
   - RETURNS: Just byteString if everything worked well or Nothing if the other end disconnected.
   - SIDE_EFFECTS: Will update unusedBytesRef.
   -}
  decodeLoop :: BGet.Decoder ByteString -> IO (Maybe ByteString)
  decodeLoop decoder = case decoder of
    Fail _ _ err -> error $ "Failed to decode byte string in tcp:" ++ err
    Done unusedBytes _ result -> do
      writeIORef unusedBytesRef unusedBytes
      return $ Just result
    Partial _ -> runMaybeT $ do
      -- Not enough bytes were availlable so we'll fetch more from the socket.
      received <- MaybeT $ recv sock 512
      -- Set unused bytes to empty since we've consumed all of them.
      liftIO $ writeIORef unusedBytesRef empty
      -- Push the received bytes into the decoder.
      let newDecoder = BGet.pushChunk decoder received
      -- Run decodeLoop with the new decoder.
      MaybeT $ decodeLoop newDecoder
