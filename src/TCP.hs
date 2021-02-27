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
import           Data.ByteString
import           Data.IORef
import           Data.IP
import           Debug.Trace
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
          (\conn@(_, sockAddr) -> do
            let -- A new thread for this connection.
                -- Here we'll run the provided computation and catch and print any exception.
                createThread = async
                  (       computation conn
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
-}
sendBytes :: Socket -> ByteString -> IO ()
sendBytes socket someByteString = send socket someByteString

{-receiveBytes socket bytestring
  Receive bytestring over a TCP connection where socket must be bounded to a server or client.
  RETURNS: Nothing or Just bytestring
-}
receiveBytes :: Socket -> IO (Maybe ByteString)
receiveBytes socket = recv socket 4096
