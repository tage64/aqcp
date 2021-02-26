-- Module comment TODO
module Options (Options, getOptions) where

import           System.Console.ArgParser
import           System.Environment (getArgs)
import           System.Exit
import           Control.Applicative
import           Crypto (Code)


{- Options that must be set to initialize the program.
 - It can either be:
 -  Server:  -- Indicating we are acting as server
 -      String  -- The local ip address.
 -      String  -- The port or service name.
 -      Code    -- A code to verify with the client.
 - Or:
 -  Client:  -- Indicating we are acting as client.
 -      String  -- The ip public ip address of the server.
 -      String  -- The port or service name of the server.
 -      Code    -- A code to verify with the server.
 - or:
 -  CentralServer:  -- Act as a central server accepting connections from many clients and matching clients with equal codes to each other.
 -      String  -- The local ip address.
 -      String  -- The port or service name.
 -}
data Options = Server String String Code
             | Client String String Code
             | CentralServer String String
             | Help String -- FOR TESTING
             deriving (Show)

{- makeParser
   TODO: Help subcommand
         (Program epilogue)
-}
makeParser :: IO (CmdLnInterface Options)
makeParser = do
   subcommands <- mkSubParser
      [ ("server", mkDefaultApp ((\s1 s2 s3 -> Server s1 s2 $ read $ s3) 
      `parsedBy` reqPos "address" `Descr` "local IP address" 
      `andBy` reqPos "port" `Descr` "server port or service name" 
      `andBy` reqPos "code" `Descr` "code to verify with client") "server")
      , ("client", mkDefaultApp ((\s1 s2 s3 -> Client s1 s2 $ read $ s3) 
      `parsedBy` reqPos "address" `Descr` "public IP address of the server " 
      `andBy` reqPos "port" `Descr` "server port or service name" 
      `andBy` reqPos "code" `Descr` "code to verify with server") "client") 
      , ("centralserver", mkDefaultApp 
      (CentralServer 
      `parsedBy` reqPos "address" 
      `andBy` reqPos "port") "centralserver")]
   return $ setAppDescr subcommands "Advanced Quantified Communication Program"

{- getOptions
 - add doc
 -}
getOptions :: IO Options
getOptions = do
   args <- getArgs
   parser <- makeParser
   case parseArgs args parser of 
      Left error -> (do
         putStrLn error
         exitSuccess)
      Right options -> return options
