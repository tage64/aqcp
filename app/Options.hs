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
             deriving (Show)

{- makeParser
   Creates a parser composed of list of subparsers for command line interaction. The three subcommands are server, client and centralserver where each 
   subcommand require mandatory positional arguments. Each subparser is associated with a command the the user must type to activate. 
   Each command tells the program what role the user want to act on. The library provides Help/Usage info that automatically built from the parser specification
   which can be called with "-h".
   RETURNS: CmdLnInterface Options in an IO computation
   EXAMPLES: After building an executable, these are examples to run on the command line to interact with aqcp 
             $ aqcp server "192.1.1.25" "5050" 102754333223424
             $ aqcp server "example.org" "80" 12314123412323
             $ aqcp client "151.56.1.256" "5050" 102754333223424
             $ aqcp centralserver "192.12.123.1" "6077"
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
   Reads the argument passed to the command line and parses it.
   RETURNS: Right Options upon success otherwise Left exception
   EXAMPLES: Preset arguments in ghci with :set args server "somePublicIP" "somePort" 123
             getOptions == Server "somePublicIp" "somePort" 123
 -}
getOptions :: IO Options
getOptions = do
   args <- getArgs
   parser <- makeParser
   case parseArgs args parser of 
      Left error -> (do
         putStrLn error
         exitFailure)
      Right options -> return options
