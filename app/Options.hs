module Options
  ( Options(..)
  , getOptions
  )
where

import           System.Console.ArgParser
import           System.Console.ArgParser.Format
import           System.Environment             ( getArgs )
import           System.Exit
import           Control.Applicative
import           Crypto                         ( Code )


{- Options that must be set to initialize the program.
 - It can either be:
 -  Server:  -- Indicating we are acting as server
 -      String    -- The local ip address.
 -      String    -- The port or service name.
 -      Code      -- A code to verify with the client.
 -      Maybe Int -- An input audio device
 -      Maybe Int -- An output audio device
 - Or:
 -  Client:  -- Indicating we are acting as client.
 -      String    -- The ip public ip address of the server.
 -      String    -- The port or service name of the server.
 -      Code      -- A code to verify with the server.
 -      Maybe Int -- An input audio device
 -      Maybe Int -- An output audio device
 - or:
 -  ListAudioDevices: verbose -- List all audio devices on the system
 -                            -- Be more verbose if verbose is True.
 -}
data Options = Server String String Code (Maybe Int) (Maybe Int)
             | Client String String Code (Maybe Int) (Maybe Int)
             | ListAudioDevices Bool
             deriving (Show)

{- makeParser
   Creates a parser composed of list of subparsers for command line interaction. The three subcommands are server, client and centralserver where each 
   subcommand require mandatory positional arguments. Each subparser is associated with a command the the user must type to activate. 
   Each command tells the program what role the user want to act on. The library provides Help/Usage info that automatically built from the parser specification
   which can be called with "-h".
   RETURNS: CmdLnInterface Options in an IO computation
-}
makeParser :: IO (CmdLnInterface Options)
makeParser = do
  subcommands <- mkSubParser
    [ ( "server"
      , mkDefaultApp
        (          (\s1 s2 s3 s4 s5 -> Server
                     s1
                     s2
                     (read s3)
                     (if s4 == "" then Nothing else Just (read s4))
                     (if s5 == "" then Nothing else Just (read s5))
                   )
        `parsedBy` reqPos "address"
        `Descr`    "local IP address"
        `andBy`    reqPos "port"
        `Descr`    "server port or service name"
        `andBy`    reqPos "code"
        `Descr`    "code to verify with client"
        `andBy`    optFlag "" "input-device"
        `Descr`    "an input audio device"
        `andBy`    optFlag "" "output-device"
        `Descr`    "an output audio device"
        )
        "server"
      )
    , ( "client"
      , mkDefaultApp
        (          (\s1 s2 s3 s4 s5 -> Client
                     s1
                     s2
                     (read s3)
                     (if s4 == "" then Nothing else Just (read s4))
                     (if s5 == "" then Nothing else Just (read s5))
                   )
        `parsedBy` reqPos "address"
        `Descr`    "public IP address of the server "
        `andBy`    reqPos "port"
        `Descr`    "server port or service name"
        `andBy`    reqPos "code"
        `Descr`    "code to verify with server"
        `andBy`    optFlag "" "input-device"
        `Descr`    "an input device"
        `andBy`    optFlag "" "output-device"
        `Descr`    "an output device"
        )
        "client"
      )
    , ( "lsdevices"
      , mkDefaultApp
        (ListAudioDevices `parsedBy` boolFlag "v" `Descr` "be verbose")
        "lsdevices"
      )
    ]
  return $ setAppDescr subcommands "Advanced Quantified Communication Program"

{- getOptions
   Reads the argument passed to the command line and parses it.
   RETURNS: Right Options upon success otherwise Left exception
   EXAMPLES: Preset arguments in ghci with :set args server "somePublicIP" "somePort" 123
             getOptions == Server "somePublicIp" "somePort" 123
 -}
getOptions :: IO Options
getOptions = do
  args   <- getArgs
  parser <- makeParser
  case parseArgs args parser of
    Left err ->
      (do
        putStrLn err
        putStrLn ""
        putStrLn "aqcp -h    for help"
        exitSuccess
      )
    Right options -> return options
