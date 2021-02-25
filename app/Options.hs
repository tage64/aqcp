-- Module comment TODO
module Options (Options, getOptions) where

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

{- getOptions
 - TODO
 -}
getOptions :: IO Options
getOptions = undefined
