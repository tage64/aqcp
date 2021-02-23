{- Encrypt and decrypt messages using asymmetric encryption.
 -
 - This module provides an interface where a client and server connects to each other over an insecure connection by a handshake and thereby exchanging public keys and identifying with a code. The handshake will fail if the identification code differs.
 - After handshaking, the client and server gets one encrypter and decrypter each. Those can be used to encrypt or decrypt messages to or from the other.
 - At each encryption/decryption, a new encrypter/decrypter is generated which should be used for the next encryption/decryption in order for the connection to still be secure.
 - A consequence is that the messages must be decrypted in order and with no loss of messages.
 - If the client encrypts a, b and c, the server will succeed to decrypt a, b and c, but fail to decrypt a and c or a, c and b.
 -
 - The general procedure is as follows:
 -  Server:
 -    * Generate a server secret and handshake with newServerHandshake.
 -    * Send the server handshake to the client.
 -    * Receive the client's handshake.
 -    * Create an encrypter and decrypter using the server secret, server handshake and client handshake.
 -    * Encrypt messages to the client and decrypt messages from the client.
 -  Client
 -    The procedure is identical to that of the server but from the client's perspective. I.E. replace server with client and client with server in the function names and you are done.
 - 
 - A code is supplied during initialization (when generating handshakes). It must be identical on both the server and client side.
 - But the code is vissable in the handshakes and will hence not be encrypted. It should just be treated as an identification and not a password.
 -
 - It is somewhat hard to make small individual examples for each function in this module. But an illustrative and commented example is found in test/crypto/CryptoTest.hs.
 -}
module Crypto
  ( Code
  , ServerSecretKey
  , ClientSecretKey
  , ServerHandshake
  , ClientHandshake
  , Encrypter
  , Decrypter
  , Crypto
  , initCrypto
  , newServerHandshake
  , newClientHandshake
  , serversPublicKey
  , clientsPublicKey
  , serversCode
  , clientsCode
  , serverHandshakeToByteString
  , serverHandshakeFromByteString
  , clientHandshakeToByteString
  , clientHandshakeFromByteString
  , createServer
  , createClient
  , encrypt
  , decrypt
  )
where

import           Crypto.Saltine                 ( sodiumInit )
import qualified Crypto.Saltine.Class          as Saltine.Class
import           Crypto.Saltine.Core.Box
import           Data.Binary
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )
import           Data.Word                      ( Word64 )

{- A identification code used when hand shaking.
 - Represented by a Word64 (64 bit positive integer).
 -}
type Code = Word64

-- A secret key for the Server. It should be kept really secret and never be shared with anyone.
newtype ServerSecretKey = ServerSecretKey SecretKey

-- A secret key for the client. It should be kept really secret and never be shared with anyone.
newtype ClientSecretKey = ClientSecretKey SecretKey

{- Server handshake. Used to handshake with the client during connection establishment.
 - Does not need to be kept secret.
 -}
data ServerHandshake = ServerHandshake PublicKey Nonce Code

instance Binary ServerHandshake where
  put (ServerHandshake pubKey nonce code) = do
    put $ Saltine.Class.encode pubKey
    put $ Saltine.Class.encode nonce
    put code

  get = do
    Just pubKey <- Saltine.Class.decode <$> get
    Just nonce  <- Saltine.Class.decode <$> get
    code        <- get
    return $ ServerHandshake pubKey nonce code

{- Client handshake. Used to handshake with the server during connection establishment.
 - Does not need to be kept secret.
 -}
data ClientHandshake = ClientHandshake PublicKey Code

instance Binary ClientHandshake where
  put (ClientHandshake pubKey code) = do
    put $ Saltine.Class.encode pubKey
    put code

  get = do
    Just pubKey <- Saltine.Class.decode <$> get
    code        <- get
    return $ ClientHandshake pubKey code

{- An encrypter used when encrypting messages.
 - Must be kept secret.
 -}
data Encrypter = Encrypter CombinedKey Nonce

{- A decrypter used to decrypt messages.
 - Must be kept secret.
 -}
data Decrypter = Decrypter CombinedKey Nonce

{- A type that is used to indicate that the crypto library is initialized. Doesn't has any other purpose and doesn't contain any data.
 -}
data Crypto = Crypto

{- initCrypto
 - Initialize the crypto library. Must be called before any other function in this module.
 - RETURNS: Crypto
 -}
initCrypto :: IO Crypto
initCrypto = do
  sodiumInit
  return Crypto

{- newServerHandshake crypto code
 - Generate a secret key and handshake for the server from a user supplied code. The code is only used as identification and will not be kept secret.
 - RETURNS: A secret key and handshake for the server.
 -}
newServerHandshake :: Crypto -> Code -> IO (ServerSecretKey, ServerHandshake)
newServerHandshake _ code = do
  (secKey, pubKey) <- newKeypair
  nonce            <- newNonce
  return (ServerSecretKey secKey, ServerHandshake pubKey nonce code)

{- newClientHandshake crypto code
 - Generate a secret key and handshake for the client from a user supplied code. The code is only used as identification and will not be kept secret.
 - RETURNS: A secret key and handshake for the client.
 -}
newClientHandshake :: Crypto -> Code -> IO (ClientSecretKey, ClientHandshake)
newClientHandshake _ code = do
  (secKey, pubKey) <- newKeypair
  return (ClientSecretKey secKey, ClientHandshake pubKey code)

{- serversPublicKey serverHandshake
 - Get the public key for the server from its handshake.
 - RETURNS: A byte string representing the public key.
 -}
serversPublicKey :: ServerHandshake -> ByteString
serversPublicKey (ServerHandshake pubKey _ _) = Saltine.Class.encode pubKey

{- serversCode serverHandshake
 - Get the identification code for the server from its handshake.
 - RETURNS: The code.
 -}
serversCode :: ServerHandshake -> Code
serversCode (ServerHandshake _ _ code) = code

{- clientsPublicKey clientHandshake
 - Get the public key for the client from its handshake.
 - RETURNS: A byte string representing the public key.
 -}
clientsPublicKey :: ClientHandshake -> ByteString
clientsPublicKey (ClientHandshake pubKey _) = Saltine.Class.encode pubKey

{- clientsCode clientHandshake
 - Get the identification code for the client from its handshake.
 - RETURNS: The code.
 -}
clientsCode :: ClientHandshake -> Code
clientsCode (ClientHandshake _ code) = code

{- serverHandshakeToByteString serverHandshake
 - Serialize a server handshake into a byte string.
 - RETURNS: A ByteString representing a server handshake.
 -}
serverHandshakeToByteString :: ServerHandshake -> ByteString
serverHandshakeToByteString = toStrict . encode

{- serverHandshakeFromByteString byteString
 - Deserialize a server handshake from a byte string.
 - PRE: byteString must be generated by serverHandshakeToByteString
 - RETURNS: A server handshake encoded by byteString.
 -}
serverHandshakeFromByteString :: ByteString -> ServerHandshake
serverHandshakeFromByteString = decode . fromStrict

{- clientHandshakeToByteString clientHandshake
 - Serialize a client handshake into a byte string.
 - RETURNS: A ByteString representing a client handshake.
 -}
clientHandshakeToByteString :: ClientHandshake -> ByteString
clientHandshakeToByteString = toStrict . encode

{- clientHandshakeFromByteString byteString
 - Deserialize a client handshake from a byte string.
 - PRE: byteString must be generated by clientHandshakeToByteString
 - RETURNS: A client handshake encoded by byteString.
 -}
clientHandshakeFromByteString :: ByteString -> ClientHandshake
clientHandshakeFromByteString = decode . fromStrict

{- createServer secretKey serverHandshake clientHandshake
 - Create an encrypter and decrypter for the server from a secret key and handshakes from both server and client. Failes if the codes in the handshakes differs.
 - PRE: secretKey and serverHandshake must be generated from the same call to newServerHandshake.
 - RETURNS: Just encrypter decrypter if the codes in the handshakes match, else Nothing
 -}
createServer
  :: ServerSecretKey
  -> ServerHandshake
  -> ClientHandshake
  -> Maybe (Encrypter, Decrypter)
createServer (ServerSecretKey serverSecKey) (ServerHandshake _ encryptNonce serverCode) (ClientHandshake clientPubKey clientCode)
  = let
      combinedKey  = beforeNM serverSecKey clientPubKey
      decryptNonce = Saltine.Class.nudge encryptNonce
    in
      if serverCode == clientCode
        then
          Just
            ( Encrypter combinedKey encryptNonce
            , Decrypter combinedKey decryptNonce
            )
        else Nothing

{- createClient secretKey serverHandshake clientHandshake
 - Create an encrypter and decrypter for the client from a secret key and handshakes from both server and client. Failes if the codes in the handshakes differs.
 - PRE: secretKey and clientHandshake must be generated from the same call to newClientHandshake.
 - RETURNS: Just encrypter decrypter if the codes in the handshakes match, else Nothing
 -}
createClient
  :: ClientSecretKey
  -> ServerHandshake
  -> ClientHandshake
  -> Maybe (Encrypter, Decrypter)
createClient (ClientSecretKey clientSecKey) (ServerHandshake serverPubKey decryptNonce serverCode) (ClientHandshake _ clientCode)
  = let
      combinedKey  = beforeNM clientSecKey serverPubKey
      encryptNonce = Saltine.Class.nudge decryptNonce
    in
      if serverCode == clientCode
        then
          Just
            ( Encrypter combinedKey encryptNonce
            , Decrypter combinedKey decryptNonce
            )
        else Nothing

{- encrypt encrypter message
 - Encrypt a message with the provided encrypter. And return a new encrypter to encrypt the next message with.
 - This encrypter should not be used after encrypting this message.
 - RETURNS: A new encrypter and an encrypted version of the message.
 -}
encrypt :: Encrypter -> ByteString -> (Encrypter, ByteString)
encrypt (Encrypter combinedKey encryptNonce) message =
  let cyphertext      = boxAfterNM combinedKey encryptNonce message
      newEncryptNonce = Saltine.Class.nudge $ Saltine.Class.nudge encryptNonce
      newEncrypter    = Encrypter combinedKey newEncryptNonce
  in  (newEncrypter, cyphertext)

{- decrypt decrypter cyphertext
 - Try to decrypt the message.
 - The decryption will succeed if:
 -  * cyphertext was computed by encrypt encrypter message and
 -  * encrypter and decrypter are either generated at opposite sides of a handshake or
 -  * encrypter is generated from a previous call to encrypt and decrypter is generated from a successful previous call to decrypt where the cyphertext was generated by that previous call to encrypt.
 - In short: Switch to the new encrypter after encrypting a message, switch to the new decrypter after decrypting a message and decrypt all messages in the same order as encrypted.
 - RETURNS: Just newDecrypter message if the decryption succeeds else Nothing
 -}
decrypt :: Decrypter -> ByteString -> Maybe (Decrypter, ByteString)
decrypt (Decrypter combinedKey decryptNonce) cyphertext =
  (\message ->
      let newDecryptNonce =
              Saltine.Class.nudge $ Saltine.Class.nudge decryptNonce
          newDecrypter = Decrypter combinedKey newDecryptNonce
      in  (newDecrypter, message)
    )
    <$> boxOpenAfterNM combinedKey decryptNonce cyphertext
