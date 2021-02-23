module Crypto
  ( PublicKey
  , Code
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
  , serverHandshakeToByteString
  , serverHandshakeFromByteString
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

type Code = Word64

newtype ServerSecretKey = ServerSecretKey SecretKey

newtype ClientSecretKey = ClientSecretKey SecretKey

data ServerHandshake = ServerHandshake PublicKey Nonce Code
data ClientHandshake = ClientHandshake PublicKey Code

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

data Encrypter = Encrypter CombinedKey Nonce

data Decrypter = Decrypter CombinedKey Nonce

data Crypto = Crypto

initCrypto :: IO Crypto
initCrypto = do
  sodiumInit
  return Crypto

newServerHandshake :: Crypto -> Code -> IO (ServerSecretKey, ServerHandshake)
newServerHandshake _ code = do
  (secKey, pubKey) <- newKeypair
  nonce            <- newNonce
  return (ServerSecretKey secKey, ServerHandshake pubKey nonce code)

newClientHandshake :: Crypto -> Code -> IO (ClientSecretKey, ClientHandshake)
newClientHandshake _ code = do
  (secKey, pubKey) <- newKeypair
  return (ClientSecretKey secKey, ClientHandshake pubKey code)

serverHandshakeToByteString :: ServerHandshake -> ByteString
serverHandshakeToByteString = toStrict . encode

serverHandshakeFromByteString :: ByteString -> ServerHandshake
serverHandshakeFromByteString = decode . fromStrict

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

encrypt :: Encrypter -> ByteString -> (Encrypter, ByteString)
encrypt (Encrypter combinedKey encryptNonce) message =
  let cyphertext      = boxAfterNM combinedKey encryptNonce message
      newEncryptNonce = Saltine.Class.nudge $ Saltine.Class.nudge encryptNonce
      newEncrypter    = Encrypter combinedKey newEncryptNonce
  in  (newEncrypter, cyphertext)

decrypt :: Decrypter -> ByteString -> Maybe (Decrypter, ByteString)
decrypt (Decrypter combinedKey decryptNonce) cyphertext =
  (\message ->
      let newDecryptNonce =
              Saltine.Class.nudge $ Saltine.Class.nudge decryptNonce
          newDecrypter = Decrypter combinedKey newDecryptNonce
      in  (newDecrypter, message)
    )
    <$> boxOpenAfterNM combinedKey decryptNonce cyphertext
