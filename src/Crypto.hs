module Crypto
  ( Keypair
  , ServerHandShake
  , ClientHandShake
  , SecretConnection
  , Crypto
  , initCrypto
  , newKeypair
  , serverHandShake
  , clientHandShake
  , createServer
  , createClient
  , encrypt
  , decrypt
  )
where

import           Crypto.Saltine                 ( sodiumInit )
import           Crypto.Saltine.Class           ( nudge )
import           Crypto.Saltine.Core.Box hiding ( Keypair
                                                , newKeypair
                                                )
import qualified Crypto.Saltine.Core.Box       as SaltineBox
import           Data.ByteString                ( ByteString )

data Keypair = Keypair SecretKey PublicKey

data ServerHandShake = ServerHandShake PublicKey Nonce
data ClientHandShake = ClientHandShake PublicKey

data SecretConnection = SecretConnection CombinedKey Nonce Nonce

data Crypto = Crypto

initCrypto :: IO Crypto
initCrypto = do
  sodiumInit
  return Crypto

newKeypair :: Crypto -> IO Keypair
newKeypair _ = do
  (secKey, pubKey) <- SaltineBox.newKeypair
  return $ Keypair secKey pubKey

serverHandShake :: Keypair -> IO ServerHandShake
serverHandShake (Keypair _ pubKey) = do
  nonce <- newNonce
  return $ ServerHandShake pubKey nonce

clientHandShake :: Keypair -> IO ClientHandShake
clientHandShake (Keypair _ pubKey) = return $ ClientHandShake pubKey


createServer
  :: Keypair -> ServerHandShake -> ClientHandShake -> SecretConnection
createServer keypair@(Keypair serverSecKey _) (ServerHandShake _ encryptNonce) (ClientHandShake clientPubKey)
  = let combinedKey  = beforeNM serverSecKey clientPubKey
        decryptNonce = nudge encryptNonce
    in  SecretConnection combinedKey encryptNonce decryptNonce

createClient
  :: Keypair -> ServerHandShake -> ClientHandShake -> SecretConnection
createClient keypair@(Keypair clientSecKey _) (ServerHandShake serverPubKey decryptNonce) _
  = let combinedKey  = beforeNM clientSecKey serverPubKey
        encryptNonce = nudge decryptNonce
    in  SecretConnection combinedKey encryptNonce decryptNonce

encrypt :: SecretConnection -> ByteString -> (SecretConnection, ByteString)
encrypt (SecretConnection combinedKey encryptNonce decryptNonce) message =
  let
    cyphertext      = boxAfterNM combinedKey encryptNonce message
    newEncryptNonce = nudge $ nudge encryptNonce
    newSecretConnection =
      SecretConnection combinedKey newEncryptNonce decryptNonce
  in
    (newSecretConnection, cyphertext)

decrypt
  :: SecretConnection -> ByteString -> (SecretConnection, Maybe ByteString)
decrypt (SecretConnection combinedKey encryptNonce decryptNonce) cyphertext =
  let
    message         = boxOpenAfterNM combinedKey decryptNonce cyphertext
    newDecryptNonce = nudge $ nudge decryptNonce
    newSecretConnection =
      SecretConnection combinedKey encryptNonce newDecryptNonce
  in
    (newSecretConnection, message)
