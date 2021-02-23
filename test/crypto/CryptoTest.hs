-- A test for the Crypto module.
import           Crypto
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as Char8
import           Test.HUnit

-- The identification code used when handshaking.
code = 12345678900987654321

-- A list of messages which will be encrypted by the server and decrypted by the client.
serverMessages :: [ByteString]
serverMessages = map Char8.pack ["Server1", "Server2", "Server3"]

-- A list of messages which will be encrypted by the client and decrypted by the server.
clientMessages :: [ByteString]
clientMessages = map Char8.pack ["Client1", "Client2", "Client3"]

main :: IO ()
main = do
  -- Initialize the crypto machinery.
  crypto                          <- initCrypto

  -- Generate secret keys and handshakes for both server and client.
  (serverSecret, serverHandshake) <- newServerHandshake crypto code
  (clientSecret, clientHandshake) <- newClientHandshake crypto code

  let
    -- Convert the handshakes to byte strings so they for instance can be sent over a socket.
    binaryServerHandshake = serverHandshakeToByteString serverHandshake
    binaryClientHandshake = clientHandshakeToByteString clientHandshake

    -- Create the server and client's encrypter and decrypter by handshaking at both sides.
    -- This would fail if the identification codes didn't match.
    (Just (serverEncrypter, serverDecrypter)) = createServer
      serverSecret
      serverHandshake
      (clientHandshakeFromByteString binaryClientHandshake)
    (Just (clientEncrypter, clientDecrypter)) = createClient
      clientSecret
      (serverHandshakeFromByteString binaryServerHandshake)
      clientHandshake

    {- encryptMany encrypter messages
     - Encrypt several messages starting with the provided encrypter.
     - After each message is encrypted, a new encrypter is generated from the encrypt function. The same goes for decrypt and makes sure that the messages are decrypted in order and that no messages are lost.
     - RETURNS: A list of cyphertexts for the encrypted messages.
     -}
    encryptMany :: Encrypter -> [ByteString] -> [ByteString]
    -- VARIANT: length messages
    encryptMany encrypter (x : xs) =
      let (newEncrypter, encrypted) = encrypt encrypter x
      in  encrypted : encryptMany newEncrypter xs
    encryptMany _ [] = []

    {- decryptMany decrypter cyphertexts
     - Decrypt the cyphertexts until the end or until a decryption fails.
     - As with encrypt, decrypt returns a new decrypter and that is used to decrypt the next message.
     - It is therefore important that the decryption happens in the same order as the encryption.
     - RETURNS: A list of decrypted messages until the first fail or the end of the input.
     -}
    decryptMany :: Decrypter -> [ByteString] -> [ByteString]
    -- VARIANT: length cyphertexts
    decryptMany decrypter (x : xs) = case decrypt decrypter x of
      Just (newDecrypter, message) -> message : decryptMany newDecrypter xs
      Nothing                      -> []
    decryptMany _ [] = []

    -- Encrypt messages at the server and decrypt them at the client.
    encryptedServerMessages = encryptMany serverEncrypter serverMessages
    decryptedServerMessages =
      decryptMany clientDecrypter encryptedServerMessages
    -- decryptedServerMessages == serverMessages
    -- It worked!

    -- Encrypt messages at the client and decrypt them at the server in reverse.
    -- By decrypting in reverse we will show that the decryption will fail if the decryption is not made in order.
    encryptedClientMessages = encryptMany clientEncrypter clientMessages
    decryptedClientMessagesInReverse =
      decryptMany serverDecrypter $ reverse encryptedClientMessages
    -- decryptedClientMessagesInReverse = []
    -- It failed due to the messages wasn't decrypted in order with the encryption.

  assertEqual "serverMessages" decryptedServerMessages          serverMessages
  assertEqual "clientMessages" decryptedClientMessagesInReverse []
