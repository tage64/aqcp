import           Crypto
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as Char8
import           Test.HUnit

code = 12345678900987654321
serverMessages :: [ByteString]
serverMessages = map Char8.pack ["Server1", "Server2", "Server3"]
clientMessages :: [ByteString]
clientMessages = map Char8.pack ["Client1", "Client2", "Client3"]

main :: IO ()
main = do
  crypto                          <- initCrypto
  (serverSecret, serverHandshake) <- newServerHandshake crypto code
  (clientSecret, clientHandshake) <- newClientHandshake crypto code
  let
    (Just (serverEncrypter, serverDecrypter)) =
      createServer serverSecret serverHandshake clientHandshake
    (Just (clientEncrypter, clientDecrypter)) =
      createClient clientSecret serverHandshake clientHandshake
    encryptMany :: Encrypter -> [ByteString] -> [ByteString]
    encryptMany encrypter (x : xs) =
      let (newEncrypter, encrypted) = encrypt encrypter x
      in  encrypted : encryptMany newEncrypter xs
    encryptMany _ [] = []

    decryptMany :: Decrypter -> [ByteString] -> [ByteString]
    decryptMany decrypter (x : xs) = case decrypt decrypter x of
      Just (newDecrypter, message) -> message : decryptMany newDecrypter xs
      Nothing                      -> []
    decryptMany _ [] = []

    encryptedServerMessages = encryptMany serverEncrypter serverMessages
    decryptedServerMessages =
      decryptMany clientDecrypter encryptedServerMessages

    encryptedClientMessages = encryptMany clientEncrypter clientMessages
    decryptedClientMessagesInReverse =
      decryptMany serverDecrypter $ reverse encryptedClientMessages

  assertEqual "serverMessages" decryptedServerMessages          serverMessages
  assertEqual "clientMessages" decryptedClientMessagesInReverse []
