-- Functions to compress and decompress raw audio. The compressed audio is stored in a ByteString.
module AudioCompress (compress, decompress) where

import           Data.ByteString
import           Samples

{- compress rawAudio
 - Compress a raw stream of audio.
 -}
compress :: SampleList -> ByteString
compress = undefined

{- decompress compressedBytes
 - Decompress some compressed bytes to raw audio.
 - PRE: decompressedBytes must be the result of a call to compress for any raw audio.
 -}
decompress :: ByteString -> SampleList
decompress = undefined
