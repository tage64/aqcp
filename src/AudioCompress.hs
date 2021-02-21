-- Functions to compress and decompress raw audio. The compressed audio is stored in a ByteString.
module AudioCompress (compressAudio, decompressAudio) where

import           Data.ByteString
import           Data.Maybe
import           Data.Vector.Storable.ByteString as V
import           Samples
import qualified Codec.Compression.LZ4 as LZ

{- compressBytestring bs
   Compresses a bytestring using LZ4 encoding.
   RETURNS: compressed bytestring of bs
   EXAMPLES: TODO
-}

compressBytestring :: ByteString -> ByteString
compressBytestring bs = fromJust (LZ.compress bs)

{- decompressBytestring bs
   Decompresses a bytestring using LZ4 decoding
   PRE: bs must be the result of calling compressBytestring bs.
   RETURNS: TODO
   EXAMPLES: TODO
-}

decompressBytestring :: ByteString -> ByteString
decompressBytestring bs = fromJust (LZ.decompress bs)
{- compress rawAudio
 - Compress a raw stream of audio.
 -}
compressAudio :: SampleList -> ByteString
compressAudio = undefined

{- decompress compressedBytes
 - Decompress some compressed bytes to raw audio.
 - PRE: decompressedBytes must be the result of a call to compress for any raw audio.
 -}
decompressAudio :: ByteString -> SampleList
decompressAudio = undefined
