-- Functions to compress and decompress raw audio. The compressed audio is stored in a ByteString.
module AudioCompress (compress, decompress) where

import           Data.ByteString
import           Data.Maybe
import           Data.Vector.Storable.ByteString as V
import           Samples
import qualified Codec.Compression.LZ4 as LZ

{- compress rawAudio
   Compress a vector storing raw audio data.
   RETURNS: rawAudio to compressed ByteString
   EXAMPLES: compress sampleList == "\n\NUL\NUL\NUL\v\NUL\NUL\NUL\160\n\NUL\n\NUL\n\NUL\n\NUL\n\NUL"
             compress []         == error
 -}

compress :: SampleList -> ByteString
compress sl = compressBytestring (listToBytestring sl)

{- decompress compressedBytes
   Decompress some compressed bytes to raw audio.
   PRE: decompressedBytes must be the result of a call to compress for any raw audio.
   RETURNS: uncompressed ByteString of compressBytes
   EXAMPLES: decompress byteString      == [10,10,10,10,10]
             decompress emptyByteString == [] 
 -}

decompress :: ByteString -> SampleList
decompress bs = bytestringToList (decompressBytestring bs)

{- compressBytestring bs
   Compresses a bytestring using LZ4 encoding.
   RETURNS: compressed bytestring of bs
   EXAMPLES: compressBytestring "\SOH\SOH" == "\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL \SOH\SOH"
-}

compressBytestring :: ByteString -> ByteString
compressBytestring bs = fromJust (LZ.compress bs)

{- decompressBytestring bs
   Decompresses a bytestring using LZ4 decoding
   PRE: bs must be the result of calling compressBytestring bs.
   RETURNS: Decompressed bytestring of bs
   EXAMPLES: decompress "STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL == \SOH\SOH 
-}

decompressBytestring :: ByteString -> ByteString
decompressBytestring bs = fromJust (LZ.decompress bs)

{- listToBytestring sampleList
   Converts a Vector to a ByteString
   RETURNS: sampleList as a ByteString
   EXAMPLES: listToBytestring [10,10] == "\n\NUL\n\NUL"
-}

listToBytestring :: SampleList -> ByteString
listToBytestring sl = vectorToByteString sl

{- bytestringTolist byteString
   Converts a ByteString to a Vector
   RETURNS: byteString as a Vector
   EXAMPLES: bytestringToList "\n\NUL\n\NUL" == [10,10]
-}

bytestringToList :: ByteString -> SampleList
bytestringToList bs = byteStringToVector bs
