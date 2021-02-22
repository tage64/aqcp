-- Audio input and output from the microphone and to the speakers.
-- Use portaudio as backend.
module AudioIO
  ( StreamHandle
  , Error
  , withStream
  )
where

import           Control.Concurrent.Chan
import           Foreign.C.Types                ( CULong )
import           Foreign.ForeignPtr             ( newForeignPtr_ )
import           Foreign.Ptr                    ( Ptr )
import           Samples
import           Sound.PortAudio.Base           ( PaStreamCallbackTimeInfo )
import           Sound.PortAudio         hiding ( withStream )
import qualified Sound.PortAudio               as PortAudio
import qualified Data.Vector.Storable          as Vector

type StreamHandle = Stream Sample Sample


sampleRate = 22050  -- Hz

channelCount = 1  -- Mono

framesPerBuffer = 441  -- 20 ms at 22050 Hz

withStream
  :: (Chan SampleList -> Chan SampleList -> StreamHandle -> IO (Either Error a))
  -> IO (Either Error a)
withStream computation = do
  inputChan  <- newChan
  outputChan <- newChan
  let computation_ = computation inputChan outputChan
      streamCallback
        :: PaStreamCallbackTimeInfo
        -> [StreamCallbackFlag]
        -> CULong
        -> Ptr Sample
        -> Ptr Sample
        -> IO StreamResult
      streamCallback _ _ nFrames_ inputPtr outputPtr = do
        let nFrames = fromIntegral nFrames_ :: Int
        foreignInputPtr  <- newForeignPtr_ inputPtr
        foreignOutputPtr <- newForeignPtr_ outputPtr
        let inputVec  = Vector.unsafeFromForeignPtr0 foreignInputPtr nFrames
            outputVec = Vector.unsafeFromForeignPtr0 foreignOutputPtr nFrames
        undefined

  PortAudio.withDefaultStream channelCount
                              channelCount
                              sampleRate
                              (Just framesPerBuffer)
                              (Just streamCallback)
                              Nothing
                              computation_
