-- Audio input and output from the microphone and to the speakers.
-- Use portaudio as backend.
module AudioIO
  ( InputStream
  , OutputStream
  , Error
  , InputOverflowed
  , OutputUnderflowed
  , DeviceInfo
  , devices
  , defaultInputDevice
  , defaultOutputDevice
  , sampleRate
  , framesPerBuffer
  , withAudioIO
  , readAudio
  , writeAudio
  )
where

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Except
import           Samples
import qualified Sound.PortAudio               as PortAudio
import qualified Sound.PortAudio.Base          as PortAudioBase
import           Data.Vector.Storable           ( Vector )
import qualified Data.Vector.Storable          as Vector
import qualified Data.Vector.Storable.Mutable  as Vector.Mutable
import           Foreign.C.Types                ( CDouble )

-- An error that might occur when port audio initializes or terminates.
type Error = PortAudio.Error

{- A stream from which audio can be read. Like reading from the microphone.
 -}
data InputStream = InputStream (PortAudio.Stream Sample Sample)

{- A stream to which audio can be written. Like writing to the speakers.
 -}
data OutputStream = OutputStream (PortAudio.Stream Sample Sample)

{- An error for when input data is discarded due to a buffer overflow.
 - It typically happens when an input stream is read slower than audio becomes availlable from the microphone.
 -}
data InputOverflowed = InputOverflowed deriving (Show)

{- An error for when silence is sent to the audio output because no audio was availlable.
 - It typically happens when samples are written to an output stream slower than they are played.
 -}
data OutputUnderflowed = OutputUnderflowed deriving (Show)

{- Information about an audio device.
 - Represented by DeviceInfo name maxInputChannels maxOutputChannels defaultSampleRate portAudioIndex
 - INVARIANT: The properties must be similar to that of the portaudio device with the same index.
 -}
data DeviceInfo = DeviceInfo {
                    name :: String,
                    maxInputChannels :: Int,
                    maxOutputChannels :: Int,
                    defaultSampleRate :: CDouble,
                    portAudioIndex :: Int
                  } deriving (Show)

{- mkDeviceInfo deviceIndex portAudioDeviceInfo
 - Construct a DeviceInfo from an portaudio index and portaudio device info.
 - PRE: deviceIndex and deviceInfo must relate to the same device.
 - RETURNS: A DeviceInfo.
 -}
mkDeviceInfo
  :: PortAudioBase.PaDeviceIndex -> PortAudioBase.PaDeviceInfo -> DeviceInfo
mkDeviceInfo index info = DeviceInfo
  (PortAudioBase.name_PaDeviceInfo info)
  (fromIntegral $ PortAudioBase.maxInputChannels info)
  (fromIntegral $ PortAudioBase.maxOutputChannels info)
  (PortAudioBase.defaultSampleRate info)
  (fromIntegral $ PortAudioBase.unPaDeviceIndex index)

{- devices
 - Retrieve information about all possible audio devices.
 - RETURNS: Right [DeviceInfo] upon success else Left Error
 - SIDE_EFFECTS: Initializes and terminates portaudio.
 -}
devices :: IO (Either Error [DeviceInfo])
devices = PortAudio.withPortAudio $ runExceptT
  (do
    numDevices <- liftIO PortAudio.getNumDevices
    forM
      [0 .. numDevices - 1]
      (\index ->
        (do
          let paIndex = fromIntegral index :: PortAudioBase.PaDeviceIndex
          info <- ExceptT $ PortAudio.getDeviceInfo paIndex
          return $ mkDeviceInfo paIndex info
        )
      )
  )

{- defaultInputDevice
 - Get the default input device.
 - RETURNS: Right DeviceInfo upon success or Left Error upon failure.
 - SIDE_EFFECTS: Initializes and terminates portaudio.
 -}
defaultInputDevice :: IO (Either Error DeviceInfo)
defaultInputDevice = PortAudio.withPortAudio $ runExceptT
  (do
    (index, info) <- ExceptT PortAudio.getDefaultInputInfo
    return $ mkDeviceInfo index info
  )

{- defaultOutputDevice
 - Get the default output device.
 - RETURNS: Right DeviceInfo upon success or Left Error upon failure.
 - SIDE_EFFECTS: Initializes and terminates portaudio.
 -}
defaultOutputDevice :: IO (Either Error DeviceInfo)
defaultOutputDevice = PortAudio.withPortAudio $ runExceptT
  (do
    (index, info) <- ExceptT PortAudio.getDefaultOutputInfo
    return $ mkDeviceInfo index info
  )

sampleRate = 22050  -- Hz

channelCount = 1  -- Mono

-- The number of frames (samples) that will be read per each call to readAudio.
framesPerBuffer = 441  -- 20 ms at 22050 Hz

suggestedLatency = 0.3  -- Seconds

{- withAudioIO inputDevice outputDevice computation
 - First initialize the audio machinary and start audio streams, then call the computation with audio input and output streams and lastly shut the audio machinary down.
 - The input and output streams should be written and read from as frequent as needed to keep a steady audio stream. Otherwise the audio might hack or samples might be lost. However, the program will not crash.
 - RETURNS: An IO computation with right the result from the function past in, or Left error if the initialization or termination failes.
 - SIDE_EFFECTS: Initializes portaudio and starts a stream as well as stops the stream and terminates portaudio.
 -}
withAudioIO
  :: DeviceInfo
  -> DeviceInfo
  -> ((InputStream, OutputStream) -> IO a)
  -> IO (Either Error a)
withAudioIO (DeviceInfo _ _ _ _ inDeviceIndex) (DeviceInfo _ _ _ _ outDeviceIndex) computation
  = PortAudio.withPortAudio $ PortAudio.withStream
    (Just $ PortAudio.StreamParameters
      (PortAudioBase.PaDeviceIndex $ fromIntegral inDeviceIndex)
      channelCount
      (PortAudioBase.PaTime suggestedLatency)
    )
    (Just $ PortAudio.StreamParameters
      (PortAudioBase.PaDeviceIndex $ fromIntegral outDeviceIndex)
      channelCount
      (PortAudioBase.PaTime suggestedLatency)
    )
    sampleRate
    (Just framesPerBuffer)
    []
    Nothing
    Nothing
    (\stream -> do
      maybeError <- PortAudio.startStream stream
      case maybeError of
        Just error -> return $ Left error
        Nothing    -> do
          let inputStream  = InputStream stream
              outputStream = OutputStream stream
          result     <- computation (inputStream, outputStream)
          maybeError <- PortAudio.stopStream stream
          case maybeError of
            Just error -> return $ Left error
            Nothing    -> return $ Right result
    )

{- readAudio inputStream
 - Read a vector of samples from the input stream (microphone).
 - Blocks until framesPerBuffer samples are availlable.
 - RETURNS: A tuple of a vector of samples,
 -          and Just InputOverflowed if input data was discarded after the previous call and before this call, else Nothing.
 - SIDE_EFFECTS: Waits until framesPerBuffer samples are availlable from the audio source.
 -               Throws an error if port audio returns an unexpected error.
 -}
readAudio :: InputStream -> IO (Vector Sample, Maybe InputOverflowed)
readAudio (InputStream stream) = do
  -- The readStream function in port audio takes a length and a foreign pointer and then writes samples to that pointer.
  -- We will allocate the memory with a mutable vector and then use the pointer to the vector.
  -- We will then convert the vector to an imutable one and return it.
  --
  -- Allocate a vector into which we will read samples.
  readBuffer <- Vector.Mutable.unsafeNew framesPerBuffer
  -- Take the foreign pointer to the vector.
  let (readPtr, _) = Vector.Mutable.unsafeToForeignPtr0 readBuffer
  -- Read the audio. Will block until all samples are availlable.
  maybeError <- PortAudio.readStream stream
                                     (fromIntegral framesPerBuffer)
                                     readPtr
  -- Convert readBuffer to an imutable vector.
  -- It is safe because the mutable vector (readBuffer) will not be written to any more.
  readAudio <- Vector.unsafeFreeze readBuffer
  -- Check wether the input overflowed or an unexpected error occurred.
  case maybeError of
    Just PortAudio.InputOverflowed -> return (readAudio, Just InputOverflowed)
    Nothing                        -> return (readAudio, Nothing)
    Just err ->
      error
        $  "A bad error occurred while reading audio from portaudio: "
        ++ show err

{- writeAudio outputStream audio
 - Write a vector of samples to the output stream (usually the speaker).
 - RETURNS: Just OutputUnderflowed if port audio needed to infer silence because not enough samples were availlable before this call, else Nothing.
 - SIDE_EFFECTS: Blocks until all samples have been written.
 -               Throws an error if port audio returns an unexpected error.
 -}
writeAudio :: OutputStream -> Vector Sample -> IO (Maybe OutputUnderflowed)
writeAudio (OutputStream stream) samples = do
  -- The writeStream function in port audio takes a length and a pointer to read samples from.
  -- We will take that pointer from the vector with samples.
  -- It is safe because the data pointed to by the pointer will not be modifyed.
  let (pointer, length) = Vector.unsafeToForeignPtr0 samples
  -- Write it to port audio. May block here until all samples are written.
  maybeError <- PortAudio.writeStream stream (fromIntegral length) pointer
  -- Check whether the output underflowed or an unexpected error occurred.
  case maybeError of
    Just PortAudio.OutputUnderflowed -> return $ Just OutputUnderflowed
    Nothing                          -> return Nothing
    Just err ->
      error
        $  "A bad error occurred while writing audio to portaudio: "
        ++ show err
