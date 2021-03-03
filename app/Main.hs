module Main where

import qualified AudioIO
import           Client
import           Control.Monad
import           Data.List                      ( find )
import           Options
import           Server

{- userExit
 - Wait for the user to exit the program.
 - It just prints a string telling how to exit and waits for the user to press return.
 - When return is pressed this IO action will complete.
 - RETURNS: IO ()
 - SIDE_EFFECTS: Prints some text to stdout.
 -               Waits for one line on stdin.
 -}
userExit :: IO ()
userExit = do
  putStrLn "Hit return to exit."
  getLine
  return ()

main :: IO ()
main = do
  options          <- getOptions
  maybeInputDevice <- AudioIO.defaultInputDevice
  let defaultInputDevice = case maybeInputDevice of
        Left err ->
          error $ "Couldn't fetch default audio input device: " ++ show err
        Right x -> x
  maybeOutputDevice <- AudioIO.defaultOutputDevice
  let defaultOutputDevice = case maybeOutputDevice of
        Left err ->
          error $ "Couldn't fetch default audio output device: " ++ show err
        Right x -> x

  maybeDevices <- AudioIO.devices
  let allDevices = case maybeDevices of
        Left  err -> error $ "Failed to fetch audio devices: " ++ show err
        Right x   -> x
      -- Find audio device with the given index.
      -- Or return the provided default device.
      findDeviceOr maybeIndex defaultD = case maybeIndex of
        Nothing -> defaultD
        Just n  -> case find ((== n) . AudioIO.portAudioIndex) allDevices of
          Just x  -> x
          Nothing -> error $ "Couldn't find audio device with index " ++ show n

  case options of
    Server ipAddr serviceName code maybeInputDevice maybeOutputDevice ->
      runServer userExit
                ipAddr
                serviceName
                code
                (findDeviceOr maybeInputDevice defaultInputDevice)
                (findDeviceOr maybeOutputDevice defaultOutputDevice)
    Client ipAddr serviceName code maybeInputDevice maybeOutputDevice ->
      runClient userExit
                ipAddr
                serviceName
                code
                (findDeviceOr maybeInputDevice defaultInputDevice)
                (findDeviceOr maybeOutputDevice defaultOutputDevice)
    CentralServer _ _ ->
      putStrLn
        "The central server isn't done yet. Ask the developers to implement it."
    ListAudioDevices verbose -> forM_
      allDevices
      (\device -> do
        putStr $ show (AudioIO.portAudioIndex device)
        putStr $ ": " ++ (AudioIO.deviceName device)
        when (defaultInputDevice == device) $ putStr " (default input)"
        when (defaultOutputDevice == device) $ putStr " (default output)"
        putStrLn ""
        when verbose $ putStrLn $ "  Default sample rate: " ++ show
          (AudioIO.defaultSampleRate device)
      )
