module Main where

import qualified AudioIO
import           Client
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
  let inputDevice = case maybeInputDevice of
        Left err ->
          error $ "Couldn't fetch default audio input device: " ++ show err
        Right x -> x
  putStrLn $ "Audio input device: " ++ AudioIO.deviceName inputDevice
  maybeOutputDevice <- AudioIO.defaultOutputDevice
  let outputDevice = case maybeOutputDevice of
        Left err ->
          error $ "Couldn't fetch default audio output device: " ++ show err
        Right x -> x
  putStrLn $ "Audio output device: " ++ AudioIO.deviceName outputDevice

  case options of
    Server ipAddr serviceName code ->
      runServer userExit ipAddr serviceName code inputDevice outputDevice
    Client ipAddr serviceName code ->
      runClient userExit ipAddr serviceName code inputDevice outputDevice
    CentralServer _ _ -> putStrLn
      "The central server isn't done yet. Ask the developers to implement it."
