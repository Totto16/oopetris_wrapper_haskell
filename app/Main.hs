module Main (main) where

import Lib (RecordingReturnValue (RecordingReturnError, RecordingReturnSuccess), getRecordingInformation, isRecordingFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      isRecFile <- isRecordingFile file
      if not isRecFile
        then do
          putStrLn "Input file is no recordings file!"
        else do
          returnValue <- getRecordingInformation file
          case returnValue of
            RecordingReturnError err -> putStrLn $ "An error occurred: " ++ err
            RecordingReturnSuccess information -> print information
          return ()
    _ -> putStrLn "Usage: <file>"
