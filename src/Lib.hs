module Lib (isRecordingFile) where

import FFI (c_is_recording_file)
import Foreign.C (withCString)
import Foreign.Marshal (toBool)

isRecordingFile :: String -> IO Bool
isRecordingFile str = withCString str $ \c_str -> do
  res <- c_is_recording_file c_str
  return $ toBool res
