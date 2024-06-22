module Lib (isRecordingFile) where

import FFI (c_is_recording_file)
import Foreign.C (withCString)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Foreign.Marshal (toBool)

isRecordingFile :: String -> IO Bool
isRecordingFile str = withCString str $ \c_str -> do
  res <- c_is_recording_file (ConstPtr c_str)
  return $ toBool res
