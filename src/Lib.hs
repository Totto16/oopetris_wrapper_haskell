module Lib (isRecordingFile, getLibVersion, getGridProperties, getRecordingInformation, RecordingReturnValue (..)) where

import qualified AdditionalInformationType as AdType
import Data.HashMap.Strict (fromList)
import FFI (c_additional_information_field_get_bool, c_additional_information_field_get_double, c_additional_information_field_get_float, c_additional_information_field_get_i32, c_additional_information_field_get_i64, c_additional_information_field_get_i8, c_additional_information_field_get_string, c_additional_information_field_get_type, c_additional_information_field_get_u32, c_additional_information_field_get_u64, c_additional_information_field_get_u8, c_additional_information_field_get_vector, c_additional_information_get_field, c_additional_information_get_keys, c_additional_information_keys_free, c_array_len, c_free_grid_properties, c_free_recording_value_whole, c_get_error, c_get_grid_properties, c_get_information, c_get_lib_version, c_get_recording_information, c_is_error, c_is_recording_file)
import Foreign (Storable (sizeOf), withForeignPtr)
import Foreign.C (CChar (), CDouble (CDouble), CFloat (CFloat), withCString)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr, unConstPtr))
import Foreign.C.String (peekCAString)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr)
import Foreign.Marshal (toBool)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (peek))
import Structs (AdditionalInformation, AdditionalInformationField (..), GridProperties (GridProperties), GridPropertiesC (c_height, c_width), Mino (Mino), MinoC (..), MinoPosition (MinoPosition), MinoPositionC (..), RecordingInformation (RecordingInformation), RecordingInformationC (..), RecordingReturnValue (RecordingReturnError, RecordingReturnSuccess), TetrionHeader (TetrionHeader), TetrionHeaderC (..), TetrionRecord (TetrionRecord), TetrionRecordC (..), TetrionSnapshot (TetrionSnapshot), TetrionSnapshotC (..))
import Types (AdditionalInformationC, AdditionalInformationFieldC, RecordingReturnValueC)

isRecordingFile :: String -> IO Bool
isRecordingFile file = withCString file $ \c_str -> do
  res <- c_is_recording_file (ConstPtr c_str)
  return $ toBool res

getLibVersion :: IO String
getLibVersion = peekCAString (unConstPtr c_get_lib_version)

gridPropertiesFinalizer :: FinalizerPtr GridPropertiesC
gridPropertiesFinalizer = c_free_grid_properties

getGridPropertiesForeign :: IO (ForeignPtr GridPropertiesC)
getGridPropertiesForeign = do
  propertiesRawPtr <- c_get_grid_properties
  newForeignPtr gridPropertiesFinalizer propertiesRawPtr

getGridProperties :: IO GridProperties
getGridProperties = do
  foreignPtr <- getGridPropertiesForeign
  struct <- withForeignPtr foreignPtr peek
  return $ GridProperties (fromInteger . toInteger $ c_height struct) (fromInteger . toInteger $ c_width struct)

sizeOfPtr :: (Storable a) => Ptr a -> Int
sizeOfPtr p = impl p undefined
  where
    impl :: (Storable b) => Ptr b -> b -> Int
    impl _ = sizeOf

cArrayMap :: (Storable a) => Ptr a -> (Ptr a -> IO b) -> IO [b]
cArrayMap ptr _fn = do
  let len = toInteger $ c_array_len (castPtr ptr :: Ptr ())
  reversedList <- collect len ptr _fn []
  return $ reverse reversedList
  where
    collect :: (Storable c) => Integer -> Ptr c -> (Ptr c -> IO b) -> [b] -> IO [b]
    collect 0 _ _ xs = pure xs
    collect count currentPtr fn list = do
      res <- fn currentPtr
      let newPtr = plusPtr currentPtr (sizeOfPtr currentPtr)
      collect (count - 1) newPtr fn (res : list)

additionalInformationKeysFinalizer :: FinalizerPtr (Ptr CChar)
additionalInformationKeysFinalizer = c_additional_information_keys_free

additionalInformationKeysForeign :: Ptr AdditionalInformationC -> IO (ForeignPtr (Ptr CChar))
additionalInformationKeysForeign ptr = do
  valueRawPtr <- c_additional_information_get_keys ptr
  newForeignPtr additionalInformationKeysFinalizer (unConstPtr valueRawPtr)

getAdditionalInformationFieldHs :: ConstPtr AdditionalInformationFieldC -> IO AdditionalInformationField
getAdditionalInformationFieldHs ptr1 = do
  let _type = toEnum . fromInteger . toInteger $ c_additional_information_field_get_type ptr1
  getFieldRaw _type ptr1
  where
    getFieldRaw :: AdType.AdditionalInformationType -> ConstPtr AdditionalInformationFieldC -> IO AdditionalInformationField
    getFieldRaw AdType.String ptr = do
      let cstr = c_additional_information_field_get_string ptr
      str <- peekCAString (unConstPtr cstr)
      return $ FieldString str
    getFieldRaw AdType.Float ptr = do
      let value = c_additional_information_field_get_float ptr
      return $ FieldFloat (valueOf value)
      where
        valueOf :: CFloat -> Float
        valueOf (CFloat f) = f
    getFieldRaw AdType.Double ptr = do
      let value = c_additional_information_field_get_double ptr
      return $ FieldDouble (valueOf value)
      where
        valueOf :: CDouble -> Double
        valueOf (CDouble d) = d
    getFieldRaw AdType.Bool ptr = do
      let value = c_additional_information_field_get_bool ptr
      return $ FieldBool (toBool value)
    getFieldRaw AdType.U8 ptr = do
      let value = c_additional_information_field_get_u8 ptr
      return $ FieldU8 (fromInteger . toInteger $ value)
    getFieldRaw AdType.I8 ptr = do
      let value = c_additional_information_field_get_i8 ptr
      return $ FieldI8 (fromInteger . toInteger $ value)
    getFieldRaw AdType.U32 ptr = do
      let value = c_additional_information_field_get_u32 ptr
      return $ FieldU32 (fromInteger . toInteger $ value)
    getFieldRaw AdType.I32 ptr = do
      let value = c_additional_information_field_get_i32 ptr
      return $ FieldI32 (fromInteger . toInteger $ value)
    getFieldRaw AdType.U64 ptr = do
      let value = c_additional_information_field_get_u64 ptr
      return $ FieldU64 (fromInteger . toInteger $ value)
    getFieldRaw AdType.I64 ptr = do
      let value = c_additional_information_field_get_i64 ptr
      return $ FieldI64 (fromInteger . toInteger $ value)
    getFieldRaw AdType.Vector ptr = do
      let value = c_additional_information_field_get_vector ptr
      vec <- cArrayMap (unConstPtr value) getVectorElem
      return $ FieldVector vec
      where
        getVectorElem :: Ptr (ConstPtr AdditionalInformationFieldC) -> IO AdditionalInformationField
        getVectorElem ptr2 = do
          val <- peek ptr2
          getAdditionalInformationFieldHs val

getAdditionalInformationField :: Ptr AdditionalInformationC -> (String, Ptr CChar) -> IO (String, AdditionalInformationField)
getAdditionalInformationField ptr (str, cstr) = do
  let fieldRaw = c_additional_information_get_field ptr (ConstPtr cstr)
  field <- getAdditionalInformationFieldHs fieldRaw
  return (str, field)

getAdditionalInformationList :: Ptr AdditionalInformationC -> [(String, Ptr CChar)] -> IO [(String, AdditionalInformationField)]
getAdditionalInformationList ptr keys = do mapM (getAdditionalInformationField ptr) keys

getAdditionalInformation :: Ptr AdditionalInformationC -> IO AdditionalInformation
getAdditionalInformation ptr = do
  foreignKeys <- additionalInformationKeysForeign ptr
  keys <- withForeignPtr foreignKeys (`cArrayMap` getKey)
  informationList <- getAdditionalInformationList ptr keys
  return $ fromList informationList
  where
    getKey :: Ptr (Ptr CChar) -> IO (String, Ptr CChar)
    getKey ptr1 = do
      stringAddr <- peek ptr1
      string <- peekCAString stringAddr
      return (string, stringAddr)

getRecords :: Ptr TetrionRecordC -> IO [TetrionRecord]
getRecords ptr = do
  cArrayMap ptr getRecord
  where
    getRecord :: Ptr TetrionRecordC -> IO TetrionRecord
    getRecord ptr1 = do
      struct <- peek ptr1
      return $ TetrionRecord (fromInteger . toInteger $ c_r_simulation_step_index struct) (toEnum . fromInteger . toInteger $ c_event struct) (fromInteger . toInteger $ c_r_tetrion_index struct)

getSnaphots :: Ptr TetrionSnapshotC -> IO [TetrionSnapshot]
getSnaphots ptr = do
  cArrayMap ptr getSnaphot
  where
    getMinoPosition :: MinoPositionC -> MinoPosition
    getMinoPosition pos = MinoPosition (fromInteger . toInteger $ c_x pos) (fromInteger . toInteger $ c_y pos)
    getMino :: Ptr MinoC -> IO Mino
    getMino ptr1 = do
      struct <- peek ptr1
      let mino_position = getMinoPosition (c_position struct)
      return $ Mino mino_position (toEnum . fromInteger . toInteger $ c_type struct)

    getSnaphot :: Ptr TetrionSnapshotC -> IO TetrionSnapshot
    getSnaphot ptr1 = do
      struct <- peek ptr1
      minos <- cArrayMap (c_mino_stack struct) getMino
      return $ TetrionSnapshot (fromInteger . toInteger $ c_level struct) minos (fromInteger . toInteger $ c_score struct) (fromInteger . toInteger $ c_s_simulation_step_index struct) (fromInteger . toInteger $ c_lines_cleared struct) (fromInteger . toInteger $ c_s_tetrion_index struct)

getTetrionHeaders :: Ptr TetrionHeaderC -> IO [TetrionHeader]
getTetrionHeaders ptr = do
  cArrayMap ptr getTetrionHeader
  where
    getTetrionHeader :: Ptr TetrionHeaderC -> IO TetrionHeader
    getTetrionHeader ptr1 = do
      struct <- peek ptr1
      return $ TetrionHeader (fromInteger . toInteger $ c_seed struct) (fromInteger . toInteger $ c_starting_level struct)

getInformation :: Ptr RecordingInformationC -> IO RecordingInformation
getInformation ptr = do
  struct <- peek ptr
  information <- getAdditionalInformation $ c_information struct
  records <- getRecords $ c_records struct
  snapshots <- getSnaphots $ c_snapshots struct
  tetrion_headers <- getTetrionHeaders $ c_tetrion_headers struct
  let version = fromInteger . toInteger $ c_version struct
  return $ RecordingInformation information records snapshots tetrion_headers version

recordingReturnValueFinalizer :: FinalizerPtr RecordingReturnValueC
recordingReturnValueFinalizer = c_free_recording_value_whole

recordingReturnValueForeign :: String -> IO (ForeignPtr RecordingReturnValueC)
recordingReturnValueForeign file = withCString file $ \c_str -> do
  valueRawPtr <- c_get_recording_information (ConstPtr c_str)
  newForeignPtr recordingReturnValueFinalizer valueRawPtr

getRecordingInformation :: String -> IO RecordingReturnValue
getRecordingInformation file = do
  foreignPtr <- recordingReturnValueForeign file
  isError <- withForeignPtr foreignPtr (pure . c_is_error)
  if toBool isError
    then do
      errorCStr <- withForeignPtr foreignPtr (pure . c_get_error)
      errorString <- peekCAString (unConstPtr errorCStr)
      return $ RecordingReturnError errorString
    else do
      informationC <- withForeignPtr foreignPtr (pure . c_get_information)
      information <- getInformation informationC
      return $ RecordingReturnSuccess information
