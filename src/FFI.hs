{-# LANGUAGE CApiFFI #-}

-- {-# INCLUDE <oopetris/oopetris_wrapper.h> #-}

module FFI where

import Foreign (Ptr)
import Foreign.C (CChar, CString)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (CBool), CDouble (CDouble), CFloat (CFloat))
import Foreign.Marshal.Utils (toBool)
import StdInt (CI32, CI64, CI8, CU32, CU64, CU8)
import Structs (GridProperties, RecordingInformation)
import Types (AdditionalInformationFieldPtr, AdditionalInformationKeysArray, AdditionalInformationPtr, RecordingReturnValuePtr)

foreign import capi "oopetris_is_recording_file" c_is_recording_file :: CString -> IO CBool

foreign import capi "oopetris_additional_information_get_keys" c_additional_information_get_keys :: AdditionalInformationPtr -> AdditionalInformationKeysArray

foreign import capi "oopetris_additional_information_keys_free" c_additional_information_keys_free :: Ptr AdditionalInformationKeysArray -> ()

foreign import capi "oopetris_additional_information_get_field" c_additional_information_get_field :: AdditionalInformationPtr -> CString -> AdditionalInformationFieldPtr

type AdditionalInformationTypeAlias = CI32

foreign import capi "oopetris_additional_information_field_get_type" c_additional_information_field_get_type :: AdditionalInformationFieldPtr -> AdditionalInformationTypeAlias

foreign import capi "oopetris_additional_information_field_get_string" c_additional_information_field_get_string :: AdditionalInformationFieldPtr -> CString

foreign import capi "oopetris_additional_information_field_get_float" c_additional_information_field_get_float :: AdditionalInformationFieldPtr -> CFloat

foreign import capi "oopetris_additional_information_field_get_double" c_additional_information_field_get_double :: AdditionalInformationFieldPtr -> CDouble

foreign import capi "oopetris_additional_information_field_get_bool" c_additional_information_field_get_bool :: AdditionalInformationFieldPtr -> CBool

foreign import capi "oopetris_additional_information_field_get_u8" c_additional_information_field_get_u8 :: AdditionalInformationFieldPtr -> CU8

foreign import capi "oopetris_additional_information_field_get_i8" c_additional_information_field_get_i8 :: AdditionalInformationFieldPtr -> CI8

foreign import capi "oopetris_additional_information_field_get_u32" c_additional_information_field_get_u32 :: AdditionalInformationFieldPtr -> CU32

foreign import capi "oopetris_additional_information_field_get_i32" c_additional_information_field_get_i32 :: AdditionalInformationFieldPtr -> CI32

foreign import capi "oopetris_additional_information_field_get_u64" c_additional_information_field_get_u64 :: AdditionalInformationFieldPtr -> CU64

foreign import capi "oopetris_additional_information_field_get_i64" c_additional_information_field_get_i64 :: AdditionalInformationFieldPtr -> CI64

foreign import capi "oopetris_additional_information_field_get_vector" c_additional_information_field_get_vector :: AdditionalInformationFieldPtr -> Ptr AdditionalInformationFieldPtr

foreign import capi "oopetris_get_recording_information" c_get_recording_information :: CString -> IO RecordingReturnValuePtr

foreign import capi "oopetris_is_error" c_is_error :: RecordingReturnValuePtr -> CBool

foreign import capi "oopetris_get_error" c_get_error :: RecordingReturnValuePtr -> CString

foreign import capi "oopetris_get_information" c_get_information :: RecordingReturnValuePtr -> Ptr RecordingInformation

foreign import capi "oopetris_free_recording_information" c_free_recording_information :: Ptr (Ptr RecordingInformation) -> ()

foreign import capi "oopetris_free_recording_value_only" c_free_recording_value_only :: Ptr RecordingReturnValuePtr -> ()

foreign import capi "oopetris_free_recording_value_whole" c_free_recording_value_whole :: Ptr RecordingReturnValuePtr -> ()

foreign import capi "oopetris_get_lib_version" c_get_lib_version :: CString

foreign import capi "oopetris_get_grid_properties" c_get_grid_properties :: Ptr GridProperties

foreign import capi "oopetris_free_grid_properties" c_free_grid_properties :: Ptr (Ptr GridProperties) -> ()
