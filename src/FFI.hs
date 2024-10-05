{-# LANGUAGE CApiFFI #-}

module FFI
  ( c_is_recording_file,
    c_additional_information_get_keys,
    c_additional_information_keys_free,
    c_additional_information_get_field,
    c_additional_information_field_get_type,
    c_additional_information_field_get_string,
    c_additional_information_field_get_float,
    c_additional_information_field_get_double,
    c_additional_information_field_get_bool,
    c_additional_information_field_get_u8,
    c_additional_information_field_get_i8,
    c_additional_information_field_get_u32,
    c_additional_information_field_get_i32,
    c_additional_information_field_get_u64,
    c_additional_information_field_get_i64,
    c_additional_information_field_get_vector,
    c_get_recording_information,
    c_is_error,
    c_get_error,
    c_get_information,
    c_free_recording_information,
    c_free_recording_value_only,
    c_free_recording_value_whole,
    c_get_lib_version,
    c_get_grid_properties,
    c_free_grid_properties,
    c_array_len,
  )
where

import Foreign (Ptr)
import Foreign.C (CChar, CSize (CSize))
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Foreign.C.Types (CBool (CBool), CDouble (CDouble), CFloat (CFloat))
import Foreign.Ptr (FunPtr)
import StdInt (CI32, CI64, CI8, CU32, CU64, CU8)
import Structs (GridPropertiesC, RecordingInformationC)
import Types (AdditionalInformationC, AdditionalInformationFieldC, EnumTypeC, RecordingReturnValueC)

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_is_recording_file" c_is_recording_file :: ConstPtr CChar -> IO CBool

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_get_keys" c_additional_information_get_keys :: Ptr AdditionalInformationC -> IO (ConstPtr (Ptr CChar))

foreign import capi "oopetris/c_wrapper/wrapper.h &oopetris_additional_information_keys_free" c_additional_information_keys_free :: FunPtr (Ptr (Ptr CChar) -> IO ())

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_get_field" c_additional_information_get_field :: Ptr AdditionalInformationC -> ConstPtr CChar -> ConstPtr AdditionalInformationFieldC

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_type" c_additional_information_field_get_type :: ConstPtr AdditionalInformationFieldC -> EnumTypeC

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_string" c_additional_information_field_get_string :: ConstPtr AdditionalInformationFieldC -> ConstPtr CChar

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_float" c_additional_information_field_get_float :: ConstPtr AdditionalInformationFieldC -> CFloat

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_double" c_additional_information_field_get_double :: ConstPtr AdditionalInformationFieldC -> CDouble

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_bool" c_additional_information_field_get_bool :: ConstPtr AdditionalInformationFieldC -> CBool

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_u8" c_additional_information_field_get_u8 :: ConstPtr AdditionalInformationFieldC -> CU8

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_i8" c_additional_information_field_get_i8 :: ConstPtr AdditionalInformationFieldC -> CI8

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_u32" c_additional_information_field_get_u32 :: ConstPtr AdditionalInformationFieldC -> CU32

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_i32" c_additional_information_field_get_i32 :: ConstPtr AdditionalInformationFieldC -> CI32

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_u64" c_additional_information_field_get_u64 :: ConstPtr AdditionalInformationFieldC -> CU64

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_i64" c_additional_information_field_get_i64 :: ConstPtr AdditionalInformationFieldC -> CI64

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_additional_information_field_get_vector" c_additional_information_field_get_vector :: ConstPtr AdditionalInformationFieldC -> ConstPtr (ConstPtr AdditionalInformationFieldC)

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_get_recording_information" c_get_recording_information :: ConstPtr CChar -> IO (Ptr RecordingReturnValueC)

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_is_error" c_is_error :: Ptr RecordingReturnValueC -> CBool

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_get_error" c_get_error :: Ptr RecordingReturnValueC -> ConstPtr CChar

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_get_information" c_get_information :: Ptr RecordingReturnValueC -> Ptr RecordingInformationC

foreign import capi "oopetris/c_wrapper/wrapper.h &oopetris_free_recording_information" c_free_recording_information :: FunPtr (Ptr RecordingInformationC -> IO ())

foreign import capi "oopetris/c_wrapper/wrapper.h &oopetris_free_recording_value_only" c_free_recording_value_only :: FunPtr (Ptr RecordingReturnValueC -> IO ())

foreign import capi "oopetris/c_wrapper/wrapper.h &oopetris_free_recording_value_whole" c_free_recording_value_whole :: FunPtr (Ptr RecordingReturnValueC -> IO ())

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_get_lib_version" c_get_lib_version :: ConstPtr CChar

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_get_grid_properties" c_get_grid_properties :: IO (Ptr GridPropertiesC)

foreign import capi "oopetris/c_wrapper/wrapper.h &oopetris_free_grid_properties" c_free_grid_properties :: FunPtr (Ptr GridPropertiesC -> IO ())

foreign import capi "oopetris/c_wrapper/wrapper.h oopetris_array_len" c_array_len :: Ptr () -> CSize
