{-# LANGUAGE CApiFFI #-}

module FFI where

import Foreign (Ptr)
import Foreign.C (CChar, CString)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Foreign.C.Types (CBool (CBool), CDouble (CDouble), CFloat (CFloat))
import StdInt (CI32, CI64, CI8, CU32, CU64, CU8)
import Structs (GridProperties, RecordingInformation)
import Types (AdditionalInformation, AdditionalInformationField, AdditionalInformationTypeAlias, RecordingReturnValue)

foreign import capi "oopetris/oopetris_wrapper.h oopetris_is_recording_file" c_is_recording_file :: ConstPtr CChar -> IO CBool

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_get_keys" c_additional_information_get_keys :: Ptr AdditionalInformation -> ConstPtr (Ptr CChar)

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_keys_free" c_additional_information_keys_free :: Ptr (ConstPtr (Ptr CChar)) -> ()

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_get_field" c_additional_information_get_field :: Ptr AdditionalInformation -> ConstPtr CChar -> ConstPtr AdditionalInformationField

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_type" c_additional_information_field_get_type :: ConstPtr AdditionalInformationField -> AdditionalInformationTypeAlias

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_string" c_additional_information_field_get_string :: ConstPtr AdditionalInformationField -> ConstPtr CChar

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_float" c_additional_information_field_get_float :: ConstPtr AdditionalInformationField -> CFloat

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_double" c_additional_information_field_get_double :: ConstPtr AdditionalInformationField -> CDouble

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_bool" c_additional_information_field_get_bool :: ConstPtr AdditionalInformationField -> CBool

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_u8" c_additional_information_field_get_u8 :: ConstPtr AdditionalInformationField -> CU8

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_i8" c_additional_information_field_get_i8 :: ConstPtr AdditionalInformationField -> CI8

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_u32" c_additional_information_field_get_u32 :: ConstPtr AdditionalInformationField -> CU32

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_i32" c_additional_information_field_get_i32 :: ConstPtr AdditionalInformationField -> CI32

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_u64" c_additional_information_field_get_u64 :: ConstPtr AdditionalInformationField -> CU64

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_i64" c_additional_information_field_get_i64 :: ConstPtr AdditionalInformationField -> CI64

foreign import capi "oopetris/oopetris_wrapper.h oopetris_additional_information_field_get_vector" c_additional_information_field_get_vector :: ConstPtr AdditionalInformationField -> ConstPtr (ConstPtr AdditionalInformationField)

foreign import capi "oopetris/oopetris_wrapper.h oopetris_get_recording_information" c_get_recording_information :: CString -> IO (Ptr RecordingReturnValue)

foreign import capi "oopetris/oopetris_wrapper.h oopetris_is_error" c_is_error :: Ptr RecordingReturnValue -> CBool

foreign import capi "oopetris/oopetris_wrapper.h oopetris_get_error" c_get_error :: Ptr RecordingReturnValue -> ConstPtr CChar

foreign import capi "oopetris/oopetris_wrapper.h oopetris_get_information" c_get_information :: Ptr RecordingReturnValue -> Ptr RecordingInformation

foreign import capi "oopetris/oopetris_wrapper.h oopetris_free_recording_information" c_free_recording_information :: Ptr (Ptr RecordingInformation) -> ()

foreign import capi "oopetris/oopetris_wrapper.h oopetris_free_recording_value_only" c_free_recording_value_only :: Ptr (Ptr RecordingReturnValue) -> ()

foreign import capi "oopetris/oopetris_wrapper.h oopetris_free_recording_value_whole" c_free_recording_value_whole :: Ptr (Ptr RecordingReturnValue) -> ()

foreign import capi "oopetris/oopetris_wrapper.h oopetris_get_lib_version" c_get_lib_version :: ConstPtr CChar

foreign import capi "oopetris/oopetris_wrapper.h oopetris_get_grid_properties" c_get_grid_properties :: Ptr GridProperties

foreign import capi "oopetris/oopetris_wrapper.h oopetris_free_grid_properties" c_free_grid_properties :: Ptr (Ptr GridProperties) -> ()
