module Types (AdditionalInformationPtr, AdditionalInformationFieldPtr, AdditionalInformationKeysArray, RecordingReturnValuePtr) where

import Foreign.C (CChar)
import Foreign.Ptr (Ptr)

type AdditionalInformationPtr = Ptr ()

type AdditionalInformationFieldPtr = Ptr ()

type AdditionalInformationKeysArray = Ptr (Ptr CChar)

type RecordingReturnValuePtr = Ptr ()
