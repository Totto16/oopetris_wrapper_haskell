module Types (AdditionalInformation, AdditionalInformationField, RecordingReturnValue, AdditionalInformationTypeAlias) where

import Foreign.Ptr (Ptr)
import StdInt (CI32)

-- opaque type
data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisAdditionalInformation" #-} AdditionalInformation

-- opaque type
data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisAdditionalInformationField" #-} AdditionalInformationField

-- opaque type
data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisRecordingReturnValue" #-} RecordingReturnValue

-- enum return type
type AdditionalInformationTypeAlias = CI32
