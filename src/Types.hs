module Types (AdditionalInformationC, AdditionalInformationFieldC, RecordingReturnValueC, EnumTypeC) where

import StdInt (CI32)

-- opaque type
data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisAdditionalInformation" #-} AdditionalInformationC

-- opaque type
data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisAdditionalInformationField" #-} AdditionalInformationFieldC

-- opaque type
data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisRecordingReturnValue" #-} RecordingReturnValueC

-- enum return type
type EnumTypeC = CI32
