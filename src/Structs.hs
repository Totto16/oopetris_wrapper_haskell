{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Structs
  ( TetrionRecord (..),
    MinoPosition (..),
    Mino (..),
    TetrionSnapshot (..),
    TetrionHeader (..),
    RecordingInformation (..),
    GridProperties (..),
    RecordingInformationC (..),
    RecordingReturnValue (..),
    AdditionalInformationField (..),
    AdditionalInformation,
    GridPropertiesC (..),
    TetrionRecordC (..),
    MinoPositionC (..),
    MinoC (..),
    TetrionSnapshotC (..),
    TetrionHeaderC (..),
  )
where

import Data.HashMap (Map)
import Data.Int (Int32, Int64, Int8)
import Data.Word (Word32, Word64, Word8)
import Foreign.CStorable (CStorable (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import InputEvent (InputEvent)
import StdInt (CU32, CU64, CU8)
import TetrominoType (TetrominoType)
import Types (AdditionalInformationC, EnumTypeC)

data RecordingReturnValue = RecordingReturnSuccess RecordingInformation | RecordingReturnError String
  deriving (Eq, Show)

data AdditionalInformationField
  = FieldString String
  | FieldFloat Float
  | FieldDouble Double
  | FieldBool Bool
  | FieldU8 Word8
  | FieldI8 Int8
  | FieldU32 Word32
  | FieldI32 Int32
  | FieldU64 Word64
  | FieldI64 Int64
  | FieldVector [AdditionalInformationField]
  deriving (Eq, Show)

type AdditionalInformation = Map String AdditionalInformationField

-- | This aligns the sizeof, so that it works correctly with the library!!
cSizeOfPatch :: (CStorable a) => a -> Int
cSizeOfPatch a = if remainder == 0 then sz else (res + 1) * al
  where
    al = cAlignment a
    sz = cSizeOf a
    (res, remainder) = sz `divMod` al

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisTetrionRecord" #-} TetrionRecordC = TetrionRecordC
  { c_r_simulation_step_index :: CU64,
    c_event :: EnumTypeC,
    c_r_tetrion_index :: CU8
  }
  deriving (Eq, Show, Generic)

data TetrionRecord = TetrionRecord
  { r_simulation_step_index :: Word64,
    event :: InputEvent,
    r_tetrion_index :: Word8
  }
  deriving (Eq, Show)

instance CStorable TetrionRecordC

instance Storable TetrionRecordC where
  sizeOf :: TetrionRecordC -> Int
  sizeOf = cSizeOfPatch
  alignment :: TetrionRecordC -> Int
  alignment = cAlignment
  poke :: Ptr TetrionRecordC -> TetrionRecordC -> IO ()
  poke = cPoke
  peek :: Ptr TetrionRecordC -> IO TetrionRecordC
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOpetrisMinoPosition" #-} MinoPositionC = MinoPositionC
  { c_x :: CU8,
    c_y :: CU8
  }
  deriving (Eq, Show, Generic)

data MinoPosition = MinoPosition
  { x :: Word8,
    y :: Word8
  }
  deriving (Eq, Show)

instance CStorable MinoPositionC

instance Storable MinoPositionC where
  sizeOf :: MinoPositionC -> Int
  sizeOf = cSizeOfPatch
  alignment :: MinoPositionC -> Int
  alignment = cAlignment
  poke :: Ptr MinoPositionC -> MinoPositionC -> IO ()
  poke = cPoke
  peek :: Ptr MinoPositionC -> IO MinoPositionC
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisMino" #-} MinoC = MinoC
  { c_position :: MinoPositionC,
    c_type :: EnumTypeC
  }
  deriving (Eq, Show, Generic)

data Mino = Mino
  { position :: MinoPosition,
    _type :: TetrominoType
  }
  deriving (Eq, Show)

instance CStorable MinoC

instance Storable MinoC where
  sizeOf :: MinoC -> Int
  sizeOf = cSizeOfPatch
  alignment :: MinoC -> Int
  alignment = cAlignment
  poke :: Ptr MinoC -> MinoC -> IO ()
  poke = cPoke
  peek :: Ptr MinoC -> IO MinoC
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOpetrisTetrionSnapshot" #-} TetrionSnapshotC = TetrionSnapshotC
  { c_level :: CU32,
    c_mino_stack :: Ptr MinoC,
    c_score :: CU64,
    c_s_simulation_step_index :: CU64,
    c_lines_cleared :: CU32,
    c_s_tetrion_index :: CU8
  }
  deriving (Eq, Show, Generic)

data TetrionSnapshot = TetrionSnapshot
  { level :: Word32,
    mino_stack :: [Mino],
    score :: Word64,
    s_simulation_step_index :: Word64,
    lines_cleared :: Word32,
    s_tetrion_index :: Word8
  }
  deriving (Eq, Show)

instance CStorable TetrionSnapshotC

instance Storable TetrionSnapshotC where
  sizeOf :: TetrionSnapshotC -> Int
  sizeOf = cSizeOfPatch
  alignment :: TetrionSnapshotC -> Int
  alignment = cAlignment
  poke :: Ptr TetrionSnapshotC -> TetrionSnapshotC -> IO ()
  poke = cPoke
  peek :: Ptr TetrionSnapshotC -> IO TetrionSnapshotC
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisTetrionHeader" #-} TetrionHeaderC = TetrionHeaderC
  { c_seed :: CU64,
    c_starting_level :: CU32
  }
  deriving (Eq, Show, Generic)

data TetrionHeader = TetrionHeader
  { seed :: Word64,
    starting_level :: Word32
  }
  deriving (Eq, Show)

instance CStorable TetrionHeaderC

instance Storable TetrionHeaderC where
  sizeOf :: TetrionHeaderC -> Int
  sizeOf = cSizeOfPatch
  alignment :: TetrionHeaderC -> Int
  alignment = cAlignment
  poke :: Ptr TetrionHeaderC -> TetrionHeaderC -> IO ()
  poke = cPoke
  peek :: Ptr TetrionHeaderC -> IO TetrionHeaderC
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisRecordingInformation" #-} RecordingInformationC = RecordingInformationC
  { c_information :: Ptr AdditionalInformationC,
    c_records :: Ptr TetrionRecordC,
    c_snapshots :: Ptr TetrionSnapshotC,
    c_tetrion_headers :: Ptr TetrionHeaderC,
    c_version :: CU32
  }
  deriving (Eq, Show, Generic)

data RecordingInformation = RecordingInformation
  { information :: AdditionalInformation,
    records :: [TetrionRecord],
    snapshots :: [TetrionSnapshot],
    tetrion_headers :: [TetrionHeader],
    version :: Word32
  }
  deriving (Eq, Show)

instance CStorable RecordingInformationC

instance Storable RecordingInformationC where
  sizeOf :: RecordingInformationC -> Int
  sizeOf = cSizeOfPatch
  alignment :: RecordingInformationC -> Int
  alignment = cAlignment
  poke :: Ptr RecordingInformationC -> RecordingInformationC -> IO ()
  poke = cPoke
  peek :: Ptr RecordingInformationC -> IO RecordingInformationC
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisGridProperties" #-} GridPropertiesC = GridPropertiesC
  { c_height :: CU32,
    c_width :: CU32
  }
  deriving (Eq, Show, Generic)

data GridProperties = GridProperties
  { height :: Word32,
    width :: Word32
  }
  deriving (Eq, Show)

instance CStorable GridPropertiesC

instance Storable GridPropertiesC where
  sizeOf :: GridPropertiesC -> Int
  sizeOf = cSizeOfPatch
  alignment :: GridPropertiesC -> Int
  alignment = cAlignment
  poke :: Ptr GridPropertiesC -> GridPropertiesC -> IO ()
  poke = cPoke
  peek :: Ptr GridPropertiesC -> IO GridPropertiesC
  peek = cPeek
