{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Structs (TetrionRecord (..), MinoPosition (..), Mino (..), TetrionSnapshot (..), TetrionHeader (..), RecordingInformation (..), GridProperties (..)) where

import Foreign.CStorable
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import InputEvent (InputEvent)
import StdInt (CU32, CU64, CU8)
import TetrominoType (TetrominoType)
import Types (AdditionalInformation)

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisTetrionRecord" #-} TetrionRecord = TetrionRecord
  { r_simulation_step_index :: CU64,
    event :: InputEvent,
    r_tetrion_index :: CU8
  }
  deriving (Eq, Show, Generic)

instance CStorable TetrionRecord

instance Storable TetrionRecord where
  sizeOf :: TetrionRecord -> Int
  sizeOf = cSizeOf
  alignment :: TetrionRecord -> Int
  alignment = cAlignment
  poke :: Ptr TetrionRecord -> TetrionRecord -> IO ()
  poke = cPoke
  peek :: Ptr TetrionRecord -> IO TetrionRecord
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOpetrisMinoPosition" #-} MinoPosition = MinoPosition
  { x :: CU8,
    y :: CU8
  }
  deriving (Eq, Show, Generic)

instance CStorable MinoPosition

instance Storable MinoPosition where
  sizeOf :: MinoPosition -> Int
  sizeOf = cSizeOf
  alignment :: MinoPosition -> Int
  alignment = cAlignment
  poke :: Ptr MinoPosition -> MinoPosition -> IO ()
  poke = cPoke
  peek :: Ptr MinoPosition -> IO MinoPosition
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisMino" #-} Mino = Mino
  { position :: MinoPosition,
    _type :: TetrominoType
  }
  deriving (Eq, Show, Generic)

instance CStorable Mino

instance Storable Mino where
  sizeOf :: Mino -> Int
  sizeOf = cSizeOf
  alignment :: Mino -> Int
  alignment = cAlignment
  poke :: Ptr Mino -> Mino -> IO ()
  poke = cPoke
  peek :: Ptr Mino -> IO Mino
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOpetrisTetrionSnapshot" #-} TetrionSnapshot = TetrionSnapshot
  { level :: CU32,
    mino_stack :: Ptr Mino,
    score :: CU64,
    s_simulation_step_index :: CU64,
    lines_cleared :: CU32,
    s_tetrion_index :: CU8
  }
  deriving (Eq, Show, Generic)

instance CStorable TetrionSnapshot

instance Storable TetrionSnapshot where
  sizeOf :: TetrionSnapshot -> Int
  sizeOf = cSizeOf
  alignment :: TetrionSnapshot -> Int
  alignment = cAlignment
  poke :: Ptr TetrionSnapshot -> TetrionSnapshot -> IO ()
  poke = cPoke
  peek :: Ptr TetrionSnapshot -> IO TetrionSnapshot
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisTetrionHeader" #-} TetrionHeader = TetrionHeader
  { seed :: CU64,
    starting_level :: CU32
  }
  deriving (Eq, Show, Generic)

instance CStorable TetrionHeader

instance Storable TetrionHeader where
  sizeOf :: TetrionHeader -> Int
  sizeOf = cSizeOf
  alignment :: TetrionHeader -> Int
  alignment = cAlignment
  poke :: Ptr TetrionHeader -> TetrionHeader -> IO ()
  poke = cPoke
  peek :: Ptr TetrionHeader -> IO TetrionHeader
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisRecordingInformation" #-} RecordingInformation = RecordingInformation
  { information :: Ptr AdditionalInformation,
    records :: Ptr TetrionRecord,
    snapshots :: Ptr TetrionSnapshot,
    tetrion_headers :: Ptr TetrionHeader,
    version :: CU32
  }
  deriving (Eq, Show, Generic)

instance CStorable RecordingInformation

instance Storable RecordingInformation where
  sizeOf :: RecordingInformation -> Int
  sizeOf = cSizeOf
  alignment :: RecordingInformation -> Int
  alignment = cAlignment
  poke :: Ptr RecordingInformation -> RecordingInformation -> IO ()
  poke = cPoke
  peek :: Ptr RecordingInformation -> IO RecordingInformation
  peek = cPeek

data {-# CTYPE "oopetris/oopetris_wrapper.h" "OOPetrisGridProperties" #-} GridProperties = GridProperties
  { height :: CU32,
    width :: CU32
  }
  deriving (Eq, Show, Generic)

instance CStorable GridProperties

instance Storable GridProperties where
  sizeOf :: GridProperties -> Int
  sizeOf = cSizeOf
  alignment :: GridProperties -> Int
  alignment = cAlignment
  poke :: Ptr GridProperties -> GridProperties -> IO ()
  poke = cPoke
  peek :: Ptr GridProperties -> IO GridProperties
  peek = cPeek
