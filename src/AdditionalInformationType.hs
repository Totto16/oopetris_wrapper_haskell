{-# LANGUAGE InstanceSigs #-}

module AdditionalInformationType (AdditionalInformationType (..)) where

import Foreign (Int32)
import Foreign.CStorable (CStorable (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

data AdditionalInformationType
  = String
  | Float
  | Double
  | Bool
  | U8
  | I8
  | U32
  | I32
  | U64
  | I64
  | Vector
  deriving (Eq, Show)

instance Enum AdditionalInformationType where
  fromEnum :: AdditionalInformationType -> Int
  fromEnum String = 0
  fromEnum Float = 1
  fromEnum Double = 2
  fromEnum Bool = 3
  fromEnum U8 = 4
  fromEnum I8 = 5
  fromEnum U32 = 6
  fromEnum I32 = 7
  fromEnum U64 = 8
  fromEnum I64 = 9
  fromEnum Vector = 10

  toEnum :: Int -> AdditionalInformationType
  toEnum 0 = String
  toEnum 1 = Float
  toEnum 2 = Double
  toEnum 3 = Bool
  toEnum 4 = U8
  toEnum 5 = I8
  toEnum 6 = U32
  toEnum 7 = I32
  toEnum 8 = U64
  toEnum 9 = I64
  toEnum 10 = Vector
  toEnum unmatched = error ("AdditionalInformationType.toEnum: Cannot match " ++ show unmatched)

instance Storable AdditionalInformationType where
  sizeOf :: AdditionalInformationType -> Int
  sizeOf _ = sizeOf (undefined :: Int32)

  alignment :: AdditionalInformationType -> Int
  alignment _ = alignment (undefined :: Int32)

  peek :: Ptr AdditionalInformationType -> IO AdditionalInformationType
  peek ptr = do
    value <- peek (castPtr ptr :: Ptr Int32)
    return . toEnum $ fromIntegral value

  poke :: Ptr AdditionalInformationType -> AdditionalInformationType -> IO ()
  poke loc val = do
    let value = Prelude.toEnum $ fromEnum val :: Int32
    poke (castPtr loc :: Ptr Int32) value

instance CStorable AdditionalInformationType where
  cSizeOf :: AdditionalInformationType -> Int
  cSizeOf = sizeOf
  cAlignment :: AdditionalInformationType -> Int
  cAlignment = alignment
  cPoke :: Ptr AdditionalInformationType -> AdditionalInformationType -> IO ()
  cPoke = poke
  cPeek :: Ptr AdditionalInformationType -> IO AdditionalInformationType
  cPeek = peek
