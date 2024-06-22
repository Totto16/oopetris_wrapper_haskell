{-# LANGUAGE InstanceSigs #-}

module TetrominoType (TetrominoType (..)) where

import Foreign (Int32)
import Foreign.CStorable (CStorable (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

data TetrominoType
  = I
  | J
  | L
  | O
  | S
  | T
  | Z
  deriving (Eq, Show)

instance Enum TetrominoType where
  fromEnum :: TetrominoType -> Int
  fromEnum I = 0
  fromEnum J = 1
  fromEnum L = 2
  fromEnum O = 3
  fromEnum S = 4
  fromEnum T = 5
  fromEnum Z = 6

  toEnum :: Int -> TetrominoType
  toEnum 0 = I
  toEnum 1 = J
  toEnum 2 = L
  toEnum 3 = O
  toEnum 4 = S
  toEnum 5 = T
  toEnum 6 = Z
  toEnum unmatched = error ("TetrominoType.toEnum: Cannot match " ++ show unmatched)

instance Storable TetrominoType where
  sizeOf :: TetrominoType -> Int
  sizeOf _ = sizeOf (undefined :: Int32)

  alignment :: TetrominoType -> Int
  alignment _ = alignment (undefined :: Int32)

  peek :: Ptr TetrominoType -> IO TetrominoType
  peek ptr = do
    value <- peek (castPtr ptr :: Ptr Int32)
    return . toEnum $ fromIntegral value

  poke :: Ptr TetrominoType -> TetrominoType -> IO ()
  poke loc val = do
    let value = Prelude.toEnum $ fromEnum val :: Int32
    poke (castPtr loc :: Ptr Int32) value

instance CStorable TetrominoType where
  cSizeOf :: TetrominoType -> Int
  cSizeOf = sizeOf
  cAlignment :: TetrominoType -> Int
  cAlignment = alignment
  cPoke :: Ptr TetrominoType -> TetrominoType -> IO ()
  cPoke = poke
  cPeek :: Ptr TetrominoType -> IO TetrominoType
  cPeek = peek
