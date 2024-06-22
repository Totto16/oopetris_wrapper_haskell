{-# LANGUAGE InstanceSigs #-}

module InputEvent (InputEvent (..)) where

import Foreign (Int32)
import Foreign.CStorable (CStorable (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

data InputEvent
  = RotateLeftPressed
  | RotateRightPressed
  | MoveLeftPressed
  | MoveRightPressed
  | MoveDownPressed
  | DropPressed
  | HoldPressed
  | RotateLeftReleased
  | RotateRightReleased
  | MoveLeftReleased
  | MoveRightReleased
  | MoveDownReleased
  | DropReleased
  | HoldReleased
  deriving (Eq, Show)

instance Enum InputEvent where
  fromEnum :: InputEvent -> Int
  fromEnum RotateLeftPressed = 0
  fromEnum RotateRightPressed = 1
  fromEnum MoveLeftPressed = 2
  fromEnum MoveRightPressed = 3
  fromEnum MoveDownPressed = 4
  fromEnum DropPressed = 5
  fromEnum HoldPressed = 6
  fromEnum RotateLeftReleased = 7
  fromEnum RotateRightReleased = 8
  fromEnum MoveLeftReleased = 9
  fromEnum MoveRightReleased = 10
  fromEnum MoveDownReleased = 11
  fromEnum DropReleased = 12
  fromEnum HoldReleased = 13
  toEnum :: Int -> InputEvent
  toEnum 0 = RotateLeftPressed
  toEnum 1 = RotateRightPressed
  toEnum 2 = MoveLeftPressed
  toEnum 3 = MoveRightPressed
  toEnum 4 = MoveDownPressed
  toEnum 5 = DropPressed
  toEnum 6 = HoldPressed
  toEnum 7 = RotateLeftReleased
  toEnum 8 = RotateRightReleased
  toEnum 9 = MoveLeftReleased
  toEnum 10 = MoveRightReleased
  toEnum 11 = MoveDownReleased
  toEnum 12 = DropReleased
  toEnum 13 = HoldReleased
  toEnum unmatched = error ("InputEvent.toEnum: Cannot match " ++ show unmatched)

instance Storable InputEvent where
  sizeOf :: InputEvent -> Int
  sizeOf _ = sizeOf (undefined :: Int32)

  alignment :: InputEvent -> Int
  alignment _ = alignment (undefined :: Int32)

  peek :: Ptr InputEvent -> IO InputEvent
  peek ptr = do
    value <- peek (castPtr ptr :: Ptr Int32)
    return . toEnum $ fromIntegral value

  poke :: Ptr InputEvent -> InputEvent -> IO ()
  poke loc val = do
    let value = Prelude.toEnum $ fromEnum val :: Int32
    poke (castPtr loc :: Ptr Int32) value

instance CStorable InputEvent where
  cSizeOf :: InputEvent -> Int
  cSizeOf = sizeOf
  cAlignment :: InputEvent -> Int
  cAlignment = alignment
  cPoke :: Ptr InputEvent -> InputEvent -> IO ()
  cPoke = poke
  cPeek :: Ptr InputEvent -> IO InputEvent
  cPeek = peek
