{-# LANGUAGE GADTs #-}
module Types
    ( Card
    , TopLeftSlot
    , HuaSlot
    , TopRightSlot
    , MainSlot
    ) where
import Control.Lens

data Card where
    Zhong :: Card
    Fa :: Card
    Bai :: Card
    Hua :: Card
    Wan :: Int -> Card
    Tong :: Int -> Card
    Tiao :: Int -> Card
    deriving (Show, Eq)

data TopLeftSlot where
    TLEmpty :: TopLeftSlot
    TLSingleton :: Card -> TopLeftSlot
    TLFull :: Card -> TopLeftSlot

data HuaSlot = HSEmpty | HSSingleton

data TopRightSlot where
    TREmpty :: TopRightSlot
    TRTaken :: Card -> TopRightSlot

data MainSlot where
    MSEmpty :: MainSlot
    MSPile :: [Card] -> MainSlot

data Board = Board {
    topleft :: (TopLeftSlot, TopLeftSlot, TopLeftSlot)
  , huaslot :: HuaSlot
  , topright :: (TopRightSlot, TopRightSlot, TopRightSlot)
  , pile :: [[Card]]
  }
