{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Types (
    Card(..)
  , TopLeftSlot(..)
  , HuaSlot(..)
  , TopRightSlot(..)
  , Board(..)
  , Position(..)
  , topleftL
  , pileL
  , toprightL
  , huaslotL
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
    deriving (Show, Eq, Ord)

data TopLeftSlot where
    TLEmpty :: TopLeftSlot
    TLSingleton :: Card -> TopLeftSlot
    TLFull :: TopLeftSlot
    deriving (Show, Eq, Ord)

data HuaSlot = HSEmpty | HSSingleton
    deriving (Show, Eq, Ord)

data TopRightSlot where
    TREmpty :: TopRightSlot
    TRTaken :: Card -> TopRightSlot
    deriving (Show, Eq, Ord)

data Board = Board {
    topleft :: (TopLeftSlot, TopLeftSlot, TopLeftSlot)
  , huaslot :: HuaSlot
  , topright :: (TopRightSlot, TopRightSlot, TopRightSlot)
  , pile :: [[Card]]
  } deriving (Show, Eq, Ord)

makeLensesFor [("topleft", "topleftL")
               ,("huaslot", "huaslotL")
               ,("topright", "toprightL")
               ,("pile", "pileL")] ''Board

-- move position (from/to)
data Position where
    PTL :: Int -> Position -- top left
    PMain :: Int -> Int -> Position -- main stack: # stack, order
    PHua :: Position -- hua position
    PTR :: Int -> Position -- top right
    deriving Show
