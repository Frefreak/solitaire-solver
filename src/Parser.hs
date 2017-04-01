{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Parser (
    getBoard
  ) where

import Types

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Network.HTTP.Client
import Data.Aeson.Lens
import Control.Lens.Operators
import Control.Exception (throw, evaluate)

getBoardJson :: IO LBS.ByteString
getBoardJson = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest "http://127.0.0.1:4000"
    response <- httpLbs request manager
    return $ responseBody response

parse :: LBS.ByteString -> Board
parse lbs =
    let toprow = lbs ^.. key "top" . _Array . traverse . _String
        topleft = topleftH $ take 3 toprow
        topleftH ts = let [a, b, c] = map topleftConv ts in (a, b, c)
        hua = huaConv $ toprow !! 3
        topright = toprightH $ drop 4 toprow
        toprightH ts = let [a, b, c] = map toprightConv ts in (a, b, c)
        pileConv n =
            let k = T.pack $ "stack" ++ show n
                stackstr = lbs ^.. key k . _Array . traverse . _String
            in if length stackstr == 1 && head stackstr == "empty"
                 then []
                 else map parseCard stackstr
        mainstack = map pileConv [0..7]
    in Board topleft hua topright mainstack

topleftConv :: T.Text -> TopLeftSlot
topleftConv "empty" = TLEmpty
topleftConv "back" = TLFull
topleftConv t = TLSingleton $ parseCard t

huaConv :: T.Text -> HuaSlot
huaConv "empty" = HSEmpty
huaConv "hua" = HSSingleton
huaConv _ = throw $ InvalidBoard "server returned impossible value"

toprightConv :: T.Text -> TopRightSlot
toprightConv "empty" = TREmpty
toprightConv t = TRTaken $ parseCard t

parseCard :: T.Text -> Card
parseCard "zhong" = Zhong
parseCard "fa" = Fa
parseCard "bai" = Bai
parseCard "hua" = Hua
parseCard s = helper (T.tail s) (read [T.head s]) where
    helper "wan" = Wan
    helper "tong" = Tong
    helper "tiao" = Tiao
    helper _ = throw $ InvalidBoard "server returned impossible value"

getBoard :: IO Board
getBoard = (parse <$> getBoardJson) >>= evaluate
