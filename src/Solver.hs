{-# LANGUAGE TupleSections #-}
module Solver where

import Types
import Parser

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Set as S
import Data.Maybe
import Data.List (foldl')

isFinished :: Board -> Bool
isFinished (Board tl h (tr1, tr2, tr3) ms) = tl == (TLFull, TLFull, TLFull) &&
    h == HSSingleton && S.fromList [tr1, tr2, tr3] == fullset &&
    ms == replicate 8 [] where
        fullset = S.fromList [TRTaken (Wan 9), TRTaken (Tong 9), TRTaken (Tiao 9)]

data Operation = Move Position Position | Hit Card
    deriving Show

allPossibleMoves :: Board -> [(Board, Operation)]
allPossibleMoves bd =
    let am = allMoveable bd
        moveOps = concatMap (\s@(p, i) -> let pos = reachableFrom bd s
                                in map (modifyBoard bd p &&& Move p) pos) am
        hitOps = canHit bd
    in hitOps ++ moveOps

canHit :: Board -> [(Board, Operation)]
canHit bd
    | length (searchSurface bd Zhong) == 4 &&
        (any ((== Zhong) . snd) (getTopLeft bd) || length (getTopLeft bd) < 3) =
        [(hitOpt bd (searchSurface bd Zhong), Hit Zhong)]
    | length (searchSurface bd Fa) == 4 &&
        (any ((== Fa) . snd) (getTopLeft bd) || length (getTopLeft bd) < 3) =
        [(hitOpt bd (searchSurface bd Fa), Hit Fa)]
    | length (searchSurface bd Bai) == 4 &&
        (any ((== Bai) . snd) (getTopLeft bd) || length (getTopLeft bd) < 3) =
        [(hitOpt bd (searchSurface bd Bai), Hit Bai)]
    | otherwise = []


getTopLeft :: Board -> [(Int, Card)]
getTopLeft bd =
    let (x, y, z) = topleft bd
        res = [x, y, z]
        f (TLSingleton _) = True
        f _ = False
        fromsingle (TLSingleton c) = c
    in map (second fromsingle) $ filter (f . snd) $ zip [0..] res

-- partial function
delete :: Board -> Position -> Board
delete bd (PTL i) = bd & topleftL . ix i .~ TLEmpty
delete bd (PMain i j) = bd & pileL . ix i .~ init (bd ^. pileL . ix i)

hitOpt :: Board -> [Position] -> Board
hitOpt bd pos =
    let bd' = foldl' delete bd pos
        topl = getTopLeft bd'
    in if length topl == 3
         then error "impossible"
         else let PTL i = firstEmptyTL bd'
                in bd' & topleftL . ix i .~ TLFull

searchSurface :: Board -> Card -> [Position]
searchSurface bd c =
    let tlpos = map (PTL . fst) $ filter ((== c) . snd) $ getTopLeft bd
        mainpos = map (\(cs, p) -> PMain p (length cs - 1)) $
            filter (\(cs, p) -> not (null cs) && last cs == c) $ zip (pile bd) [0..]
    in tlpos ++ mainpos

-- (start position, # cards will be moved)
allMoveable :: Board -> [(Position, Int)]
allMoveable b = moveableTL ++ moveableMain b where
    moveableTL = map ((, 1) . PTL . snd) $ filter fst $
                    zip (map tlMoveable (topleft' b)) [0..]
    topleft' b' = let (a, b, c) = topleft b' in [a, b, c]
    tlMoveable (TLSingleton _) = True
    tlMoveable _ = False

moveableMain :: Board -> [(Position, Int)]
moveableMain (Board _ _ _ mainstack) = concatMap helper $ zip mainstack [0..]
    where
        helper (cards, i) = map (first $ PMain i) $ moveableM cards
        moveableM :: [Card] -> [(Int, Int)]
        moveableM cs = if null cs then [] else
            (length cs - 1, 1) : remains cs where
            remains cs = let l = length $ filter canCombine $
                                    zip (reverse cs) (tail $ reverse cs)
                        in zip [length cs - 2, length cs - 3..] [2..l + 1]
            canCombine (c1, c2) = not (isSameType c1 c2) &&
                cardNumber c1 + 1 == cardNumber c2

isSameType :: Card -> Card -> Bool
isSameType (Wan _) (Wan _) = True
isSameType (Tiao _) (Tiao _) = True
isSameType (Tong _) (Tong _) = True
isSameType _ _ = False

cardNumber :: Card -> Int
cardNumber (Wan n) = n
cardNumber (Tiao n) = n
cardNumber (Tong n) = n
cardNumber _ = -1

reachableFrom :: Board -> (Position, Int) -> [Position]
reachableFrom b (p, i) =
    let c = getCardFromPos b p
    in case c of
        Hua -> [PHua] -- if Hua is moveable, it must be on the surface and should
                      -- only be moved to Hua slot
        Zhong -> moveableZFB b
        Fa -> moveableZFB b
        Bai -> moveableZFB b
        -- normal card
        nc -> let emptySlots = moveableZFB b
                  validEmpty = if i == 1
                                 then emptySlots
                                 else filter (not . isPTL) emptySlots
                  validTopRight
                    | i /= 1 = []
                    | nc == Wan 1 || nc == Tong 1 || nc == Tiao 1 = [firstEmptyTR b]
                    | otherwise =
                        let (tr1', tr2', tr3') = topright b
                            trlist = map (first $ fromJust . topRightConv) $
                                filter (isJust . topRightConv. fst) $
                                zip [tr1', tr2', tr3'] [0..]
                        in map (PTR . snd) $ filter (\(t, po) -> isSameType t c &&
                                    cardNumber t + 1 == cardNumber c) trlist
                  validMainS = filter (\(cards, (n, la)) ->
                        null cards ||
                            let f = last cards
                            in not (isSameType c f) &&
                                cardNumber c + 1 == cardNumber f) $
                                zip (pile b) $ zip [0..] (map length $ pile b)
                  validMain = map (\(_, (n, la)) -> PMain n (la - 1)) validMainS
              in validEmpty ++ validTopRight ++ validMain

firstEmptyTL :: Board -> Position
firstEmptyTL bd = let (a, b, c) = topleft bd
    in if a == TLEmpty
         then PTL 0
         else if b == TLEmpty
                then PTL 1
                else if c == TLEmpty
                       then PTL 2
                       else error "impossible"

firstEmptyTR :: Board -> Position
firstEmptyTR bd = let (a, b, c) = topright bd
    in if a == TREmpty
         then PTR 0
         else if b == TREmpty
                then PTR 1
                else if c == TREmpty
                       then PTR 2
                       else error "impossible"

isPTL :: Position -> Bool
isPTL (PTL _) = True
isPTL _ = False

topRightConv :: TopRightSlot -> Maybe Card
topRightConv TREmpty = Nothing
topRightConv (TRTaken c) = Just c

moveableZFB :: Board -> [Position]
moveableZFB bd =
    let (a, b, c) = topleft bd
        part1 = map (PTL . snd) $ filter ((== TLEmpty) . fst) $ zip [a, b, c] [0..]
        mainstack = pile bd
        part2 = map (flip PMain 0 . snd) $ filter (null . fst) $ zip mainstack [0..]
    in part1 ++ part2

-- partial function, only handle positions returned by `allMoveable`
getCardFromPos :: Board -> Position -> Card
getCardFromPos bd (PTL i) = let (a, b, c) = topleft bd
    in case [a, b, c] !! i of
        TLSingleton c -> c
        _ -> error "impossible"
getCardFromPos bd (PMain i j) = pile bd !! i !! j

modifyBoard :: Board -> Position -> Position -> Board
modifyBoard bd (PTL i) (PTL j) = bd -- useless move, so we don't move
modifyBoard bd (PTL i) (PMain x y) = bd & topleftL . ix i .~ TLEmpty
    & pileL . ix x .~ (bd ^. pileL . ix x ++ [getCardFromPos bd (PTL i)])
modifyBoard bd (PTL i) PHua = error "impossible"
modifyBoard bd (PTL i) (PTR j) = bd & topleftL . ix i .~ TLEmpty
    & toprightL . ix j .~ TRTaken (getCardFromPos bd (PTL i))
modifyBoard bd pm@(PMain x y) (PTL i) =
    let c = getCardFromPos bd pm
        ori = bd ^. pileL . ix x
    in bd & topleftL . ix i .~ TLSingleton c
          & pileL . ix x .~ init ori
modifyBoard bd (PMain x y) PHua =
    let ori = bd ^. pileL . ix x
    in bd & huaslotL .~ HSSingleton
          & pileL . ix x .~ init ori
modifyBoard bd pm@(PMain x y) (PTR i) =
    let c = getCardFromPos bd pm
        ori = bd ^. pileL . ix x
    in bd & toprightL . ix i .~ TRTaken c
          & pileL . ix x .~ init ori
modifyBoard bd (PMain a b) (PMain x y) =
    let ori = bd ^. pileL . ix a
        ori2 = bd ^. pileL . ix x
    in bd & pileL . ix a .~ take b ori
          & pileL . ix x .~ ori2 ++ drop b ori

try :: Board -> State (S.Set Board) Bool
try bd =
    if isFinished bd
      then return True
      else do
        vis <- get
        let bds = map fst $ allPossibleMoves bd
            bds' = filter (not . (`S.member` vis)) bds
        if null bds'
          then return False
          else do
            let vis' = S.union vis $ S.fromList bds
            put vis'
            a <- mapM try bds'
            return $ or a

finished :: Board
finished = Board (TLEmpty, TLEmpty, TLEmpty) HSSingleton (TREmpty, TREmpty, TREmpty)
    [[Wan 9, Wan 8, Wan 7, Wan 6, Wan 5, Wan 4, Wan 3, Wan 2, Wan 1],
    [Tiao 9, Tiao 8, Tiao 7, Tiao 6, Tiao 5, Tiao 4, Tiao 3, Tiao 2, Tiao 1],
    [Tong 9, Tong 8, Tong 7, Tong 6, Tong 5, Tong 4, Tong 3, Tong 2, Tong 1],
    [Zhong, Fa, Bai], [Zhong, Fa, Bai], [Zhong, Fa, Bai], [Zhong, Fa, Bai], []]
