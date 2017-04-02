{-# LANGUAGE TupleSections #-}
module Solver where

import Types
import Parser
import Pretty

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.HashSet as S
import qualified Data.Sequence as Sq
import Data.Maybe
import Data.List (foldl', transpose, sortBy)
import Data.Function (on)

isFinished :: Board -> Bool
isFinished (Board tl h (tr1, tr2, tr3) ms) = tl == (TLFull, TLFull, TLFull) &&
    h == HSSingleton && S.fromList [tr1, tr2, tr3] == fullset &&
    ms == replicate 8 [] where
        fullset = S.fromList [TRTaken (Wan 9), TRTaken (Tong 9), TRTaken (Tiao 9)]

allPossibleMoves :: Board -> [(Board, Operation)]
allPossibleMoves bd =
    let slayOps = slayMoves bd
    in if not (null slayOps)
         then slayOps
         else
            let am = allMoveable bd
            in sortBy sortOp $ concatMap (\s@(p, i) -> let pos = reachableFrom bd s
                        in map (makeMove bd p &&& Move p) pos) am

rankOp :: Operation -> Int
rankOp (Move _ (PTL _)) = 3
rankOp (Move (PMain _ _) (PMain _ _)) = 2
rankOp (Move (PTL _) (PMain _ _)) = 1
rankOp (Move _ (PTR _)) = 0
rankOp _ = -1

minimum' :: [Card] -> Card
minimum' [] = Wan 5 -- priority, empirical value
minimum' cs = minimum cs

sortOp :: (Board, Operation) -> (Board, Operation) -> Ordering
sortOp (_, Slay _) _ = error "impossible"
sortOp _ (_, Slay _) = error "impossible"
sortOp (b1, op1@(Move (PMain x _) _)) (b2, op2@(Move (PMain y _) _)) =
    if rankOp op1 == rankOp op2
      then (compare `on` minimum')
        (b1 ^. pileL . ix x) (b2 ^. pileL . ix y)
      else (compare `on` rankOp) op1 op2
sortOp (_, op1) (_, op2) = (compare `on` rankOp) op1 op2

slayMoves :: Board -> [(Board, Operation)]
slayMoves bd
    | length (searchSurface bd Zhong) == 4 &&
        (any ((== Zhong) . snd) (getTopLeft bd) || tlNonEmpty bd < 3) =
        [(slayOp bd (searchSurface bd Zhong), Slay Zhong)]
    | length (searchSurface bd Fa) == 4 &&
        (any ((== Fa) . snd) (getTopLeft bd) || tlNonEmpty bd < 3) =
        [(slayOp bd (searchSurface bd Fa), Slay Fa)]
    | length (searchSurface bd Bai) == 4 &&
        (any ((== Bai) . snd) (getTopLeft bd) || tlNonEmpty bd < 3) =
        [(slayOp bd (searchSurface bd Bai), Slay Bai)]
    | otherwise = []

tlNonEmpty :: Board -> Int
tlNonEmpty bd =
    let (x, y, z) = topleft bd
        res = [x, y, z]
        f TLEmpty = False
        f _ = True
        fromsingle (TLSingleton c) = c
    in length $ filter f res

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

slayOp :: Board -> [Position] -> Board
slayOp bd pos =
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
    topleft' b' = let (x, y, z) = topleft b' in [x, y, z]
    tlMoveable (TLSingleton _) = True
    tlMoveable _ = False

moveableMain :: Board -> [(Position, Int)]
moveableMain (Board _ _ _ mainstack) = concatMap helper $ zip mainstack [0..]
    where helper (cards, i) = map (first $ PMain i) $ moveableM cards

moveableM :: [Card] -> [(Int, Int)]
moveableM cs = if null cs then [] else
    (length cs - 1, 1) : remains cs where
    remains cs = let l = length $ helper $ zip (reverse cs) (tail $ reverse cs)
                in zip [length cs - 2, length cs - 3..] [2..l + 1]
    helper [] = []
    helper (t:ts) = if canCombine t then t: helper ts else []
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
        {- Zhong -> allEmptySlots b -}
        {- Fa -> allEmptySlots b -}
        {- Bai -> allEmptySlots b -}
        -- normal card
        nc -> let validToTopRight
                    | i /= 1 = []
                    | nc == Wan 1 || nc == Tong 1 || nc == Tiao 1 = [firstEmptyTR b]
                    | otherwise =
                        let (tr1', tr2', tr3') = topright b
                            trlist = map (first $ fromJust . topRightConv) $
                                filter (isJust . topRightConv. fst) $
                                zip [tr1', tr2', tr3'] [0..]
                        in map (PTR . snd) $ filter (\(t, po) -> isSameType t c &&
                                    cardNumber t + 1 == cardNumber c) trlist
                  (tlempty, mainempty) = allEmptySlots b
                  tlempty' = if null tlempty then [] else [head tlempty]
                  mainempty' = if null mainempty then [] else [head mainempty]
                  validToEmpty | i == 1 =
                                if positionOnTop b p
                                    then if isPTL p then mainempty' else tlempty'
                                    else mainempty' ++ tlempty'
                               | otherwise = if positionOnTop b p
                                                then []
                                                else mainempty'
                  validToMainS = filter (\(cards, (n, la)) ->
                        not (null cards) && -- exclude empty main pile
                            let f = last cards
                            in not (isSameType c f) &&
                                cardNumber c + 1 == cardNumber f) $
                                zip (pile b) $ zip [0..] (map length $ pile b)
                  validToMain = map (uncurry PMain . snd) validToMainS
              in if null validToTopRight
                   then validToMain ++ validToEmpty
                   else [head validToTopRight]

positionOnTop :: Board -> Position -> Bool
positionOnTop _ (PTL _) = True
positionOnTop _ (PMain _ j) = j == 0

firstEmptyTL :: Board -> Position
firstEmptyTL bd = let (a, b, c) = topleft bd
    in if a == TLEmpty
         then PTL 0
         else if b == TLEmpty
                then PTL 1
                else if c == TLEmpty
                       then PTL 2
                       else error $ show bd -- "impossible"

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

-- actually, top right and main
allEmptySlots :: Board -> ([Position], [Position])
allEmptySlots bd =
    let (a, b, c) = topleft bd
        part1 = map (PTL . snd) $ filter ((== TLEmpty) . fst) $ zip [a, b, c] [0..]
        mainstack = pile bd
        part2 = map (flip PMain 0 . snd) $ filter (null . fst) $ zip mainstack [0..]
    in (part1, part2)

-- partial function, only handle positions returned by `allMoveable`
getCardFromPos :: Board -> Position -> Card
getCardFromPos bd (PTL i) = let (a, b, c) = topleft bd
    in case [a, b, c] !! i of
        TLSingleton c -> c
        _ -> error "impossible"
getCardFromPos bd (PMain i j) = pile bd !! i !! j

makeMove :: Board -> Position -> Position -> Board
makeMove bd (PTL i) (PTL j) = bd & topleftL . ix i .~ TLEmpty
    & topleftL . ix j .~ TLSingleton (getCardFromPos bd (PTL i))
makeMove bd (PTL i) (PMain x y) = bd & topleftL . ix i .~ TLEmpty
    & pileL . ix x .~ (bd ^. pileL . ix x ++ [getCardFromPos bd (PTL i)])
makeMove bd (PTL i) PHua = error "impossible"
makeMove bd (PTL i) (PTR j) = bd & topleftL . ix i .~ TLEmpty
    & toprightL . ix j .~ TRTaken (getCardFromPos bd (PTL i))
makeMove bd pm@(PMain x y) (PTL i) =
    let c = getCardFromPos bd pm
        ori = bd ^. pileL . ix x
    in bd & topleftL . ix i .~ TLSingleton c
          & pileL . ix x .~ init ori
makeMove bd (PMain x y) PHua =
    let ori = bd ^. pileL . ix x
    in bd & huaslotL .~ HSSingleton
          & pileL . ix x .~ init ori
makeMove bd pm@(PMain x y) (PTR i) =
    let c = getCardFromPos bd pm
        ori = bd ^. pileL . ix x
    in bd & toprightL . ix i .~ TRTaken c
          & pileL . ix x .~ init ori
makeMove bd (PMain a b) (PMain x y) =
    let ori = bd ^. pileL . ix a
        ori2 = bd ^. pileL . ix x
    in bd & pileL . ix a .~ take b ori
          & pileL . ix x .~ ori2 ++ drop b ori

modifyBoard :: Board -> Operation -> Board
modifyBoard b (Move p1 p2) = makeMove b p1 p2
modifyBoard b (Slay c) = let poss = searchSurface b c
    in slayOp b poss

dfs :: Board -> [Operation]
dfs b = simplify b . fst $ evalState (dfs' b) S.empty

dfs' :: Board -> State (S.HashSet Board) ([Operation], Bool)
dfs' bd = do
    vis <- get
    if isFinished bd
      then return ([], True)
      else do
        let allboards = allPossibleMoves bd
            newboards = filter (not . (`S.member` vis) . fst) allboards
        modify $ S.insert bd
        if null newboards
          then return ([], False)
          else do
            r <- mapM (\(b, op) -> (, op) <$> dfs' b) newboards 
            let goodOp = filter (snd . fst) r
            if null goodOp
              then return ([], False)
              else let ((a, b), c) = head goodOp in return (c:a, True)

-- debug only
dfs_ :: S.HashSet Board -> Board -> IO Bool
dfs_ vis bd = do
    putStrLn $ pretty bd
    if isFinished bd
      then putStrLn "Finished!!!" >> return True
      else do
        let allboards = allPossibleMoves bd
            newboards = filter (not . (`S.member` vis) . fst) allboards
            vis' = S.insert bd vis
        if null newboards
          then putStrLn "no new board, backtracking..." >> return False
          else do
            allResult <- mapM (\(b, op) -> print op >> dfs_ vis' b)
                newboards
            return $ or allResult

-- currently bfs fails to solve most of the games
type St = State (S.HashSet Board)

bfs' :: Sq.Seq Board -> St Bool
bfs' q = case Sq.viewl q of
            Sq.EmptyL -> return False
            b Sq.:< bs ->
                if isFinished b
                  then return True
                  else do
                    vis <- get
                    modify (S.insert b)
                    let allmoves = map fst $ allPossibleMoves b
                        newMoves = filter (not . (`S.member` vis)) allmoves
                    bfs' $ foldl' (Sq.|>) bs newMoves

bfs :: Board -> Bool
bfs b = evalState (bfs' $ Sq.singleton b) S.empty

-- simplifier

simplify :: Board -> [Operation] -> [Operation]
simplify bd ops =
    let allboard = scanl modifyBoard bd ops
        allboard_e = zip allboard [0..]
        possibleBoards = map allPossibleMoves allboard
        helper (bds, op) idx = op
        newops = simplify' allboard possibleBoards ops
    in if length ops == length newops
         then ops
         else simplify bd newops

simplify' :: [Board] -> [[(Board, Operation)]] -> [Operation] ->
    [Operation]
simplify' [] _ ops = ops
simplify' _ [] ops = ops
simplify' _ _ [] = []
simplify' bds (bo:bos) ops =
    let bds' = zip bds [0..]
        sb = S.fromList $ map fst bo
        r = map (first (`S.member` sb)) $ drop 2 bds'
        rr = filter fst r
    in if null rr
         then head ops : simplify' (tail bds) bos (tail ops)
         else let idx = snd . head $ rr
                  pickb = bds !! idx
                  Just newOp = lookup pickb bo
                in newOp : drop idx ops
