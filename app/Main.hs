{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Solver
import Performer
import Parser
import Types
import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Timeout

performOperations :: [Operation] -> IO [Operation]
performOperations [] = return []
performOperations (Move _ PHua:ops) = performOperations ops
performOperations op@(Move _ (PTR _):ops) = do
    print (head op)
    mousemove'' (0, 0) -- don't block
    let seriesPTR = length $ takeWhile isMoveToPTR ops
    threadDelay $ 400000 * (seriesPTR + 1)
    return op
performOperations (op:ops) = do
    print op
    performOperation op
    performOperations ops

main :: IO ()
main = do
    {- b <- getBoard -}
    {- print . length . dfs $ b -}
    main'

main' :: IO ()
main' = do
    {- threadDelay 1000000 -}
    (b' :: Either SomeException Board) <- try getBoard
    case b' of
        Left err -> do
            putStrLn "wait 1 more rounds"
            getBoard' 1 >>= perform
        Right b -> perform b
    startNewGame

startNewGame :: IO ()
startNewGame = do
    mousemove'' (1220, 728)
    threadDelay 150000
    mousedown
    threadDelay 150000
    mouseup
    threadDelay 5000000
    main'

isMoveToPTR :: Operation -> Bool
isMoveToPTR (Move _ (PTR _)) = True
isMoveToPTR _ = False

perform :: Board -> IO ()
perform b = do
    r <- timeout 20000000 (return $! dfs b)
    case r of
        Nothing -> startNewGame
        Just [] -> return ()
        Just allop@(op:op_) -> do
            print op
            if all isMoveToPTR allop
              then threadDelay 5000000 >> startNewGame
              else do
                performOperation op
                r <- performOperations op_
                unless (null r) main'

finishedBoard :: Board
finishedBoard = Board (TLFull, TLFull, TLFull) HSSingleton
    (TRTaken (Wan 9), TRTaken (Tong 9), TRTaken (Tiao 9)) (replicate 8 [])

getBoard' :: Int -> IO Board
getBoard' 0 = return finishedBoard -- something's wrong, or finished
getBoard' n = do
    (b :: Either InvalidBoard Board) <- try getBoard
    case b of
        Left _ -> threadDelay 2000000 >> getBoard' (n - 1)
        Right b' -> return b'

