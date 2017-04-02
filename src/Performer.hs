module Performer where

import System.Process
import Control.Concurrent
import Control.Monad
import Types

boxsize = 24
ctopleft = (122, 26)
cbotleft1_1 = (122, 290)
cbotleft1_2 = (122, 321)
cbotleft2_1 = (274, 290)
chua = (689, 26)
czhong = (600, 50)
cfa = (600, 140)
cbai = (600, 220)

voffset = snd cbotleft1_2 - snd cbotleft1_1
hoffset = fst cbotleft2_1 - fst cbotleft1_1

getmouselocation :: IO (Int, Int)
getmouselocation = do
    str <- readProcess "xdotool" ["getmouselocation", "--shell"] []
    let [x, y] = take 2 $ lines str
    return (read $ drop 2 x, read $ drop 2 y)

speed :: Double
speed = 18

-- move mouse to coordinate at speed sp
mousemove :: (Int, Int) -> Double -> IO ()
mousemove (x, y) sp = do
    (x', y') <- getmouselocation
    let (dx, dy) = (x - x', y' - y)
        r = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
        theta = tanh (fromIntegral dy / fromIntegral dx) * 180 / pi
        interval = floor (r / sp)
    forM_ [1..interval] $ \_ -> do
        readProcess "xdotool" ["mousemove_relative", "--polar", "--",
            show (90 - theta), show sp] []
        threadDelay 10000
    void $ readProcess "xdotool" ["mousemove", show x, show y] []

mousemove' :: (Int, Int) -> IO ()
mousemove' = flip mousemove speed

mousemove'' :: (Int, Int) -> IO ()
mousemove'' (x, y) = void $ readProcess "xdotool"
    ["mousemove", show x, show y] []

mousedown :: IO ()
mousedown = void $ readProcess "xdotool" ["mousedown", "1"] []

mouseclick :: IO ()
mouseclick = void $ readProcess "xdotool" ["click", "1"] []

mouseup :: IO ()
mouseup = void $ readProcess "xdotool" ["mouseup", "1"] []

coord :: Position -> (Int, Int)
coord (PMain x y) =
    let (a, b) = cbotleft1_1 in (a + hoffset * x, b + voffset * y)
coord (PTL i) =
    let (a, b) = ctopleft in (a + hoffset * i, b)
coord (PTR i) =
    let (a, b) = ctopleft in (a + hoffset * (i + 5), b)
coord PHua = chua

performOperation :: Operation -> IO ()
{- performOperation (Move p1 (PTR i)) = do -}
    {- let (cx, cy) = coord p1 -}
    {- mousemove'' (cx, min (cy + 210) 690) -}
    {- threadDelay 150000 -}
    {- mousedown -}
    {- threadDelay 150000 -}
    {- let (dx, dy) = coord (PTR i) -}
    {- mousemove'' (dx, dy + 210) -}
    {- threadDelay 150000 -}
    {- mouseup -}
    {- threadDelay 100000 -}
performOperation (Move p1 p2) = do
    mousemove'' (coord p1)
    threadDelay 150000
    mousedown
    threadDelay 150000
    mousemove'' (coord p2)
    threadDelay 150000
    mouseup
    threadDelay 200000
performOperation (Slay Zhong) = do
    mousemove'' czhong
    threadDelay 200000
    mousedown
    threadDelay 100000
    mouseup
    threadDelay 1000000
performOperation (Slay Fa) = do
    mousemove'' cfa
    threadDelay 200000
    mousedown
    threadDelay 100000
    mouseup
    threadDelay 1000000
performOperation (Slay Bai) = do
    mousemove'' cbai
    threadDelay 200000
    mousedown
    threadDelay 100000
    mouseup
    threadDelay 1000000

