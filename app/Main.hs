module Main where

import Solver
import Parser
import qualified Data.HashSet as S

main :: IO ()
{- main = print $ dfs S.empty finished -}
main = getBoard >>= print . bfs
