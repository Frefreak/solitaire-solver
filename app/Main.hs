module Main where

import Solver
import Parser
import qualified Data.Set as S

main :: IO ()
main = getBoard >>= print . dfs S.empty
