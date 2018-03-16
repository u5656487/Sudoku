module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.IO

import Sudoku (solve)

-- Solve Sudoku puzzles from a file of Sudoku strings
main = do
  [f] <- getArgs
  lines <$> readFile f >>= mapM_ (mapM_ putStrLn . solve)
