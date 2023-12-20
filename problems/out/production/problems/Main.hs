module Main (main) where

import System.Environment  
import Task1 (solveMoves)


-- Arguments: row column size
main :: IO ()
main = do
    args <- getArgs
    let a = map read args
    print $ solveMoves (head a) (a !! 1) (a !! 2)