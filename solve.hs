-- solve.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Slitherlink
import System.Environment

main :: IO ()
main = do
     args <- getArgs
     case args of
          [filename, number] -> do
                     s <- readFile filename
                     work s (read number)
          [filename] -> do
                     s <- readFile filename
                     work s 2
          [] -> error "Usage: solve filename [#solutions]"
          _  -> error "Too many arguments."
  where work s n = case readProblem s of
             Left e -> putStrLn e
             Right p -> do
                   putStrLn $ "Showing " ++ (if n == 0 then "all" else "up to " ++ show n) ++ " solutions."
                   let solutions = solve p
                   let display
                         | n == 0 = solutions
                         | otherwise = take n solutions
                   putStr $ concatMap showState display
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)
