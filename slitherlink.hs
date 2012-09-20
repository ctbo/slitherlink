-- slitherlink.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import Data.List (find)
import System.Environment
import qualified Data.Map as Map


data Constraint = Unconstrained | Exactly Int deriving (Eq)
instance Show Constraint where
    show Unconstrained = "."
    show (Exactly x) = show x

match :: Constraint -> Int -> Bool
match Unconstrained _ = True
match (Exactly x)   y = x == y 

readConstraint :: Char -> Either String Constraint
readConstraint '.' = Right Unconstrained
readConstraint '0' = Right $ Exactly 0
readConstraint '1' = Right $ Exactly 1
readConstraint '2' = Right $ Exactly 2
readConstraint '3' = Right $ Exactly 3
readConstraint c = Left $ "Invalid character " ++ show c ++ "."

type ProblemList = [[Constraint]]

readProblemList ::  String -> Either String ProblemList
readProblemList = (mapM . mapM) readConstraint . lines

type Problem = Array (Int, Int) Constraint

readProblem :: String -> Either String Problem
readProblem s = do
            pl <- readProblemList s
            when (null pl) $ Left "Problem is empty."
            let columns = length $ head pl
            when (columns == 0) $ Left "Problem starts with an empty line."
            unless (all ((== columns) . length) pl) $ Left "Problem not rectangular."
            let rows = length pl
            return $ listArray ((0, 0), (rows-1, columns-1)) $ concat pl 

type Index = (Int, Int)
type Segments = Map.Map Index Index

addSegment :: Index -> Index -> Segments -> Maybe Segments
addSegment i j l = 
    case (Map.lookup i l,  Map.lookup j l) of
      (Nothing, Nothing) -> Just $ Map.insert i  j  $ Map.insert j  i  l
      (Just i', Nothing) -> Just $ Map.insert i' j  $ Map.insert j  i' $ Map.delete i l
      (Nothing, Just j') -> Just $ Map.insert i  j' $ Map.insert j' i  $ Map.delete j l
      (Just i', Just j') -> if i' == j
                               then if Map.null $ Map.delete i $ Map.delete j l
                                       then Just Map.empty -- the only loop has been closed
                                       else Nothing -- a loop has closed but there is more
                               else Just $ Map.insert i' j' $ Map.insert j' i'
                                         $ Map.delete i     $ Map.delete j     l

data TwoLines = TwoLines { lRight :: Bool, lDown :: Bool } deriving Show
data State = State { sProblem  :: Problem
                   , sLines    :: Array Index TwoLines
                   , sSegments :: Segments 
                   }

stateFromProblem :: Problem -> State
stateFromProblem p = State p (array ((0, 0), (rn+1, cn+1)) []) Map.empty
    where ((0, 0), (rn, cn)) = bounds p

step :: Index -> State -> [State]
step i@(r, c) (State problem lines segments) =
     ( if aboveOK 0 && leftOK 0 && ulLines `elem` [0, 2]
          then [State problem (lines//[(i, TwoLines False False)]) segments]
          else []
     )
     ++
     ( if c < cn && aboveOK 1 && leftOK 0 && ulLines == 1
          then [State problem (lines//[(i, TwoLines {lRight=True, lDown=False})]) segments] -- FIXME loop detection
          else []
     )
     ++
     ( if r < rn && aboveOK 0 && leftOK 1 && ulLines == 1
          then [State problem (lines//[(i, TwoLines {lRight=False, lDown=True})]) segments] -- FIXME loop detection
          else []
     )
     ++
     (
       if c < cn && r < rn && aboveOK 1 && leftOK 1 && ulLines == 0
          then [State problem (lines//[(i, TwoLines True True)]) segments] -- FIXME loop detection
          else []
     )
     where ((0, 0), (rn, cn)) = bounds lines
           aboveOK b = match aboveConstraint (aboveLines + b)
           leftOK b = match leftConstraint (leftLines+b) || match leftConstraint (leftLines+b+1)
           aboveLines = count [rightLine (r-1, c), downLine (r-1, c), downLine (r-1, c+1)]
           leftLines = count [rightLine (r, c-1), downLine (r, c-1)]
           ulLines = count [downLine (r-1, c), rightLine (r, c-1)]
           aboveConstraint = constraint (r-1, c)
           leftConstraint = constraint (r, c-1)
           rightLine i = inRange (bounds lines) i && lRight (lines!i)
           downLine  i = inRange (bounds lines) i && lDown  (lines!i)
           constraint i = if inRange (bounds problem) i then problem!i else Unconstrained
           count = sum . map (\b -> if b then 1 else 0)


{-
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
          [] -> work sampleProblemString 2
          _  -> error "Too many arguments."
  where work s n = case readProblem s of
             Left e -> putStrLn e
             Right p -> do
                   putStrLn $ "Showing " ++ (if n == 0 then "all" else show n) ++ " solutions."
                   let solutions = solve p
                   let display
                         | n == 0 = solutions
                         | otherwise = take n solutions
                   putStr $ concatMap (showSolution p) display
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)
-}

-- stuff for interactive experiments

sampleProblemString :: String
sampleProblemString = unlines [".3.112.2.."
                              ,".3..3.1312"
                              ,"22.1......"
                              ,".3..3..2.2"
                              ,"2.....2.21"
                              ,"31.3.....3"
                              ,"2.2..3..2."
                              ,"......1.32"
                              ,"2220.3..3."
                              ,"..3.122.2."
                              ]

sampleProblem :: Problem
sampleProblem = case readProblem sampleProblemString of 
  Right x -> x
  Left _ -> undefined -- can't happen

