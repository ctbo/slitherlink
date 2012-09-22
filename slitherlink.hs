-- slitherlink.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
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
data LoopStatus = Unfinished Segments | OneLoop | Invalid deriving (Show, Eq)

addSegment :: Index -> Index -> LoopStatus -> LoopStatus
addSegment i j (Unfinished l) = 
    case (Map.lookup i l,  Map.lookup j l) of
      (Nothing, Nothing) -> Unfinished $ Map.insert i  j  $ Map.insert j  i  l
      (Just i', Nothing) -> Unfinished $ Map.insert i' j  $ Map.insert j  i' $ Map.delete i l
      (Nothing, Just j') -> Unfinished $ Map.insert i  j' $ Map.insert j' i  $ Map.delete j l
      (Just i', Just j') -> if i' == j
                               then if Map.null $ Map.delete i $ Map.delete j l
                                       then OneLoop -- the only loop has been closed
                                       else Invalid -- a loop has closed but there is more
                               else Unfinished $ Map.insert i' j' $ Map.insert j' i'
                                               $ Map.delete i     $ Map.delete j     l
addSegment _ _ _ = Invalid

data TwoLines = TwoLines { lRight :: Bool, lDown :: Bool } deriving Show
data State = State { sProblem  :: Problem
                   , sLines    :: Array Index TwoLines
                   , sLoops    :: LoopStatus 
                   }

stateFromProblem :: Problem -> State
stateFromProblem p = State p (array ((0, 0), (rn+1, cn+1)) []) (Unfinished Map.empty)
    where ((0, 0), (rn, cn)) = bounds p

step :: State -> Index -> [State]
step (State problem lines loops) i@(r, c)  =
     ( if aboveOK 0 && leftOK 0 && ulLines `elem` [0, 2]
          then [State problem (lines//[(i, TwoLines False False)]) loops]
          else []
     )
     ++
     ( if c < cn && aboveOK 1 && leftOK 0 && ulLines == 1
          then let newLoops = addSegment (r, c) (r, c+1) loops
               in if newLoops /= Invalid
                  then [State problem (lines//[(i, TwoLines {lRight=True, lDown=False})]) newLoops]
                  else []
          else []
     )
     ++
     ( if r < rn && aboveOK 0 && leftOK 1 && ulLines == 1
          then let newLoops = addSegment (r, c) (r+1, c) loops
               in if newLoops /= Invalid
                  then [State problem (lines//[(i, TwoLines {lRight=False, lDown=True})]) newLoops]
                  else []
          else []
     )
     ++
     (
       if c < cn && r < rn && aboveOK 1 && leftOK 1 && ulLines == 0
          then let newLoops = addSegment (r+1, c) (r, c+1) loops
               in if newLoops /= Invalid
                  then [State problem (lines//[(i, TwoLines True True)]) newLoops]
                  else []
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
           rightLine j = inRange (bounds lines) j && lRight (lines!j)
           downLine  j = inRange (bounds lines) j && lDown  (lines!j)
           constraint j = if inRange (bounds problem) j then problem!j else Unconstrained
           count = sum . map (\b -> if b then 1 else 0)

evenRow :: Int -> State -> [State]
evenRow r state@(State _ lines _) =
    if even $ sum $ map (\c -> if lDown (lines!(r,c)) then 1 else 0 :: Int) [0..cn]
       then [state]
       else []
    where ((0,0), (_, cn)) = bounds lines

solve :: Problem -> [State]
solve problem = foldM lineStep start [0 .. rn]
    where start = stateFromProblem problem
          ((0, 0), (rn, cn)) = bounds (sLines start)
          lineStep state r = foldM step state [(r, c) | c <- [0..cn]] >>= evenRow r

showSolution :: State -> String
showSolution (State problem lines _) = concatMap twoLines [0..rn]
    where ((0, 0), (rn, cn)) = bounds lines
          twoLines r = evenLine r ++ "\n" ++ oddLine r ++ "\n"
          evenLine r = concatMap (evenCell r) [0..cn]
          evenCell r c = " " ++ (if lRight (lines!(r, c)) then "-" else " ")
          oddLine r = concatMap (oddCell r) [0..cn]
          oddCell r c = (if lDown (lines!(r, c)) then "|" else " ") ++ constraint (r, c)
          constraint i = if inRange (bounds problem) i then show (problem!i) else " "

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
                   putStr $ concatMap showSolution display
                   putStrLn $ "Total number of solutions: " ++ show (length solutions)

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


-- stuff for interactive experiments

sampleProblem :: Problem
sampleProblem = case readProblem sampleProblemString of 
  Right x -> x
  Left _ -> undefined -- can't happen

