-- slitherlink.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import System.Environment
import Data.List (find)
import Data.Either
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
import Control.Parallel.Strategies

data Constraint = Unconstrained | Exactly Int deriving (Eq)
instance Show Constraint where
    show Unconstrained = "."
    show (Exactly x) = show x

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

type SpaceIndex = (Int, Int)
type LineIndex = (Int, Int)
type Index = (Int, Int)
type Problem = Array SpaceIndex Constraint

readProblem :: String -> Either String Problem
readProblem s = do
            pl <- readProblemList s
            when (null pl) $ Left "Problem is empty."
            let columns = length $ head pl
            when (columns == 0) $ Left "Problem starts with an empty line."
            unless (all ((== columns) . length) pl) $ Left "Problem not rectangular."
            let rows = length pl
            return $ listArray ((0, 0), (rows-1, columns-1)) $ concat pl 

data LoopStatus = Pieces (Map.Map Index Index) | OneLoop | Invalid deriving (Show, Eq)
 
addSegment :: (Index, Index) -> LoopStatus -> LoopStatus
addSegment (i,j) (Pieces l) = 
    case (Map.lookup i l,  Map.lookup j l) of
      (Nothing, Nothing) -> Pieces $ Map.insert i  j  $ Map.insert j  i  l
      (Just i', Nothing) -> Pieces $ Map.insert i' j  $ Map.insert j  i' $ Map.delete i l
      (Nothing, Just j') -> Pieces $ Map.insert i  j' $ Map.insert j' i  $ Map.delete j l
      (Just i', Just j') -> if i' == j
                               then if Map.null $ Map.delete i $ Map.delete j l
                                       then OneLoop -- the only loop has been closed
                                       else Invalid -- a loop has closed but there is more
                               else Pieces $ Map.insert i' j' $ Map.insert j' i'
                                           $ Map.delete i     $ Map.delete j     l
addSegment _ _ = Invalid

data FourLines = FourLines { top :: Bool
                           , right :: Bool
                           , bottom :: Bool
                           , left :: Bool
                           } deriving (Eq, Show)

countLines :: FourLines -> Int
countLines x = sum [ 1 | True <- [ top x, right x, bottom x, left x]]

flAll :: [FourLines]
flAll = [FourLines t r b l | t <- [False, True]
                           , r <- [False, True]
                           , b <- [False, True]
                           , l <- [False, True]
        ]

flConstraint :: Constraint -> [FourLines]
flConstraint Unconstrained = flAll
flConstraint (Exactly n) = filter ((==n) . countLines) flAll

flXing :: [FourLines]
flXing = filter (\fl -> countLines fl `elem` [0, 2]) flAll

data LineState = Line [Bool]
data SpaceState = Space [FourLines] (Maybe Constraint) deriving (Eq, Show)
data State =  State { sSpaces :: Array SpaceIndex SpaceState
                    , sLines :: Array LineIndex LineState
                    , sLoops :: LoopStatus }

stateFromProblem :: Problem -> State
stateFromProblem p =
    State (array ((0, 0), (rows, columns)) spaces)
          (array ((0, 0), (rows, columns)) lines)
          (Pieces Map.empty)
  where ((0, 0), (rn, cn)) = bounds p
        rows    = 2*rn + 2
        columns = 2*cn + 2
        spaces = [((r, c), Space flXing Nothing) | r <- [0, 2 .. 2*rn+2], c <- [0, 2 .. 2*cn+2]]
             ++ [((2*r+1, 2*c+1), Space (flConstraint cst) (Just cst))| r <- [0 .. rn], c <- [0 .. cn], let cst=p!(r, c)]
        lines = [((r, c), Line [False, True]) | r <- [0, 2 .. 2*rn+2], c <- [1, 3 .. 2*cn+1]]
             ++ [((r, c), Line [False, True]) | r <- [1, 3 .. 2*rn+1], c <- [0, 2 .. 2*cn+2]]

type Direction = (Int, Int)
directions4 :: [Direction] -- right, down, left, up
directions4 = [(0, 1), (1, 0), (0, -1), (-1, 0)]
diagonals4 :: [Direction] -- right down, down left, left up, up right
diagonals4 = [(1, 1), (1, -1), (-1, -1), (-1, 1)]
directions8 :: [Direction]
directions8 = directions4 ++ diagonals4

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

indexType :: Index -> Either LineIndex SpaceIndex
indexType (r, c) = if even r == even c then Right (r, c) else Left (r, c)

lineNeighbors :: LineIndex -> [Either LineIndex SpaceIndex]
lineNeighbors i = map indexType $ map (i .+) directions4

linesAtSpace :: SpaceIndex -> [LineIndex]
linesAtSpace i = map (i .+) directions4

spacesAtLine :: LineIndex -> [SpaceIndex]
spacesAtLine i = map (i .+) directions4

spacesAtSpace :: SpaceIndex -> [LineIndex]
spacesAtSpace i = map (i .+) diagonals4

spaceNeighbors :: SpaceIndex -> [Either LineIndex SpaceIndex]
spaceNeighbors i = map indexType $ map (i .+) directions8

vertical :: LineIndex -> Bool
vertical (r, _) = odd r

dotsAtLine :: LineIndex -> (SpaceIndex, SpaceIndex)
dotsAtLine i = if vertical i then (i .+ (-1, 0), i .+ (1, 0))
                             else (i .+ (0, -1), i .+ (0, 1))
                                
allDotIndices :: State -> [SpaceIndex]
allDotIndices state = [(r, c) | r <- [0, 2 .. rn], c <- [0, 2 .. cn]]
    where ((0, 0), (rn, cn)) = bounds (sSpaces state)

allLineIndices :: State -> [LineIndex]
allLineIndices state = lefts $ allIndices state

allIndices :: State -> [Either LineIndex SpaceIndex]
allIndices state = map indexType $ indices (sSpaces state)

grid :: State -> [[Either LineIndex SpaceIndex]]
grid state = [ [ indexType (r,c) | c <- [c0..cn] ] | r <- [r0 .. rn] ]
  where ((r0, c0), (rn, cn)) = bounds (sSpaces state)

type Seed = Set.Set (Either LineIndex SpaceIndex) 

narrow :: Seed -> State -> [State]
narrow seed state = if Set.null seed then [state] else
    case Set.deleteFindMin seed of
        (Left li, seed') ->
            if not (inRange (bounds (sLines state)) li)
            then narrow seed' state
            else narrowLine li seed' state
        (Right si, seed') -> 
            if not (inRange (bounds (sSpaces state)) si)
            then narrow seed' state
            else narrowSpace si seed' state

narrowLine :: LineIndex -> Seed -> State -> [State]
narrowLine i seed state = 
    case (sLines state)!i of
      Line ls -> do
        let ls' = filter (\b -> all (\(si, lineThere) -> match si state lineThere b) $
                            zip (spacesAtLine i)  [left, top, right, bottom]) ls
        if null ls' 
          then [] 
          else if ls == ls 
            then narrow seed state 
            else let newSeeds = Set.fromList $ lineNeighbors i
                     newLoops = if ls' == [True]
                                then addSegment (dotsAtLine i) (sLoops state)
                                else sLoops state
                 in if newLoops /= Invalid
                    then narrow (Set.union seed newSeeds) $
                        state { sLines = sLines state // [(i, Line ls')],
                                sLoops = newLoops}
                    else []

narrowSpace :: SpaceIndex -> Seed -> State -> [State]
narrowSpace i seed state = 
    case (sSpaces state)!i of
      Space ss cst -> do
        let ss' = filter (\x -> all (\(li, lineThere) -> matchl li state lineThere) $
                            zip (linesAtSpace i)  [right x, bottom x, left x, top x])
                $ filter (\x -> all (\(si, linesThere) -> match2 si state linesThere x) $
                            zip (spacesAtSpace i) [[(top, right), (left, bottom)],
                                                   [(top, left), (right, bottom)],
                                                   [(bottom, left), (right, top)],
                                                   [(bottom, right), (left, top)]])
                ss
        if null ss'
          then []
          else if ss' == ss
            then narrow seed state
            else let newSeeds = Set.fromList $ spaceNeighbors i
                 in narrow (Set.union seed newSeeds) $
                    state { sSpaces = sSpaces state // [(i, Space ss' cst)] }

match :: SpaceIndex -> State -> (FourLines -> Bool) -> Bool -> Bool
match i state f x = (not (inRange (bounds (sSpaces state)) i))
                           || check (sSpaces state ! i)
    where check (Space xs _) = any ((==x).f) xs

matchl :: LineIndex -> State -> Bool -> Bool
matchl i state x = 
    if inRange (bounds (sLines state)) i
       then check (sLines state ! i)
       else x == False -- no lines allowed outside grid
    where check (Line ls) = x `elem` ls

match2 :: (Int, Int) -> State -> [(FourLines -> Bool, FourLines -> Bool)] -> FourLines -> Bool
match2 i state fps thiscell = (not (inRange (bounds (sSpaces state)) i)) || any ok otherlist
    where Space otherlist _ = sSpaces state ! i
          ok othercell = all pairmatch fps
              where pairmatch (otherf, thisf) = thisf thiscell == otherf othercell


narrowAll :: State -> [State]
narrowAll state = narrow (Set.fromList (allIndices state)) state

solve :: Problem -> [State]
solve problem = do
    state <- narrowAll $ stateFromProblem problem
    solve' 0 state

solve' :: Int -> State -> [State]
solve' depth state =
--    (if depth >= 35 then trace (showState state) else id) $
    case sLoops state of
         Pieces p -> if Map.null p
                     then case find undecided evenGrid of
                               Just i -> continueAt i
                               Nothing -> []
                     else continueAt $ head $ Map.keys p
         OneLoop -> zeroRemainingLines state
         Invalid -> []
    where evenGrid = allDotIndices state
          undecided i = undecided' (sSpaces state ! i)
          undecided' (Space (_:_:_) _) = True -- list has at least 2 elements
          undecided' _ = False 
          continueAt i = concat $ parMap rseq fix list
            where (Space list cst) = sSpaces state ! i
                  fix ss = narrow neighbors (state { sSpaces = sSpaces state // [(i, Space [ss] cst)] })
                                 >>= solve' (depth+1)
                  neighbors = Set.fromList $ spaceNeighbors i

zeroRemainingLines :: State -> [State]
zeroRemainingLines state = foldM zeroLine state (allLineIndices state) >>= narrowAll
    where zeroLine state i = case sLines state ! i of
                   Line [True] -> [state]
                   Line [False, True] -> [state { sLines = sLines state // [(i, Line [False])] }]
                   _ -> [state]

showState :: State -> String
showState state = unlines $ map oneLine (grid state)
  where oneLine = concat . map oneCell
        oneCell (Left li) = showLine (vertical li) $ sLines state ! li
        oneCell (Right si) = showSpace $ sSpaces state ! si
        showLine vertical (Line [True])        = if vertical then "|" else "-"
        showLine _        (Line [False])       = " "
        showLine _        (Line _)             = "?"
        showSpace         (Space _ (Just cst)) = show cst
        showSpace         (Space ls Nothing)   = if hasLine ls then "+" else " "
        hasLine ls = not (FourLines False False False False `elem` ls)

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
                   putStrLn $ "Showing " ++ (if n == 0 then "all" else "up to " ++ show n) ++ " solutions."
                   let solutions = solve p
                   let display
                         | n == 0 = solutions
                         | otherwise = take n solutions
                   putStr $ concatMap showState display
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


