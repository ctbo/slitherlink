-- slitherlink.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import System.Environment
import Data.List (find)
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

type Index = (Int, Int)
type Problem = Array Index Constraint

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
 
addSegment :: Index -> Index -> LoopStatus -> LoopStatus
addSegment i j (Pieces l) = 
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
addSegment _ _ _ = Invalid

data FourLines = FourLines { top :: Bool
                           , right :: Bool
                           , bottom :: Bool
                           , left :: Bool
                           } deriving (Eq, Show)

countLines :: FourLines -> Int
countLines x = count top + count right + count bottom + count left
           where count f = if f x then 1 else 0

flListAll :: [FourLines]
flListAll = [FourLines t r b l | t <- [False, True]
                               , r <- [False, True]
                               , b <- [False, True]
                               , l <- [False, True]
            ]

flListForConstraint :: Constraint -> [FourLines]
flListForConstraint Unconstrained = flListAll
flListForConstraint (Exactly n) = filter ((==n) . countLines) flListAll

flListXing :: [FourLines]
flListXing = filter (zeroOrTwo.countLines) flListAll
    where zeroOrTwo 0 = True
          zeroOrTwo 2 = True
          zeroOrTwo _ = False

data CellState = Line [Bool]
               | Space [FourLines] (Maybe Constraint) deriving (Eq, Show)
data State =  State { sCells :: Array Index CellState
                    , sLoops :: LoopStatus }

stateFromProblem :: Problem -> State
stateFromProblem p = State (array ((0, 0), (rows, columns)) cells) (Pieces Map.empty)
  where ((0, 0), (rn, cn)) = bounds p
        rows    = 2*rn + 2
        columns = 2*cn + 2
        cells = [((r, c), Space flListXing Nothing) | r <- [0, 2 .. 2*rn+2], c <- [0, 2 .. 2*cn+2]]
             ++ [((r, c), Line [False, True]) | r <- [0, 2 .. 2*rn+2], c <- [1, 3 .. 2*cn+1]]
             ++ [((r, c), Line [False, True]) | r <- [1, 3 .. 2*rn+1], c <- [0, 2 .. 2*cn+2]]
             ++ [((2*r+1, 2*c+1), Space (flListForConstraint cst) (Just cst))| r <- [0 .. rn], c <- [0 .. cn], let cst=p!(r, c)]

type Direction = (Int, Int)
directions4 :: [Direction] -- right, down, left, up
directions4 = [ (0, 1)
              , (1, 0)
              , (0,-1)
              , (-1,0)
              ]
directions8 :: [Direction] -- right down, down left, left up, up right
directions8 = directions4 ++ [ (1,  1)
                             , (1, -1)
                             , (-1,-1)
                             , (-1, 1)
                             ]

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

narrow :: Set.Set Index -> State -> [State]
narrow seed state = if Set.null seed then [state] else
    let (i@(r,c), seed') = Set.deleteFindMin seed in
      if not (inRange (bounds (sCells state)) i) then narrow seed' state else
    case (sCells state)!i of
      Line ls -> do
        let ls' = filter (match (r-1, c) state bottom)
                $ filter (match (r, c+1) state left)
                $ filter (match (r+1, c) state top)
                $ filter (match (r, c-1) state right) ls
        if null ls' 
          then [] 
          else if ls' == ls 
            then narrow seed' state 
            else let newSeeds = Set.fromList $ map (i .+) directions4
                     newLoops = if ls' == [True]
                                then if odd r
                                     then addSegment (r-1, c) (r+1, c) (sLoops state)
                                     else addSegment (r, c-1) (r, c+1) (sLoops state)
                                else sLoops state
                 in if newLoops /= Invalid
                    then narrow (Set.union seed' newSeeds) (State (sCells state // [(i, Line ls')]) newLoops)
                    else []
      Space ss cst -> do
        let ss' = filter ((matchl (r-1, c) state).top)
                $ filter ((matchl (r, c+1) state).right)
                $ filter ((matchl (r+1, c) state).bottom)
                $ filter ((matchl (r, c-1) state).left) 
                $ filter (match2 (r-1, c-1) state [(bottom, left), (right, top)])
                $ filter (match2 (r-1, c+1) state [(bottom, right), (left, top)])
                $ filter (match2 (r+1, c-1) state [(top, left), (right, bottom)])
                $ filter (match2 (r+1, c+1) state [(top, right), (left, bottom)]) 
                ss
        if null ss'
          then []
          else if ss' == ss
            then narrow seed' state
            else let newSeeds = Set.fromList $ map (i .+) directions8
                 in narrow (Set.union seed' newSeeds) (State (sCells state // [(i, Space ss' cst)]) (sLoops state))

match :: Index -> State -> (FourLines -> Bool) -> Bool -> Bool
match i (State cells _) f x = (not (inRange (bounds cells) i)) 
                           || check (cells!i)
    where check (Space xs _) = any ((==x).f) xs
          check _ = undefined -- can't happen

matchl :: Index -> State -> Bool -> Bool
matchl i (State cells _) x = 
    if inRange (bounds cells) i
       then check (cells!i)
       else x == False -- no lines allowed outside grid
    where check (Line ls) = x `elem` ls
          check _ = undefined -- can't happen

match2 :: (Int, Int) -> State -> [(FourLines->Bool, FourLines->Bool)] -> FourLines -> Bool
match2 i (State cells _) fps thiscell = (not (inRange (bounds cells) i)) || any ok otherlist
    where Space otherlist _ = cells!i
          ok othercell = all pairmatch fps
              where pairmatch (otherf, thisf) = thisf thiscell == otherf othercell

narrowAll :: State -> [State]
narrowAll state = narrow (Set.fromList (indices (sCells state))) state

solve :: Problem -> [State]
solve problem = do
    state <- narrowAll $ stateFromProblem problem
    solve' 0 state

solve' :: Int -> State -> [State]
solve' depth state@(State cells loops) =
--    (if depth >= 35 then trace (showState state) else id) $
    case loops of
         Pieces p -> if Map.null p
                     then case find undecided evenGrid of
                               Just i -> continueAt i
                               Nothing -> []
                     else continueAt $ head $ Map.keys p
         OneLoop -> zeroRemainingLines state
         Invalid -> []
    where ((0, 0), (rn, cn)) = bounds cells
          evenGrid = [(r, c) | r <- [0, 2 .. rn], c <- [0, 2 .. cn]]
          undecided i = undecided' (cells!i)
          undecided' (Space (_:_:_) _) = True -- list has at least 2 elements
          undecided' _ = False 
          continueAt i = concat $ parMap rseq fix list
            where (Space list cst) = cells!i
                  fix ss = narrow neighbors (State (cells // [(i, Space [ss] cst)]) loops) >>= solve' (depth+1)
                  neighbors = Set.fromList $ map (i .+) directions8

zeroRemainingLines :: State -> [State]
zeroRemainingLines state = foldM zeroLine state (indices (sCells state)) >>= narrowAll
    where zeroLine state@(State cells loops) i = case cells!i of
                   Line [True] -> [state]
                   Line [False, True] -> [State (cells // [(i, Line [False])]) loops]
                   _ -> [state]

hasLine :: CellState -> Bool
hasLine (Space ls _) = not (FourLines False False False False `elem` ls)
hasLine _            = undefined -- can't happen

showState :: State -> String
showState (State cells _) = (unlines $ map oneLine [r0 .. rn])
  where ((r0, c0), (rn, cn)) = bounds cells
        oneLine r = concat $ map (oneCell r) [c0 .. cn]
        oneCell r c 
          | even r && even c = showDot $ cells ! (r, c)
          | odd r && odd c = showConstraint $ cells ! (r, c)
          | otherwise = showLine (isVertical r) $ cells ! (r, c)
        isVertical = odd
        showLine vertical s = case s of
          Line [True]  -> if vertical then "|" else "-"
          Line [False] -> " "
          Line _       -> "?"
          _            -> undefined -- can't happen
        showDot s = if hasLine s then "+" else " "
        showConstraint (Space _ (Just cst)) = show cst
        showConstraint _             = undefined -- can't happen

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


