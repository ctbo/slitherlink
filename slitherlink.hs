-- slitherlink.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import Data.Foldable (foldrM)
import Data.Maybe (isJust)
import Data.List (find)
import System.Environment
import qualified Data.Set as Set

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
               | Space [FourLines] deriving (Eq, Show)
type State =  Array (Int, Int) CellState

stateFromProblem :: Problem -> State
stateFromProblem p = array ((0, 0), (rows, columns)) cells
  where ((0, 0), (rn, cn)) = bounds p
        rows    = 2*rn + 2
        columns = 2*cn + 2
        cells = [((r, c), Space flListXing) | r <- [0, 2 .. 2*rn+2], c <- [0, 2 .. 2*cn+2]]
             ++ [((r, c), Line [False, True]) | r <- [0, 2 .. 2*rn+2], c <- [1, 3 .. 2*cn+1]]
             ++ [((r, c), Line [False, True]) | r <- [1, 3 .. 2*rn+1], c <- [0, 2 .. 2*cn+2]]
             ++ [((2*r+1, 2*c+1), Space (flListForConstraint (p!(r, c))))| r <- [0 .. rn], c <- [0 .. cn]]

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


narrow :: Set.Set (Int, Int) -> State -> Maybe State
narrow seed state = if Set.null seed then Just state else
    let (i@(r,c), seed') = Set.deleteFindMin seed in
      if not (inRange (bounds state) i) then narrow seed' state else
    case state!i of
      Line ls -> do
        let ls' = filter (match (r-1, c) state bottom)
                $ filter (match (r, c+1) state left)
                $ filter (match (r+1, c) state top)
                $ filter (match (r, c-1) state right) ls
        if null ls' 
          then Nothing 
          else if ls' == ls 
            then narrow seed' state 
            else let newSeeds = Set.fromList $ map (i .+) directions4
                 in narrow (Set.union seed' newSeeds) (state // [(i, Line ls')])
      Space ss -> do
        let ss' = filter ((matchl (r-1, c) state).top)
                $ filter ((matchl (r, c+1) state).right)
                $ filter ((matchl (r+1, c) state).bottom)
                $ filter ((matchl (r, c-1) state).left) 
                $ filter (\x -> match2 (r-1, c-1) state bottom (left x) right (top x))
                $ filter (\x -> match2 (r-1, c+1) state bottom (right x) left (top x ))
                $ filter (\x -> match2 (r+1, c-1) state top (left x) right (bottom x))
                $ filter (\x -> match2 (r+1, c+1) state top (right x) left (bottom x)) 
                ss
        if null ss'
          then Nothing
          else if ss' == ss
            then narrow seed' state
            else let newSeeds = Set.fromList $ map (i .+) directions8
                 in narrow (Set.union seed' newSeeds) (state // [(i, Space ss')])

match :: (Int, Int) -> State -> (FourLines -> Bool) -> Bool -> Bool
match i state f x = (not (inRange (bounds state) i)) 
                || check (state!i)
    where check (Space xs) = any ((==x).f) xs
          check _ = undefined -- can't happen

matchl :: (Int, Int) -> State -> Bool -> Bool
matchl i state x = if inRange (bounds state) i
                      then check (state!i)
                      else x == False -- no lines allowed outside grid
    where check (Line ls) = x `elem` ls
          check _ = undefined -- can't happen

match2 :: (Int, Int) -> State -> (FourLines -> Bool) -> Bool -> (FourLines -> Bool) -> Bool -> Bool
match2 i state f1 x1 f2 x2 = (not (inRange (bounds state) i)) 
                || check (state!i)
    where check (Space xs) = any (\x -> f1 x == x1 && f2 x == x2) xs
          check _ = undefined -- can't happen

narrowAll :: State -> Maybe State
narrowAll state = narrow (Set.fromList (indices state)) state

move :: (Int, Int) -> Direction -> State -> Maybe State
move pos dir state = do
          let viaLine = pos .+ dir
          let toDot = pos .+ dir .+ dir
          let affected = Set.fromList $ map (viaLine .+) directions4
          unless (inRange (bounds state) toDot) Nothing
          case state!viaLine of
            Line [True] -> Just state
            Line [False, True] -> narrow affected (state // [(viaLine,Line [True])])
            _ -> Nothing

zeroRemainingLines :: [(Int, Int)] -> State -> Maybe State
zeroRemainingLines trail state = foldrM zero state (indices state) >>= narrowAll
    where zero i s = if i `elem` onTrail
                            then Just s
                            else case s!i of
                              Line [True] -> Nothing
                              Line _      -> Just (s // [(i, Line [False])])
                              _           -> Just s
          onTrail = zipWith middle trail (tail trail ++ [head trail])
          middle (a, b) (c, d) = ((a+c) `div` 2, (b+d) `div` 2)

untilJust :: (b -> Maybe a) -> [b] -> Maybe a
untilJust f = join . find isJust . map f

solve :: Problem -> Maybe State
solve problem = do
  state <- narrowAll $ stateFromProblem problem
  solve' (startingPositions state) state

solve' :: [(Int, Int)] -> State -> Maybe State
solve' is state = untilJust (\i -> solve'' i i [] state) is

solve'' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> State -> Maybe State
solve'' goal pos trail state = untilJust f directions4
            where f dir = do
                      let newPos = pos .+ dir .+ dir
                      when (newPos `elem` trail) Nothing
                      newState <- move pos dir state
                      let newTrail = newPos:trail
                      if newPos == goal
                        then zeroRemainingLines newTrail newState
                        else solve'' goal newPos newTrail newState

startingPositions :: State -> [(Int, Int)]
startingPositions state = if null s then evenIndices else [head s]
    where s = filter (hasLine.(state!)) evenIndices
          ((0, 0), (rn, cn)) = bounds state
          evenIndices = [(r, c) | r <- [0, 2 .. rn], c <- [0, 2 .. cn]]
        
hasLine :: CellState -> Bool
hasLine (Space ls) = not (FourLines False False False False `elem` ls)
hasLine _          = undefined -- can't happen

showSolution :: Problem -> Maybe State -> String
showSolution problem (Just state) = unlines $ map oneLine [r0 .. rn]
  where ((r0, c0), (rn, cn)) = bounds state
        oneLine r = concat $ map (oneCell r) [c0 .. cn]
        oneCell r c 
          | even r && even c = showDot $ state ! (r, c)
          | odd r && odd c = showConstraint (((r-1) `div` 2), ((c-1) `div` 2))
          | otherwise = showLine (isVertical r) $ state ! (r, c)
        isVertical = odd
        showLine vertical s = case s of
          Line [True] -> if vertical then "|" else "-"
          Line _      -> " "
          _           -> undefined -- can't happen
        showDot s = if hasLine s then "+" else " "
        showConstraint i = show $ problem!i
showSolution _ _ = "No solution.\n"

main :: IO ()
main = getArgs >>= f >>= g where
    f [filename] = readFile filename
    f []         = return sampleProblemString
    f _          = error "Too many arguments."
    g pString = case readProblem pString of
      Left e -> putStrLn e
      Right p -> putStrLn $ showSolution p $ solve p


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

showState :: State -> String
showState state = unlines $ map oneLine [r0 .. rn]
  where ((r0, c0), (rn, cn)) = bounds state
        oneLine r = concat $ map (oneCell r) [c0 .. cn]
        oneCell r c = showCell (isVertical r) $ state ! (r, c)
        isVertical = odd
        showCell vertical s = case s of
          Line [False, True] -> " "
          Line [False] -> "x"
          Line [True] -> if vertical then "|" else "-"
          Space fs -> ["0123456789ABCDEFGH" !! (length fs)]
          _ -> undefined -- can't happen

showMaybeState :: Maybe State -> String
showMaybeState Nothing = "No solution.\n"
showMaybeState (Just state) = showState state


