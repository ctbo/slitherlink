-- slitherlink.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
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

data SixLines = SixLines { top :: Bool
                         , right :: Bool
                         , bottom :: Bool
                         , left :: Bool
                         , up :: Bool
                         , totheleft :: Bool
                           } deriving (Eq, Show)

countLine :: SixLines -> (SixLines -> Bool) -> Int
countLine x f =  if f x then 1 else 0

countBoxLines :: SixLines -> Int
countBoxLines x = sum $ map (countLine x) [top, right, bottom, left]

countDotLines :: SixLines -> Int
countDotLines x = sum $ map (countLine x) [up, top, left, totheleft]

slListAll :: [SixLines]
slListAll = [x | t <- [False, True]
               , r <- [False, True]
               , b <- [False, True]
               , l <- [False, True]
               , u <- [False, True]
               , tl <- [False, True]
               , let x = SixLines t r b l u tl
               , countDotLines x `elem` [0, 2]
               ]

slListTop :: [SixLines]
slListTop = [SixLines { top = False
                      , right = False
                      , bottom = b
                      , left = False
                      , up = False
                      , totheleft = False
                      } | b <- [False, True]]

slListRight :: [SixLines]
slListRight = filter (not.top)
            $ filter (not.right)
            $ filter (not.bottom) slListAll

slListBottom :: [SixLines]
slListBottom = filter (not.right)
             $ filter (not.bottom)
             $ filter (not.left) slListAll

slListLeft :: [SixLines]
slListLeft =  [SixLines { top = False
                        , right = r
                        , bottom = False
                        , left = False
                        , up = False
                        , totheleft = False
                        } | r <- [False, True]]

type CellState = [SixLines]
type State =  Array (Int, Int) CellState

stateFromProblem :: Problem -> State
stateFromProblem p = array ((-1, -1), (rn+1, cn+1)) cells
    where ((0, 0), (rn, cn)) = bounds p
          cells = [((r, c), xform (p!(r, c))) | r <- [0..rn], c <- [0..cn]]
               ++ [((-1, c), slListTop) | c <- [0 .. cn+1]]
               ++ [((r, cn+1), slListRight) | r <- [0 .. rn+1]]
               ++ [((rn+1, c), slListBottom) | c <- [0 .. cn]]
               ++ [((r, -1), slListLeft) | r <- [-1 .. rn+1]]
          xform Unconstrained = slListAll
          xform (Exactly n) = filter ((== n) . countBoxLines) slListAll

type Direction = (Int, Int)
directions6 :: [Direction] -- right, down, left, up, up&left, down&right
directions6 = [ (0, 1)
             , (1, 0)
             , (0,-1)
             , (-1,0)
             , (-1, -1)
             , (1, 1)
             ]

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

narrow :: Set.Set (Int, Int) -> State -> Maybe State
narrow seed state = if Set.null seed then Just state else
    let (i@(r,c), seed') = Set.deleteFindMin seed in
      if not (inRange (bounds state) i) 
        then narrow seed' state 
        else do
          let sls = state!i
          let sls' = filter (match state (r-1, c-1) [(right, up), (bottom, totheleft)])
                   $ filter (match state (r-1, c  ) [(bottom, top), (left, up)])
                   $ filter (match state (r,   c-1) [(top, totheleft), (right, left)])
                   $ filter (match state (r,   c+1) [(totheleft, top), (left, right)])
                   $ filter (match state (r+1, c  ) [(up, left), (top, bottom)])
                   $ filter (match state (r+1, c+1) [(up, right), (totheleft, bottom)])    
                   sls
          if null sls'
             then Nothing
             else if sls' == sls
                     then narrow seed' state
                     else let newSeeds = Set.fromList $ map (i .+) directions6
                          in narrow (Set.union seed' newSeeds) (state // [(i, sls')])
                 
match :: State -> (Int, Int) -> [(SixLines->Bool, SixLines->Bool)] -> SixLines -> Bool
match state i fps thiscell = (not (inRange (bounds state) i)) || all pairmatch fps
    where pairmatch (otherf, thisf) = any ((==thisf thiscell) . otherf) othercell
          othercell = state!i

narrowAll :: State -> Maybe State
narrowAll state = narrow (Set.fromList (indices state)) state

untilJust :: (b -> Maybe a) -> [b] -> Maybe a
untilJust f = join . find isJust . map f

solve :: Problem -> Maybe State
solve problem = do
  state <- narrowAll $ stateFromProblem problem
  solve' (indices state) state

solve' :: [(Int, Int)] -> State -> Maybe State
solve' [] state = Just state
solve' (i:is) state = untilJust f $ state!i
    where f sl = narrow neighbors (state // [(i, [sl])]) >>= solve' is
          neighbors = Set.fromList $ map (i .+) directions6
--TODO: Check if solution consists of a single loop. Might be more than one.

showSolution :: Problem -> Maybe State -> String
showSolution problem (Just state) = concat $ map twoLines [r0 .. rn]
  where ((r0, c0), (rn, cn)) = bounds state
        twoLines r = unlines [oddLine r, evenLine r]
        oddLine r = concat $ map (oddPair r) [c0 .. cn]
        oddPair r c = dot r c ++ hLine r c
        dot r c = if countDotLines (unwrap r c) == 0 then " " else "+"
        hLine r c = if top (unwrap r c) then "-" else " "
        evenLine r = concat $ map (evenPair r) [c0 .. cn]
        evenPair r c = vLine r c ++ square r c
        vLine r c = if left (unwrap r c) then "|" else " "
        square r c = if inRange (bounds problem) (r, c) then show $ problem!(r, c) else " "
        unwrap r c = head $ state!(r, c)
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
        oneCell r c = superhex $ length $ state ! (r, c)
        superhex x
          | x > 35 = "*"
          | otherwise = ["0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" !! x]  

showMaybeState :: Maybe State -> String
showMaybeState Nothing = "No solution.\n"
showMaybeState (Just state) = showState state
