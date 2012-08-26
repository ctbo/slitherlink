import Data.Array.IArray

import Control.Monad
import Control.Monad.Instances()

import System.Environment

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

allPossibilities :: [FourLines]
allPossibilities = [FourLines t r b l | t <- [False, True]
                                      , r <- [False, True]
                                      , b <- [False, True]
                                      , l <- [False, True]
                                      ]

countLines :: FourLines -> Int
countLines x = count top + count right + count bottom + count left
           where count f = if f x then 1 else 0

possibilitiesForConstraint :: Constraint -> [FourLines]
possibilitiesForConstraint Unconstrained = allPossibilities
possibilitiesForConstraint (Exactly n) = filter ((==n) . countLines) allPossibilities

dotPossibilities = filter (zeroOrTwo.countLines) allPossibilities
    where zeroOrTwo 0 = True
          zeroOrTwo 2 = True
          zeroOrTwo _ = False

data CellState = Line [Bool]
               | Space [FourLines] deriving (Eq, Show)
type State =  Array (Int, Int) CellState


stateFromProblem :: Problem -> State
stateFromProblem p = array ((0, 0), (rows, columns)) constraints
  where ((0, 0), (rn, cn)) = bounds p
        rows    = 2*rn + 2
        columns = 2*cn + 2
        constraints = [((r, c), Space dotPossibilities) | r <- [2, 4 .. 2*rn], c <- [2, 4 .. 2*cn]]
         ++ [((r, 0), Space (filter (not.left) dotPossibilities) )| r <- [2, 4 .. 2*rn]]
         ++ [((r, 2*cn+2), Space (filter (not.right) dotPossibilities)) | r <- [2, 4 .. 2*rn]]
         ++ [((0, c), Space (filter (not.top) dotPossibilities)) | c <- [2, 4 .. 2*cn]]
         ++ [((2*rn+2, c), Space (filter (not.bottom) dotPossibilities)) | c <- [2, 4 .. 2*cn]]
         ++ [((0, 0), Space (filter (not.top) $ filter (not.left) dotPossibilities))
            ,((0, 2*cn+2), Space (filter (not.top) $ filter (not.right) dotPossibilities))
            ,((2*rn+2, 0), Space (filter (not.bottom) $ filter (not.left) dotPossibilities))
            ,((2*rn+2, 2*cn+2), Space (filter (not.bottom) $ filter (not.right) dotPossibilities))]
         ++ [((r, c), Line [False, True]) | r <- [0, 2 .. 2*rn+2], c <- [1, 3 .. 2*cn+1]]
         ++ [((r, c), Line [False, True]) | r <- [1, 3 .. 2*rn+1], c <- [0, 2 .. 2*cn+2]]
         ++ [((2*r+1, 2*c+1), Space (possibilitiesForConstraint (p!(r, c))))| r <- [0 .. rn], c <- [0 .. cn]]

narrow :: (Int, Int) -> State -> Maybe State
narrow i@(r,c) state = if not (inRange (bounds state) i) then Just state else
    case state!i of
      Line ls -> do
        let ls' = filter (match (r-1,c) state bottom)
                $ filter (match (r, c+1) state left)
                $ filter (match (r+1, c) state top)
                $ filter (match (r, c-1) state right) ls
        if null ls' 
          then Nothing 
          else if ls' == ls 
            then Just state 
            else Just (state // [(i, Line ls')]) >>= narrow (r-1, c)
                                                 >>= narrow (r, c+1)
                                                 >>= narrow (r+1, c)
                                                 >>= narrow (r, c-1)


match :: (Int, Int) -> State -> (FourLines -> Bool) -> Bool -> Bool
match i state f x = (not (inRange (bounds state) i)) 
                || check (state!i)
    where check (Space xs) = any ((==x).f) xs


sampleProblemString :: String
sampleProblemString = unlines [".22.."
                              ,"..13."
                              ,"313.2"
                              ,"....."
                              ,".2.23"
                              ]

sampleProblem :: Problem
sampleProblem = case readProblem sampleProblemString of 
  Right x -> x
  Left _ -> undefined -- can't happen



{-
showState :: State -> String
showState state = unlines $ map oneLine [r0 .. rn]
  where ((r0, c0), (rn, cn)) = bounds state
        oneLine r = concat $ map (oneCell r) [c0 .. cn]
        oneCell r c = showCell (isVertical r) $ state ! (r, c)
        isVertical = odd
        showCell vertical s = case s of
          Dot x  -> if x then "+" else " "
          Line x -> if x then (if vertical then "|" else "-") else " "
          Box x _ -> show x

showMaybeState :: Maybe State -> String
showMaybeState Nothing = "No solution.\n"
showMaybeState (Just state) = showState state

match :: (Int, Int) -> CellState -> State -> Bool
match i x state = inRange (bounds state) i && state!i == x
      
decrement :: (Int, Int) -> State -> Maybe State
decrement i state = 
  if not (inRange (bounds state) i) then Just state else
  case state ! i of
     Box Unconstrained _ -> Just state
     Box (Exactly 0) x   -> Nothing
     Box (Exactly n) x   -> Just (state // [(i, Box (Exactly (n-1)) x)])
     _                   -> Nothing

fixLine :: (Int, Int) -> (FourLines -> Bool) -> State -> Maybe State
fixLine i f state =
  if not (inRange (bounds state) i) 
     then Just state 
     else case state ! i of
       Box x p -> if null p' then Nothing else Just (state // [(i, Box x p')])
                  where p' = filter f p
       _ -> error "this can't happen"

type Direction = (Int, Int)
directions :: [Direction]
directions = [ (0, 1)
             , (1, 0)
             , (0,-1)
             , (-1,0)
             ]
turnRight :: (Int, Int) -> (Int, Int)
turnRight (r, c) = (-c, r)

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (r, c) = (c, -r)

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)


move :: (Int, Int) -> Direction -> State -> Maybe State
move pos dir state = do
          let viaLine = pos .+ dir
          let toDot = pos .+ dir .+ dir
          unless (inRange (bounds state) toDot) Nothing
          unless ((state ! toDot) == Dot False) Nothing
          state'  <- decrement (viaLine .+ turnLeft dir) state
          state'' <- decrement (viaLine .+ turnRight dir) state'
          return (state'' // [(toDot, Dot True), (viaLine, Line True)])

onlyZeros :: State -> Bool
onlyZeros state = all ok (elems state)
  where ok (Box (Exactly 0) _) = True
        ok (Box (Exactly _) _) = False
        ok _                 = True

solve :: Problem -> Maybe State
solve problem = solve' (0,0) (0,0) (stateFromProblem problem)
  where solve' goal pos state = foldl f Nothing directions
           where f solution dir = case solution of
                    Just x -> Just x
                    Nothing -> do
                      state' <- move pos dir state
                      let newPos = pos .+ dir .+ dir
                      if newPos == goal
                        then if onlyZeros state'
                               then return state'
                               else Nothing
                        else solve' goal (newPos) state'

main :: IO ()
main = do
     [filename] <- getArgs
     pString <- readFile filename
     case readProblem pString of
       Left e -> putStrLn e
       Right p -> putStrLn $ showMaybeState $ solve p

-}
