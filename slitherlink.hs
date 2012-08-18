import Data.Array.IArray

import Control.Monad
import Control.Monad.Instances
import Data.List (transpose)

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

data Constraint = Unconstrained | Exactly Int deriving (Eq)

sampleProblemString = unlines [".22.."
                              ,"..13."
                              ,"313.2"
                              ,"....."
                              ,".2.23"
                              ]

showConstraint :: Constraint -> Char
showConstraint Unconstrained = '.'
showConstraint (Exactly x) = head $ show x

instance Show Constraint where
    show = (:[]) . showConstraint         

showProblemList :: ProblemList -> String
showProblemList = unlines . (map . map) showConstraint

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

sampleProblem = case readProblem sampleProblemString of Right x -> x

type Dots = Array (Int, Int) Bool
emptyDots :: (Int, Int) -> Dots
emptyDots p = listArray ((0, 0), p) (repeat False)

data State = State { constraints :: Problem
                   , dots :: Dots
                   , position :: (Int, Int)
                   , goal :: (Int, Int)
                   } deriving (Show)

showState :: State -> String
showState state = unlines $ map line [r0 .. rn] ++ [hLine (rn+1)]
  where ((r0, c0), (rn, cn)) = bounds $ constraints state
        line r = hLine r ++ "\n" ++ vLine r
        hLine r = concat $ map (hCell r) [c0 .. cn] ++ [dot r (cn+1)]
        hCell r c = (dot r c) ++
                    (if (dots state ! (r, c)) && (dots state ! (r, c+1)) then "-" else " ")
        dot r c = if dots state ! (r, c) then "+" else " "
        vLine r = concat $ map (vCell r) [c0 .. cn]
        vCell r c = (if (dots state ! (r, c)) && (dots state ! (r+1, c)) then "|" else " ") ++
                    (show $ constraints state ! (r, c))

showMaybeState :: Maybe State -> String
showMaybeState Nothing = "Nothing\n"
showMaybeState (Just state) = showState state

stateFromProblem :: Problem -> (Int, Int) -> State
stateFromProblem c p = State { constraints = c
                             , dots = emptyDots (snd (bounds c) .+ (1, 1))
                             , position = p
                             , goal = p
                             }

decrement :: (Int, Int) -> State -> Maybe State
decrement i state = 
  if not (inRange (bounds (constraints state)) i) then Just state else
  case constraints state ! i of
     Unconstrained -> Just state
     Exactly 0 -> Nothing
     Exactly n -> Just State { constraints = (constraints state) // [(i, Exactly (n-1))]
                             , dots = dots state
                             , position = position state
                             , goal = goal state
                             }

data Direction = Direction { delta, lookRight, lookLeft :: (Int, Int) }
directions :: [Direction]
directions = [ Direction (0, 1) (0, 0)  (-1, 0)
             , Direction (1, 0) (0, -1) (0, 0)
             , Direction (0,-1) (-1,-1) (0, -1)
             , Direction (-1,0) (-1, 0) (-1,-1)
             ]

moveDirection :: Direction -> State -> Maybe State
moveDirection dir state = do
          let to = position state .+ delta dir
          unless (inRange (bounds (dots state)) to) Nothing
          when (dots state ! to) Nothing
          state' <- decrement (position state .+ lookLeft dir) state
          state'' <- decrement (position state' .+ lookRight dir) state'
          return (State { constraints = constraints state''
                        , dots = (dots state'') // [(to, True)]
                        , position = to
                        , goal = goal state''
                        })

onlyZeros :: Problem -> Bool
onlyZeros p = all (\x -> x == Unconstrained || x == Exactly 0) (elems p)
-- onlyZeros _ = True

solve :: State -> Maybe State
solve state = foldl f Nothing directions
   where f solution dir = case solution of
            Just x -> Just x
            Nothing -> do
              state' <- moveDirection dir state
              if goal state' == position state'
                then if onlyZeros (constraints state')
                       then return state'
                       else Nothing
                else solve state'
main = do
     putStrLn $ show $ solve $ stateFromProblem sampleProblem (4,4)
