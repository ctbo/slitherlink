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
                   } deriving (Show)

stateFromProblem :: Problem -> State
stateFromProblem p = State {constraints = p
                           ,dots = emptyDots (snd (bounds p) .+ (1, 1))
                           ,position = (0, 0)
                           }

notZero :: Constraint -> Bool
notZero c = c /= Exactly 0

decrementConstraint :: Constraint -> Constraint
decrementConstraint Unconstrained = Unconstrained
decrementConstraint (Exactly x) = Exactly (x - 1)

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
          let ll = lookLeft dir
          let lr = lookRight dir
          unless (notZero (constraints state ! lr)) Nothing
--          unless (notZero (constraints state ! ll)) Nothing
          return (State {constraints = constraints state
                        ,dots = (dots state) // [(to, True)]
                        ,position = to
                        })

