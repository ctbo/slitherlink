import Data.Array.IArray

import Control.Monad
import Control.Monad.Instances
import Data.List (transpose)

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

data Constraint = Unconstrained | Exactly Int

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
                   } deriving (Show)

stateFromProblem :: Problem -> State
stateFromProblem p = State {constraints = p,
                            dots = emptyDots (snd (bounds p) .+ (1, 1))}

moveDelta :: State -> (Int, Int) -> (Int, Int) -> Maybe State
moveDelta state from delta = do
          let to = from .+ delta
          unless (inRange (bounds (dots state)) to) Nothing
          when (dots state ! to) Nothing
          return (State {constraints = constraints state,
                         dots = (dots state) // [(to, True)]})

