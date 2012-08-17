import Data.Array.IArray

import Control.Monad
import Control.Monad.Instances
import Data.List (transpose)

data Constraint = Unconstrained | Exactly Int

sampleProblemString = unlines [".22.."
                              ,"..13."
                              ,"313.2"
                              ,"...."
                              ,".2.23"
                              ]

showConstraint :: Constraint -> Char
showConstraint Unconstrained = '.'
showConstraint (Exactly x) = head $ show x

instance Show Constraint where
    show = (:[]) . showConstraint         

showProblem :: ProblemList -> String
showProblem = unlines . (map . map) showConstraint

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
            return $ listArray ((1, 1), (rows, columns)) $ concat pl 


data LineState = Line | NoLine | Unknown deriving (Eq, Ord, Enum, Show, Bounded)

type State = ([[LineState]]  -- N lists of N+1 vertical lines, left to right
             ,[[LineState]]) -- N lists of N+1 horizontal lines, top to bottom

unknownState :: Int -> State
unknownState n = (unknown, unknown)
    where unknown = replicate (n+1) $ replicate (n+1) Unknown

sampleState :: State
sampleState = (
                [ [Unknown,Unknown,Unknown,Unknown,Unknown,Unknown]
                , [Unknown,Unknown,Unknown,Unknown,Line   ,Unknown]
                , [Unknown,Unknown,Line   ,Unknown,Unknown,Unknown]
                , [Unknown,Unknown,Unknown,Unknown,Unknown,Unknown]
                , [Unknown,Unknown,Unknown,Unknown,Unknown,Line   ]
                ]
                ,
                [ [Unknown,Unknown,Unknown,Unknown,Unknown,Unknown]
                , [Unknown,Unknown,Unknown,Unknown,Unknown,Unknown]
                , [Unknown,Unknown,Unknown,Line   ,Unknown,Unknown]
                , [Unknown,Line   ,Unknown,Unknown,Unknown,Unknown]
                , [Unknown,Unknown,Unknown,Unknown,Unknown,Line   ]
                ]
              )

neighborCount :: State -> [[Int]]
neighborCount (vertical, horizontal) = matrixAdd vCount hCount
    where vCount = count vertical
          hCount = transpose $ count horizontal
          count sss = map countLine sss
          countLine ss = zipWith countField ss $ tail ss
          countField l r = countState l + countState r
          countState i = if i == Line then 1 else 0

matrixAdd :: [[Int]] -> [[Int]] -> [[Int]]
matrixAdd = (zipWith . zipWith) (+)
