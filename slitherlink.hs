import Control.Monad.Instances
import Data.List (transpose)

data Constraint = Unconstrained | Exactly Int
type Problem = [[Constraint]]

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

showProblem :: Problem -> String
showProblem = unlines . (map . map) showConstraint

readConstraint :: Char -> Either String Constraint
readConstraint '.' = Right Unconstrained
readConstraint '0' = Right $ Exactly 0
readConstraint '1' = Right $ Exactly 1
readConstraint '2' = Right $ Exactly 2
readConstraint '3' = Right $ Exactly 3
readConstraint c = Left $ "Invalid character " ++ show c ++ "."

readProblem ::  String -> Either String Problem
readProblem = (mapM . mapM) readConstraint . lines

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
