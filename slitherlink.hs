import Control.Monad.Instances

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

readConstraint :: Char -> Either String Constraint
readConstraint '.' = Right Unconstrained
readConstraint '0' = Right $ Exactly 0
readConstraint '1' = Right $ Exactly 1
readConstraint '2' = Right $ Exactly 2
readConstraint '3' = Right $ Exactly 3
readConstraint c = Left $ "Invalid character " ++ show c ++ "."

readProblem ::  String -> Either String Problem
readProblem = (mapM . mapM) readConstraint . lines
