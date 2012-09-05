-- slitherlink.hs 
-- a solver for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Data.Array.IArray
import Control.Monad
import Control.Monad.Instances()
import System.Environment

import Data.Bits hiding (popCount) -- popCount is buggy, rolling my own

popCount :: Integer -> Int
popCount = go 0
    where
        go c 0 = c
        go c w = go (c+1) (w .&. (w - 1))

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

data Pattern = Pattern { pMask :: Integer
                       , pVal :: [Integer]} deriving (Eq, Show)
data State = State { sMask :: Integer
                   , sVal :: Integer
                   , loops :: [Integer]} deriving (Eq, Show)

allValsFromMask :: Integer -> [Integer]
allValsFromMask 0 = [0]
allValsFromMask x = do
    y <- allValsFromMask (x .&. (x-1))
    [y, y .|. (x .&. (-x))]


patternsFromProblem :: Problem -> [Pattern]
patternsFromProblem p = concatMap dotSquare [(r, c) | r <- [0..rn+1], c <- [0..cn+1]]
    where ((0, 0), (rn, cn)) = bounds p
          bitsPerRow = 3*(cn+1) + 2
          bitnoForSquare (r, c) = r * bitsPerRow + c * 3
          dotSquare (i@(r, c)) = if r <= rn && c <= cn
                                   then dot i ++ square (i, p!i)
                                   else dot i
          square (_, Unconstrained) = []
          square (i, Exactly n) = [ Pattern { pMask = mask
                                            , pVal  = filter ((==n) . popCount) $ allValsFromMask mask
                                            } ]
              where b = bitnoForSquare i
                    b1 = b + bitsPerRow
                    mask = bit (b+1) .|. bit (b+2) .|. bit (b+4) .|. bit (b1+2)
          dot i@(r, c) = [ Pattern { pMask = dotMask
                                   , pVal  =  0 : map (.|. bit b) twoLines
                                   } ]
              where b = bitnoForSquare i
                    dotMask = dotUp .|. dotRight .|. dotDown .|. dotLeft
                    dotUp = if r > 0 then bit (b - bitsPerRow + 1) else 0
                    dotRight = if c <= cn then bit (b + 2) else 0
                    dotDown = if r <= rn then bit (b + 1) else 0
                    dotLeft = if c > 0 then bit (b - 1) else 0
                    twoLines = filter ((==2) . popCount) $ allValsFromMask dotMask

{-
main :: IO ()
main = getArgs >>= f >>= g where
    f [filename] = readFile filename
    f []         = return sampleProblemString
    f _          = error "Too many arguments."
    g pString = case readProblem pString of
      Left e -> putStrLn e
      Right p -> putStrLn $ showSolution p $ solve p
-}

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


