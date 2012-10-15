-- generate.hs 
-- a generator for Slitherlink puzzles
-- Copyright (C) 2012 by Harald Bögeholz
-- See LICENSE file for license information

import Slitherlink
import qualified Data.Set as Set
import System.Environment
import System.Random
import Data.List (nub)
import Data.Array.IArray
import Debug.Trace


type Index = (Int, Int)
type Direction = (Int, Int)
type Seed = (Index, Direction)
data Inside = Inside { sRows :: Int
                     , sColumns :: Int
                     , sIn :: Set.Set Index
                     , sSeeds :: Set.Set Seed } deriving Show

directions4 :: [Direction] -- right, down, left, up
directions4 = [(0, 1), (1, 0), (0, -1), (-1, 0)]

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (a, b) = (-b, a)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (a, b) = (b, -a)

forward6 :: Index -> Direction -> [Index]
forward6 i d = [ i,      i .+ turnLeft d,      i .+ turnRight d
               , i .+ d, i .+ turnLeft d .+ d, i .+ turnRight d .+ d]

makeSeed :: Index -> Direction -> Seed
makeSeed i d = (i .+ d, d)

inGrid :: Int -> Int -> Seed -> Bool
inGrid nr nc ((r, c), _) = r >= 0 && r < nr && c >= 0 && c < nc

deletePickRandom :: Ord a => Set.Set a -> StdGen -> (a, Set.Set a, StdGen)
deletePickRandom s gen = (e, set', gen')
    where l = Set.toList s
          (r, gen') = randomR (0, length l - 1) gen
          e = l !! r
          set' = Set.delete e s

addSquare :: Inside -> StdGen -> (Inside, StdGen)
addSquare state gen
  | Set.null (sSeeds state) = (state, gen)
  | otherwise = if Set.null $ Set.intersection (sIn state) (Set.fromList (forward6 i d))
                   then addSquare state { sIn = in', sSeeds = seeds'' } gen'
                   else addSquare state { sSeeds = seeds' } gen'
      where ((i, d), seeds', gen') = deletePickRandom (sSeeds state) gen
            newSeeds = Set.fromList $ filter (inGrid (sRows state) (sColumns state)) 
                                    $ map (makeSeed i) [d, turnLeft d, turnRight d]
            seeds'' = Set.union seeds' newSeeds
            in' = Set.insert i (sIn state)

initialInside :: Int -> Int -> StdGen -> (Inside, StdGen)
initialInside nr nc gen = ( Inside { sRows = nr, sColumns = nc, sIn = Set.singleton i
                                 , sSeeds = initialSeeds }
                         , gen'' )
  where (r, gen') = randomR (0, nr - 1) gen
        (c, gen'') = randomR (0, nc - 1) gen'
        i = (r, c)
        initialSeeds = Set.fromList $ filter (inGrid nr nc) $ map (makeSeed i) directions4

{-
showInside :: Inside -> String
showInside state = concatMap showLine [0 .. sRows state - 1]
    where showLine r = concatMap (showCell r) [0 .. sColumns state - 1] ++ "\n"
          showCell r c = if (r, c) `Set.member` sIn state then "X" else " "

showInsideN :: Inside -> String
showInsideN state = concatMap showLine [0 .. sRows state - 1]
    where showLine r = concatMap (showCell r) [0 .. sColumns state - 1] ++ "\n"
          showCell r c = show $ countLines (r, c)
          countLines i = let this = i `Set.member` sIn state
                         in length $ filter (\d -> this /= i.+d `Set.member` sIn state) directions4
-}

randomPermutation :: [a] -> StdGen -> [a]
randomPermutation xs gen = map (xs!!) $ take l $ nub $ randomRs (0, l-1) gen
    where l = length xs

grid :: Int -> Int -> [Index]
grid nr nc = [(r, c) | r <- [0 .. nr-1], c <- [0 .. nc-1]]

generate :: Int -> Int -> StdGen -> Problem
generate nr nc gen = foldl remove problem $ zip perm [length perm, length perm - 1 .. 1]
    where (initial, gen') = initialInside nr nc gen
          (inside, gen'') = addSquare initial gen'
          perm = randomPermutation (grid nr nc) gen''
          problem = makeProblem inside
          remove p (i, t) = let p' = p // [(i, Unconstrained)]
                            in if uniqueSolution p'
                                  then trace (showProblem p' ++ "+ " ++ show t) p'
                                  else trace ("- " ++ show t) p
          uniqueSolution p = 1 == (length $ take 2 $ solve p)

makeProblem :: Inside -> Problem
makeProblem ins = array ((0, 0), (sRows ins - 1, sColumns ins - 1)) 
                $ map constraint $ grid (sRows ins) (sColumns ins)
    where constraint i = (i, Exactly (countLines i))
          countLines i = let this = i `Set.member` sIn ins
                         in length $ filter (\d -> this /= i.+d `Set.member` sIn ins) directions4

showProblem :: Problem -> String
showProblem p = unlines $ map oneLine [0 .. rn]
    where ((0, 0), (rn, cn)) = bounds p
          oneLine r = concatMap (\c -> show (p!(r, c))) [0 .. cn]

showProblemEPS :: Problem -> String
showProblemEPS p = epsStr1 ++ show ((cn+1)*22+4) ++ " " ++ show ((rn+1)*22+4) ++ "\n"
                ++ "/nr " ++ show (rn+1) ++ " def\n"
                ++ "/nc " ++ show (cn+1) ++ " def\n"
                ++ epsStr2
                ++ constraints
    where ((0, 0), (rn, cn)) = bounds p
          constraints = concatMap constraint $ assocs p
          constraint (_, Unconstrained) = ""
          constraint ((r, c), Exactly n) = "(" ++ show n ++ ") " ++ show r ++ " " ++ show c ++ " constraint\n"

main :: IO ()
main = do
     args <- getArgs
     gen <- newStdGen
     case args of
          [nr, nc] -> do
               let problem = generate (read nr) (read nc) gen
               putStr $ showProblem problem
          [nr, nc, filename] -> do
               let problem = generate (read nr) (read nc) gen
               putStr $ showProblem problem
               writeFile filename $ showProblemEPS problem               
          _ -> error "usage: generate rows columns [filename.eps]"


epsStr1 :: String
epsStr1 =
    "%!PS-Adobe-3.0 EPSF-3.0\n\
    \%%BoundingBox: 0 0 "

epsStr2 :: String
epsStr2 =
    "/w 22 def\n\
    \1 setlinewidth\n\
    \.7 setgray\n\
    \2 2 translate\n\
    \0 0 moveto\n\
    \nc w mul 0 lineto\n\
    \nc w mul nr w mul lineto\n\
    \0 nr w mul lineto\n\
    \closepath\n\
    \stroke\n\
    \1 1 nc 1 sub % for\n\
    \{\n\
    \    w mul 0 moveto\n\
    \    0 nr w mul rlineto\n\
    \    stroke\n\
    \} for\n\
    \1 1 nr 1 sub % for\n\
    \{\n\
    \    w mul 0 exch moveto\n\
    \    nc w mul 0 rlineto\n\
    \    stroke\n\
    \} for\n\
    \0 1 nr % for\n\
    \{\n\
    \    0 1 nc % for\n\
    \    {\n\
    \        w mul 1 index w mul 1.5 0 360 arc closepath fill\n\
    \    } for\n\
    \    pop\n\
    \} for\n\
    \/Helvetica findfont 12 scalefont setfont\n\
    \0 setgray\n\
    \/constraint\n\
    \{\n\
    \    w mul 8 add exch nr exch sub 1 sub w mul 6.5 add moveto show\n\
    \} def\n"
