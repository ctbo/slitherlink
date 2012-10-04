import qualified Data.Set as Set
import System.Random

type Index = (Int, Int)
type Direction = (Int, Int)
type Seed = (Index, Direction)
data State = State { sRows :: Int
                   , sColumns :: Int
                   , sIn :: Set.Set Index
                   , sSeeds :: Set.Set Seed } deriving Show

directions4 :: [Direction] -- right, down, left, up
directions4 = [(0, 1), (1, 0), (0, -1), (-1, 0)]

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a+c, b+d)

turnLeft (a, b) = (-b, a)
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

addSquare :: State -> StdGen -> (State, StdGen)
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

initialState :: Int -> Int -> StdGen -> (State, StdGen)
initialState nr nc gen = ( State { sRows = nr, sColumns = nc, sIn = Set.singleton i
                                 , sSeeds = initialSeeds }
                         , gen'' )
  where (r, gen') = randomR (0, nr - 1) gen
        (c, gen'') = randomR (0, nc - 1) gen'
        i = (r, c)
        initialSeeds = Set.fromList $ filter (inGrid nr nc) $ map (makeSeed i) directions4

showState :: State -> String
showState state = concatMap showLine [0 .. sRows state - 1]
    where showLine r = concatMap (showCell r) [0 .. sColumns state - 1] ++ "\n"
          showCell r c = if (r, c) `Set.member` sIn state then "X" else " "

generate :: Int -> Int -> StdGen -> (State, StdGen)
generate nr nc gen = addSquare init gen'
    where (init, gen') = initialState nr nc gen

showStateN :: State -> String
showStateN state = concatMap showLine [0 .. sRows state - 1]
    where showLine r = concatMap (showCell r) [0 .. sColumns state - 1] ++ "\n"
          showCell r c = show $ countLines (r, c)
          countLines i = let this = i `Set.member` sIn state
                         in length $ filter (\d -> this /= i.+d `Set.member` sIn state) directions4
