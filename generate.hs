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

initialSeeds :: Index -> Set.Set Seed
initialSeeds i = Set.fromList $ map (makeSeed i) directions4

inGrid :: State -> Seed -> Bool
inGrid state ((r, c), _) = r >= 0 && r < sRows state && c >= 0 && c < sColumns state

addSquare :: State -> StdGen -> (State, StdGen)
addSquare state gen
  | Set.null (sSeeds state) = (state, gen)
  | otherwise = if Set.null $ Set.intersection (sIn state) (Set.fromList (forward6 i d))
                   then addSquare state { sIn = in', sSeeds = seeds'' } gen
                   else addSquare state { sSeeds = seeds' } gen
      where ((i, d), seeds') = Set.deleteFindMin (sSeeds state)
            newSeeds = Set.fromList $ filter (inGrid state) $ map (makeSeed i) [d, turnLeft d, turnRight d]
            seeds'' = Set.union seeds' newSeeds
            in' = Set.insert i (sIn state)

initialState :: Int -> Int -> Index -> State
initialState r c i = State { sRows = r, sColumns = c, sIn = Set.singleton i
                           , sSeeds = initialSeeds i }