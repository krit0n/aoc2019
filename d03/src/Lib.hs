module Lib
    ( solve1
    , solve2)
where
import Data.List.Split (splitOn)
import Control.Monad.State (State, state, runState, foldM)
import qualified Data.Map.Strict as M
import Data.List.Safe (minimumBy)

-- | Directions
-- U = Up, R = Right, D = Down, L = Left
data Dir = U | R | D | L deriving (Read, Show)

-- | Cartesian coordinates
type Point = (Int, Int)

-- | A @Point@ on a path with the distance from
-- the paths origin to the point
data Pathpoint = Pathpoint { getPoint :: Point
                           , getDistance :: Int
                           } deriving (Show)

-- | State that holds the covered path to pathpoint
type PathState = State [Pathpoint] Pathpoint

-- | The origin of the coordinate system
origin :: Point
origin = (0,0)

-- | Move from pathpoint in direction dir
(|>) :: Pathpoint -> Dir -> PathState
ppoint |> dir = state $ \pps -> (ppoint', ppoint':pps)
  where
    ppoint' = ppoint ||> dir
    Pathpoint (x,y) d ||> U = Pathpoint (x,y+1) (d+1)
    Pathpoint (x,y) d ||> R = Pathpoint (x+1,y) (d+1)
    Pathpoint (x,y) d ||> D = Pathpoint (x,y-1) (d+1)
    Pathpoint (x,y) d ||> L = Pathpoint (x-1,y) (d+1)

-- | Move d times from pathpoint in direction dir
(|>>) :: Pathpoint -> (Dir, Int) -> PathState
ppoint |>> (dir, d) = foldM doStep ppoint (replicate d (|> dir))
  where
    doStep ppoint' step = step ppoint'

walkPath :: Pathpoint -> [(Dir, Int)] -> [Pathpoint]
walkPath start path = snd $ runState (walkPath' start path) []
  where
    walkPath' pp (path':ps) = do
      pp' <- pp |>> path'
      walkPath' pp' ps
    walkPath' pp [] = return pp

manhatten :: Point -> Point -> Int
manhatten (x1,y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

manhattenOrd :: (Int, Int) -> (Int, Int) -> Ordering
manhattenOrd p1 p2 = compare (mDist p1) (mDist p2)
  where mDist = manhatten origin

solve1 :: String -> String
solve1 input = show $ (minimumBy manhattenOrd intersections :: Maybe Point)
  where
    path1:path2:_ = map _toPath $ splitOn "\n" input
    origin' = Pathpoint origin 0
    map1 = _toMap $ walkPath origin' path1
    map2 = _toMap $ walkPath origin' path2
    intersections = M.keys $ M.intersection map1 map2

solve2 :: String -> String
solve2 input = show $ (minimumBy compareSteps (M.toList intersections) :: Maybe (Point, Int))
  where
    path1:path2:_ = map _toPath $ splitOn "\n" input
    origin' = Pathpoint origin 0
    map1 = _toMap $ walkPath origin' path1
    map2 = _toMap $ walkPath origin' path2
    intersections = M.intersectionWith (+) map1 map2
    compareSteps (_, s1) (_, s2) = compare s1 s2

_toPath :: String -> [(Dir, Int)]
_toPath s = map toPath' $ splitOn "," s
  where
    toPath' (d:ns) = (read [d], read ns)
    toPath' _ = undefined

_toMap :: [Pathpoint] -> M.Map (Int, Int) Int
_toMap pps = M.fromList $ map (\pp -> (getPoint pp, getDistance pp)) pps
