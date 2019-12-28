module Lib where
import qualified Data.Vector.Unboxed as V
import qualified Data.IntSet as S


type Dimensions = (Int, Int)
type Coordinate = (Int, Int)
type Tiles = V.Vector Bool

toCoord :: Dimensions -> Int -> Coordinate
toCoord (dimX, _) x = (x `mod` dimX, x `div` dimX)

toIndex :: Dimensions -> Coordinate -> Int
toIndex (dimX, _) (x,y) = (dimX * y) + x

contains :: Dimensions -> Coordinate -> Bool
contains (dimX,dimY) (x,y) = 0 <= x && 0 <= y && x < dimX && y < dimY

nextTiles :: Dimensions -> Tiles -> Tiles
nextTiles dim tiles = V.generate (V.length tiles) (nextTile dim tiles)

nextTile :: Dimensions -> Tiles -> Int -> Bool
nextTile dim tiles index
  | tiles V.! index = hasOne adjacentBugs
  | otherwise = hasOneOrTwo adjacentBugs
  where
    (a,b) = toCoord dim index
    neighbours = filter (dim `contains`)
      [(a+x,b+y) | x <- [-1,0,1], y <- [-1,0,1], abs x + abs y == 1]
    hasBug coord = tiles V.! toIndex dim coord
    adjacentBugs = filter hasBug neighbours

    hasOne [_] = True
    hasOne _   = False

    hasOneOrTwo [_]   = True
    hasOneOrTwo [_,_] = True
    hasOneOrTwo _     = False

dup :: [Int] -> Maybe (Int, S.IntSet)
dup ls = dup' ls S.empty
  where
    dup' [] _ = Nothing
    dup' (x:xs) s = if S.member x s
      then Just (x, s)
      else dup' xs (S.insert x s)

rating :: Tiles -> Int
rating = rating' . V.toList
  where
    rating' ts = sum [b | (a,b) <- zip ts (iterate (*2) 1), a]

insert :: Int -> a -> [a] -> [a]
insert n y = countdown n
  where
   countdown 0 xs = y:countdown n xs
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs


solution1 :: String -> String
solution1 s = show . dup . map rating . iterate (nextTiles dim) $ initialTiles
  where
    dim = let ls@(l:_) = lines s in (length l, length ls)
    bugOrEmpty = (`elem` ".#")
    isBug = ('#'==)
    initialTiles = V.fromList . map isBug . filter bugOrEmpty $ s

_show :: Dimensions -> Tiles -> String
_show (dimX,_) = _show' . V.toList
  where
    _show' = insert dimX '\n' . map (\x -> if x then '#' else '.')
