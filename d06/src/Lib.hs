module Lib
    ( d06_1
    , d06_2
    ) where
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Tree as T

invert :: (Ord k, Ord v) => M.Map k v -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, v) <- M.toList m]

toParentMap :: String -> M.Map String String
toParentMap str = M.fromList
  [(c, p) | ln <- lines str, let [p, c] = splitOn ")" ln]

d06_1 :: String -> String
d06_1 str = show $ sum $ zipWith (*) [0..] (map length . T.levels $ orbits)
  where parents = toParentMap str
        children = invert parents
        orbits = T.unfoldTree (
          \b -> case children M.!? b of
            Nothing -> (b, [])
            Just v -> (b, v)) "COM"

d06_2 :: String -> String
d06_2 str = show $ sum . map length $ [youOnly, sanOnly]
  where
    parents = toParentMap str
    allParents = fmap
      (\x -> case allParents M.!? x of
        Nothing -> [x]
        Just xx -> x:xx)
      parents
    you = allParents M.! "YOU"
    san = allParents M.! "SAN"
    youOnly = filter (`notElem` san) you
    sanOnly = filter (`notElem` you) san
