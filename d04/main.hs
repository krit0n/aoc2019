module Main where
import Data.List (group)

solution1 = length [x |
    e5 <- [0..9]
  , e4 <- [e5..9]
  , e3 <- [e4..9]
  , e2 <- [e3..9]
  , e1 <- [e2..9]
  , e0 <- [e1..9]
  , (any (>= 2)) . (map length) . group $ [e5,e4,e3,e2,e1,e0]
  ,  let x = 100000*e5 + 10000*e4 + 1000*e3 + 100*e2 + 10*e1 + e0
  , 278384 <= x, x <= 824795]

solution2 = length [x |
    e5 <- [0..9]
  , e4 <- [e5..9]
  , e3 <- [e4..9]
  , e2 <- [e3..9]
  , e1 <- [e2..9]
  , e0 <- [e1..9]
  , (elem 2) . (map length) . group $ [e5,e4,e3,e2,e1,e0]
  ,  let x = 100000*e5 + 10000*e4 + 1000*e3 + 100*e2 + 10*e1 + e0
  , 278384 <= x, x <= 824795]

main :: IO()
main = do
 putStrLn $ "solution1: " ++ show solution1
 putStrLn $ "sorution2: " ++ show solution2