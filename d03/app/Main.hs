module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Lib
  (solve1
  , solve2)

main :: IO ()
main = do
  input <- getArgs >>= parse
  putStrLn $ solve1 input
  putStrLn $ solve2 input

parse :: [String] -> IO String
parse [] = getContents
parse ['-':_] = die "Invalid parameters"
parse [filename] = readFile filename
parse _ = die "Invalid arguments"
