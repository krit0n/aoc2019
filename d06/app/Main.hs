module Main where
import Lib

main :: IO ()
main = interact (\str -> (d06_1 str) ++ "\n" ++ (d06_2 str) ++ "\n")
