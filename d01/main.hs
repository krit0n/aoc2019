module Main where
import Text.Printf (printf)

-- | Fuel requirement for given mass
getFuel :: Int -> Int
getFuel mass = max 0 (mass `div` 3) - 2

-- | Total fuel requirement for given mass.
-- Includes the fuel to support the mass of the fuel itself
getTotalFuel :: Int -> Int
getTotalFuel = sum . takeWhile (> 0) . tail . iterate getFuel

main :: IO()
main = do
  masses <- getContents >>= return . (map read) . lines
  let fuel = sum . map getFuel $ masses
  let totalFuel = sum . map getTotalFuel $ masses
  printf "Fuel: %d\nTotal Fuel: %d\n" fuel totalFuel
