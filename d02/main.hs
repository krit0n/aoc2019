module Main where
import Data.Sequence (Seq, fromList, index, update)

type Cursor = Int
type Memory = Seq Int
type State = (Cursor, Memory)

operator :: Int -> Maybe (Int -> Int -> Int)
operator 1  = Just (+)
operator 2  = Just (*)
operator 99 = Nothing
operator _  = error "Invalid state"

step :: State -> Maybe State
step (c,m) = do
  let [op, ptr1, ptr2, ptr3] = map (index m) [c .. c+3]
  let [val1, val2] = map (index m) [ptr1, ptr2]
  operation <- operator op
  return (c + 4, update ptr3 (operation val1 val2) m)

run :: State -> [State]
run initial = iterateMaybe step (Just initial)

iterateMaybe :: (a -> Maybe a) -> Maybe a -> [a]
iterateMaybe _ Nothing = []
iterateMaybe f (Just a) = a : iterateMaybe f (f a)


part1 :: Memory -> Int -> Int -> Int
part1 memory noun verb = index lastState 0
  where
    memory' = update 1 noun . update 2 verb $ memory
    (_, lastState) = last $ run (0, memory')

part2 :: Memory -> [(Int, Int, Int)]
part2 memory = [(noun, verb, 100*noun + verb) |
                noun <- [0..99],
                verb <- [0..99],
                part1 memory noun verb == 19690720]

main :: IO()
main = do
  contents <- getContents
  let memory = fromList $ read ("[" ++ contents ++ "]") :: Seq Int
  print $ part1 memory 12 2
  print $ part2 memory
