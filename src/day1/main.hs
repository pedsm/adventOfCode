import Data.List.Split
import Data.List

main = do
  contents <- readFile "input.txt"
  -- let lines = split
  let elvesRaw = splitOn "\n\n" contents
  let elves = map (\x -> map read (splitOn "\n" x)) elvesRaw
  let result = listMax (map sum elves)
  print result

listMax :: [Int] -> Int
listMax (x:y:xs) = if length xs == 0
  then max x y
  else listMax ((max x y) : xs)
