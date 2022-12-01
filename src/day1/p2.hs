import Data.List.Split
import Data.List

main = do
  contents <- readFile "input.txt"
  let elvesRaw = splitOn "\n\n" contents
  let elves = map (\x -> map read (splitOn "\n" x)) elvesRaw
  let result = sum (take 3 (rSort (map sum elves)))
  print result

rSort = reverse . sort