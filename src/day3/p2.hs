import Data.List
import Data.List.Split

main = do
  contents <- readFile "input.txt"
  let rucksacks = splitOn "\n" contents
  let chunks = chunksOf 3 rucksacks
  let solution = sum (map (charToAscii. findCommon3 . tuplify3) chunks)
  print solution


splitMiddle :: [Char] -> ([Char], [Char])
splitMiddle xs = splitAt ((length xs) `div` 2) xs

findCommon3 ((x:xs), ys, zs) = if and[any (x==) ys, any (x==) zs] then x
  else findCommon3 (xs, ys, zs)

charToAscii :: Char -> Int
charToAscii x = if fromEnum x > 96 then (fromEnum x) - 96
  else (fromEnum x) - 64 + 26

tuplify3 [x,y,z] = (x,y,z)
