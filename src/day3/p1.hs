import Data.List
import Data.List.Split

main = do
  contents <- readFile "input.txt"
  let rucksacks = splitOn "\n" contents
  let result = sum (map solve rucksacks)
  print result


splitMiddle :: [Char] -> ([Char], [Char])
splitMiddle xs = splitAt ((length xs) `div` 2) xs

findCommon ((x:xs), ys) = if any (x==) ys then x
  else findCommon (xs, ys)

charToAscii :: Char -> Int
charToAscii x = if fromEnum x > 96 then (fromEnum x) - 96
  else (fromEnum x) - 64 + 26

solve = charToAscii . findCommon . splitMiddle