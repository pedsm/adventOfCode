import Data.List

main = do
  contents <- readFile "input.txt"
  let solution = solve contents
  print solution

solve xs = solve' xs 0

solve' xs i = if length(nub (take 14 xs)) == 14 
  then i + 14
  else solve' (drop 1 xs) i+1