import Data.List

main = do
  contents <- readFile "input.txt"
  let solution = solve contents
  print solution

solve xs = solve' xs 0

solve' xs i = if length(nub (take 4 xs)) == 4 
  then i + 4
  else solve' (drop 1 xs) i+1