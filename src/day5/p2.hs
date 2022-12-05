import Data.List.Split
import Data.List


main = do
  contents <- readFile "input.txt"
  print contents
  let parts = splitOn "\n\n" contents
  let stacks = head parts
  let lines = scrapLastLine (splitOn "\n" stacks)
  let stackGrid = map (\x -> map readChar (chunksOf 4 x)) lines
  let stacksRaw = transpose stackGrid
  -- print stacksRaw
  let stacks = map (\xs -> filter (' '/=) xs) stacksRaw
  print stacks
  let instructionsRaw = head (tail parts)
  let instructions = splitOn "\n" instructionsRaw
  let parsedInstructions = (map parseInstruction instructions)
  print parsedInstructions
  print parsedInstructions
  let solution = solve stacks parsedInstructions
  print solution
  let answer = map head solution
  print answer

solve stacks moves = if length(moves) == 0 then stacks
  else solve' stacks moves

solve' stacks ((x,y,z):ms) = solve (move (stacks,x,y,z)) ms

parseInstruction string = tuplify3 (map 
  (\(_,x) -> read x :: Int) 
  (filter (\(i,s) -> i `mod` 2 == 0) (zip [1..] (splitOn " " string))))

flattenInstructions (x,y,z) = take x (repeat (y,z))

tuplify3 (x:y:z:_) = (x,y,z)

scrapLastLine xs = take (length xs  - 1) xs

readChar (_:x:_) = x

-- move :: ([[Char]], Int, Int)
move (stacks, amount, from, to) = map 
  (\(i,xs) -> 
    if i == from then drop amount xs
    else if i == to then (take amount (head (getIndex from stacks))) ++ xs
    else xs 
  ) 
  (indexWrap stacks)


indexWrap xs = zip [1..] xs

getIndex x xs = take 1 (drop (x-1) xs)