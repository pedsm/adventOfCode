main = do 
  contents <- readFile "warmup/input.txt"
  let numbers = map read (lines contents) :: [Int]
  print (countHigherSteps numbers)

countHigherSteps :: [Int] -> Int
countHigherSteps list = step 0 list

step :: Int -> [Int] -> Int
step counter list = if length list <= 1
  then counter
  else if isBHigher (take 2 list) then step (counter + 1) (drop 1 list)
  else step counter (drop 1 list)

isBHigher :: [Int] -> Bool
isBHigher (x:y:xs) = x < y