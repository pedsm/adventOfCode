import Data.List.Split
import Data.List


main = do
  contents <- readFile "input.txt"
  let pairsRaw = splitOn "\n" contents
  let pairs = map (tuplify . parsePair) pairsRaw
  let overLapArray = map fullyOverlaps pairs
  let result = length (filter (True==) overLapArray)
  print result


fullyOverlaps :: ([Int], [Int]) -> Bool
fullyOverlaps (xs,ys) = length(union xs ys) == (max (length xs) (length ys))

parsePair pairRaw = map parseSection (splitOn "," pairRaw)

parseSection raw = parseSection' (map parseInt (splitOn "-" raw))
parseSection' (x:y:_) = [x..y]

parseInt :: [Char] -> Int
parseInt s = read s :: Int

tuplify (x:y:_) = (x,y)