import Data.List.Split
import Data.List


main = do
  contents <- readFile "input.txt"
  let pairsRaw = splitOn "\n" contents
  let pairs = map (tuplify . parsePair) pairsRaw
  let overLapArray = map anyOverlap pairs
  let result = length (filter (True==) overLapArray)
  print result


anyOverlap :: ([Int], [Int]) -> Bool
anyOverlap (xs,ys) = length(intersect xs ys) >= 1

parsePair pairRaw = map parseSection (splitOn "," pairRaw)

parseSection raw = parseSection' (map parseInt (splitOn "-" raw))
parseSection' (x:y:_) = [x..y]

parseInt :: [Char] -> Int
parseInt s = read s :: Int

tuplify (x:y:_) = (x,y)