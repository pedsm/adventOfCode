import Data.List.Split
import Data.List


main = do
  contents <- readFile "input.txt"
  let gamesRaw = splitOn "\n" contents
  let games = map (\x -> tuplify (map head (splitOn " " x)) ) gamesRaw
  let pointsPerGame = map (\x -> solve x) games
  let totalPoints = sum pointsPerGame
  print totalPoints

tuplify xs = (head xs, last xs)

solve :: (Char, Char) -> Int
solve (a,b) = (fixedPoints b) + (matchPoints a b)


fixedPoints :: Char -> Int
fixedPoints 'X' = 1
fixedPoints 'Y' = 2
fixedPoints 'Z' = 3

--   A B C (THEM)
-- X D W L
-- Y L D W
-- Z W L D

matchPoints :: Char -> Char -> Int
matchPoints 'A' 'X' = 3
matchPoints 'B' 'X' = 0
matchPoints 'C' 'X' = 6
matchPoints 'A' 'Y' = 6
matchPoints 'B' 'Y' = 3
matchPoints 'C' 'Y' = 0
matchPoints 'A' 'Z' = 0
matchPoints 'B' 'Z' = 6
matchPoints 'C' 'Z' = 3