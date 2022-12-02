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

splitOnWhiteSpace = splitOn " "

solve :: (Char, Char) -> Int
solve (a,b) = (fixedPoints ((whatToDo b) (mapToRps a))) + (matchPoints (mapToRps a) ((whatToDo b) (mapToRps a)))

data RPS = R | P | S deriving (Enum, Show, Eq)

matchPoints :: RPS -> RPS -> Int
matchPoints x y = if x == y then 3
  else if winner x == y then 6
  else 0

winner :: RPS -> RPS
winner S = R
winner a = succ a

draw :: RPS -> RPS
draw x = x

loser :: RPS -> RPS
loser R = S
loser x = pred x

whatToDo :: Char -> RPS -> RPS
whatToDo 'X' = loser
whatToDo 'Y' = draw
whatToDo 'Z' = winner

-- Rock beats Scissor Beat Paper beats Rock

mapToRps :: Char -> RPS
mapToRps 'A' = R
mapToRps 'B' = P
mapToRps 'C' = S

fixedPoints :: RPS -> Int
fixedPoints R = 1
fixedPoints P = 2
fixedPoints S = 3