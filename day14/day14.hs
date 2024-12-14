import Data.Either (fromRight)
import Data.Function (on)
import Data.List (maximumBy, nub)
import Text.Parsec (char, newline, sepBy1, sepEndBy1, string)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

type Pos = (Int, Int)

type Robot = (Pos, Pos)

part1 :: FilePath -> IO Int
part1 file = do
  robots <- fromRight [] <$> parseFromFile parseRobots file
  let size@(sizeX, sizeY) = (maxX robots + 1, maxY robots + 1)
  let poss = move size 100 robots
  let q1 = length . filter (\(x, y) -> x > (sizeX `div` 2) && y < (sizeY `div` 2))
  let q2 = length . filter (\(x, y) -> x < (sizeX `div` 2) && y < (sizeY `div` 2))
  let q3 = length . filter (\(x, y) -> x < (sizeX `div` 2) && y > (sizeY `div` 2))
  let q4 = length . filter (\(x, y) -> x > (sizeX `div` 2) && y > (sizeY `div` 2))
  return . product $ [q1, q2, q3, q4] <*> [poss]

part2 :: FilePath -> IO Int
part2 file = do
  robots <- fromRight [] <$> parseFromFile parseRobots file
  let size@(sizeX, sizeY) = (maxX robots + 1, maxY robots + 1)
  return $ findTree size 0 robots

move :: Pos -> Int -> [Robot] -> [Pos]
move (sizeX, sizeY) steps = map (\((px, py), (vx, vy)) -> ((px + steps * vx) `mod` sizeX, (py + steps * vy) `mod` sizeY))

findTree :: Pos -> Int -> [Robot] -> Int
findTree size steps robots = do
  let poss = move size steps robots
  if poss == nub poss
    then
      steps
    else
      findTree size (steps + 1) robots

maxX :: [Robot] -> Int
maxX = fst . fst . maximumBy (compare `on` (fst . fst))

maxY :: [Robot] -> Int
maxY = snd . fst . maximumBy (compare `on` (snd . fst))

parseRobots :: Parser [Robot]
parseRobots = sepEndBy1 parseRobot newline

parseRobot :: Parser Robot
parseRobot = do
  string "p="
  [px, py] <- sepBy1 int $ char ','
  string " v="
  [vx, vy] <- sepBy1 int $ char ','
  return ((px, py), (vx, vy))
