import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Text.Parsec (newline, sepBy1, string)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

type Pos = (Int, Int)

type Machine = (Pos, Pos, Pos)

part1 :: FilePath -> IO Int
part1 file = sum . catMaybes . solve . fromRight [] <$> parseFromFile parseMachines file

part2 :: FilePath -> IO Int
part2 file = sum . catMaybes . solve . map (\(a, b, (px, py)) -> (a, b, (px + 10000000000000, py + 10000000000000))) . fromRight [] <$> parseFromFile parseMachines file

solve :: [Machine] -> [Maybe Int]
solve [] = []
solve (x : xs) = (price x :) $ solve xs

price :: Machine -> Maybe Int
price ((ax, ay), (bx, by), (px, py)) = do
  let (a, ar) = (by * px - bx * py) `divMod` (ax * by - ay * bx)
  let (b, br) = (ay * px - ax * py) `divMod` (ay * bx - ax * by)
  if ar /= 0 || br /= 0
    then
      Nothing
    else
      Just $ a * 3 + b

parseMachines :: Parser [Machine]
parseMachines = sepBy1 parseMachine newline

parseMachine :: Parser Machine
parseMachine = do
  a <- parseButtonA
  newline
  b <- parseButtonB
  newline
  prize <- parsePrize
  newline
  return (a, b, prize)

parseButtonA :: Parser Pos
parseButtonA = do
  string "Button A: X+"
  x <- int
  string ", Y+"
  y <- int
  return (x, y)

parseButtonB :: Parser Pos
parseButtonB = do
  string "Button B: X+"
  x <- int
  string ", Y+"
  y <- int
  return (x, y)

parsePrize :: Parser Pos
parsePrize = do
  string "Prize: X="
  x <- int
  string ", Y="
  y <- int
  return (x, y)
