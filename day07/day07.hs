import Data.Either (fromRight)
import Text.Parsec (char, newline, sepBy1, sepEndBy1)
import Text.Parsec.Char (string)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

part1 :: FilePath -> IO Int
part1 file = do
  equations <- fromRight [] <$> parseFromFile parser file
  return . sum . map fst . filter snd $ zipWith (\(x, _) possible -> (x, possible)) equations (map testPart1 equations)

part2 :: FilePath -> IO Int
part2 file = do
  equations <- fromRight [] <$> parseFromFile parser file
  return . sum . map fst . filter snd $ zipWith (\(x, _) possible -> (x, possible)) equations (map testPart2 equations)

testPart1 :: (Int, [Int]) -> Bool
testPart1 (result, vals) = do
  let plus = replicate (length vals - 1) (+)
  let times = replicate (length vals - 1) (*)
  let operators = combineTwo plus times
  let values = map (\ops -> foldl (\acc (f, x) -> f acc x) (head vals) (zip ops (tail vals))) operators
  result `elem` values

testPart2 :: (Int, [Int]) -> Bool
testPart2 (result, vals) = do
  let plus = replicate (length vals - 1) (+)
  let times = replicate (length vals - 1) (*)
  let concate = replicate (length vals - 1) concatInts
  let operators = combineThree plus times concate
  let values = map (\ops -> foldl (\acc (f, x) -> f acc x) (head vals) (zip ops (tail vals))) operators
  result `elem` values

combineTwo :: [a] -> [a] -> [[a]]
combineTwo (x : xs) (y : ys) = map (x :) (combineTwo xs ys) ++ map (y :) (combineTwo xs ys)
combineTwo _ _ = [[]]

combineThree :: [a] -> [a] -> [a] -> [[a]]
combineThree (x : xs) (y : ys) (z : zs) = map (x :) (combineThree xs ys zs) ++ map (y :) (combineThree xs ys zs) ++ map (z :) (combineThree xs ys zs)
combineThree _ _ _ = [[]]

concatInts :: Int -> Int -> Int
concatInts a b = a * (10 ^ length (show b)) + b

parser :: Parser [(Int, [Int])]
parser = sepEndBy1 parserCalib newline

parserCalib :: Parser (Int, [Int])
parserCalib = do
  result <- int
  string ": "
  vals <- sepBy1 int $ char ' '
  return (result, vals)
