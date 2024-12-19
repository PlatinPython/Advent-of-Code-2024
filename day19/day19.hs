import Data.Either (fromRight)
import Data.Function.Memoize (memoFix2)
import Data.List (find, groupBy, sortOn)
import Data.Ord (Down (Down))
import Text.Parsec (lower, many1, newline, sepBy1, sepEndBy1, string)
import Text.Parsec.String (Parser, parseFromFile)

type Memo f = f -> f

part1 :: FilePath -> IO Int
part1 file = length . filter (> 0) . uncurry solve . fromRight ([], []) <$> parseFromFile parse file

part2 :: FilePath -> IO Int
part2 file = sum . uncurry solve . fromRight ([], []) <$> parseFromFile parse file

solve :: [String] -> [String] -> [Int]
solve patterns = map (memoFix2 checkDesign . map (\x -> (length $ head x, x)) . groupBy (\a b -> length a == length b) . sortOn (Down . length) $ patterns)
  where
    checkDesign :: Memo ([(Int, [String])] -> String -> Int)
    checkDesign _ _ [] = 1
    checkDesign checkDesign patterns design = do
      let substrings = map (\x -> drop (fst x) design) . filter (\(i, patterns) -> any (\pattern -> take i design == pattern) patterns) $ patterns
      if null substrings
        then
          0
        else
          sum $ map (checkDesign patterns) substrings

parse :: Parser ([String], [String])
parse = do
  patterns <- sepBy1 (many1 lower) (string ", ")
  newline
  newline
  designs <- sepEndBy1 (many1 lower) newline
  return (patterns, designs)
