import Data.Either (fromRight)
import Data.List (sortBy)
import Data.Maybe (isJust, isNothing)
import Text.Parsec (char, newline, sepBy1, sepEndBy1)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

part1 :: FilePath -> IO Int
part1 file = do
  (rules, updates) <- fromRight ([], []) <$> parseFromFile parser file
  return . sum . map (\x -> x !! (length x `div` 2)) $ filterUpdates isJust rules updates

part2 :: FilePath -> IO Int
part2 file = do
  (rules, updates) <- fromRight ([], []) <$> parseFromFile parser file
  return . sum . map ((\x -> x !! (length x `div` 2)) . sortByRules rules) $ filterUpdates isNothing rules updates
  where
    sortByRules rules = sortBy rulesOrder
      where
        rulesOrder a b =
          if (a, b) `elem` rules then LT else GT

filterUpdates :: (Maybe [Int] -> Bool) -> [(Int, Int)] -> [[Int]] -> [[Int]]
filterUpdates check rules = filter $ check . foldl filterFold (Just [])
  where
    filterFold Nothing _ = Nothing
    filterFold (Just acc) val =
      if not (any (\(_, snd) -> snd `elem` acc) (filter (\(fst, _) -> fst == val) rules))
        then
          Just (val : acc)
        else Nothing

parser :: Parser ([(Int, Int)], [[Int]])
parser = do
  rules <- sepEndBy1 parseRule newline
  newline
  updates <- sepEndBy1 parseUpdate newline
  return (rules, updates)

parseRule :: Parser (Int, Int)
parseRule = do
  [fst, snd] <- sepBy1 int (char '|')
  return (fst, snd)

parseUpdate :: Parser [Int]
parseUpdate = sepBy1 int (char ',')
