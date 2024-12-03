import Control.Arrow (ArrowChoice (right))
import Data.Either (rights)
import Data.Functor (($>))
import Text.Parsec (between, parse, sepBy1, (<|>))
import Text.Parsec.Char (char, string)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Regex.TDFA (getAllTextMatches, (=~))

part1 :: FilePath -> IO Int
part1 file = do
  contents <- readFile file
  return . sum . map (uncurry (*)) . rights . map (parse parseMul mempty) . getAllTextMatches $ contents =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

part2 :: FilePath -> IO Int
part2 file = do
  contents <- readFile file
  return . snd . foldl condSum (True, 0) . map (right $ uncurry (*)) . rights . map (parse parseCondMul mempty) . getAllTextMatches $ contents =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)"
  where
    condSum (_, acc) (Left True) = (True, acc)
    condSum (_, acc) (Left False) = (False, acc)
    condSum (True, acc) (Right val) = (True, acc + val)
    condSum (False, acc) (Right _) = (False, acc)

parseMul :: Parser (Int, Int)
parseMul = do
  [fst, snd] <- between (string "mul(") (char ')') (sepBy1 int (char ','))
  return (fst, snd)

parseCond :: Parser Bool
parseCond = try (string "do()" $> True) <|> try (string "don't()" $> False)

parseCondMul :: Parser (Either Bool (Int, Int))
parseCondMul = do
  Left <$> parseCond <|> Right <$> parseMul
