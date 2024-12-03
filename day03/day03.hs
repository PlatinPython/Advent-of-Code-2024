import Control.Arrow (ArrowChoice (right))
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Text.Parsec (anyChar, between, many, parse, sepBy1, (<|>))
import Text.Parsec.Char (char, string)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

part1 :: FilePath -> IO Int
part1 file = sum . map (uncurry (*)) . unwrap . parse (parseMemory parseMul) file <$> readFile file

part2 :: FilePath -> IO Int
part2 file = snd . foldl condSum (True, 0) . map (right $ uncurry (*)) . unwrap . parse (parseMemory parseCondMul) file <$> readFile file
  where
    condSum (_, acc) (Left True) = (True, acc)
    condSum (_, acc) (Left False) = (False, acc)
    condSum (True, acc) (Right val) = (True, acc + val)
    condSum (False, acc) (Right _) = (False, acc)

unwrap :: Either a [b] -> [b]
unwrap (Left _) = []
unwrap (Right x) = x

parseMemory :: Parser a -> Parser [a]
parseMemory parser = catMaybes <$> many (try (Just <$> parser) <|> (anyChar $> Nothing))

parseMul :: Parser (Int, Int)
parseMul = do
  [fst, snd] <- between (string "mul(") (char ')') (sepBy1 int (char ','))
  return (fst, snd)

parseCond :: Parser Bool
parseCond = try (string "do()" $> True) <|> try (string "don't()" $> False)

parseCondMul :: Parser (Either Bool (Int, Int))
parseCondMul = Left <$> parseCond <|> Right <$> parseMul
