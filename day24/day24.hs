import Control.Lens (each, (%~))
import Data.Bits (xor)
import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Functor (($>))
import Data.List (intercalate, sort, tails)
import Data.Map (Map)
import Data.Map qualified as M (empty, fromList, insert, lookup, toList, unionWith)
import Data.Maybe (isJust, mapMaybe)
import Foreign (fromBool)
import Numeric (readBin)
import Text.Parsec (alphaNum, char, many, newline, sepEndBy1, string, try, (<|>))
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (binDigit)

type WireName = String

type Wire = Maybe Bool

type Wires = Map WireName Wire

data Gate = Gate
  { gateType :: GateType,
    input :: (WireName, WireName),
    output :: WireName
  }
  deriving (Show)

data GateType = And | Or | Xor deriving (Show)

main :: IO ()
main = do
  putStrLn . ("Part 1: " ++) . show =<< part1 "day24/input.txt"
  -- putStr "Part 2: "
  -- part2 "day24/input.txt"

part1 :: FilePath -> IO Int
part1 file = do
  (wires, gates) <- fromRight (M.empty, []) <$> parseFromFile parser file
  return . readWires 'z' . until (all isJust) (stepWires gates) $ wires

-- No, just no, brute force is not feasible
part2 :: FilePath -> IO ()
part2 file = do
  (wires, gates) <- fromRight (M.empty, []) <$> parseFromFile parser file
  let x = readWires 'x' wires
  let y = readWires 'y' wires
  print . length . pairs . pairs $ gates
  mapM_ (putStrLn . intercalate "," . sort . (\((a, a'), (b, b'), (c, c'), (d, d')) -> [a, a', b, b', c, c', d, d']) . (each . each %~ output)) . find (\swaps -> (== x + y) . readWires 'z' . until (all isJust) (stepWires $ runSwap gates swaps) $ wires) . map (\((a, b), (c, d)) -> (a, b, c, d)) . pairs . pairs . pairs $ gates
  where
    runSwap :: [Gate] -> ((Gate, Gate), (Gate, Gate), (Gate, Gate), (Gate, Gate)) -> [Gate]
    runSwap [] _ = []
    runSwap (x@(Gate {output = out}) : xs) swaps@((Gate {output = a}, Gate {output = a'}), (Gate {output = b}, Gate {output = b'}), (Gate {output = c}, Gate {output = c'}), (Gate {output = d}, Gate {output = d'}))
      | out == a = (x {output = a'}) : runSwap xs swaps
      | out == a' = (x {output = a}) : runSwap xs swaps
      | out == b = (x {output = b'}) : runSwap xs swaps
      | out == b' = (x {output = b}) : runSwap xs swaps
      | out == c = (x {output = c'}) : runSwap xs swaps
      | out == c' = (x {output = c}) : runSwap xs swaps
      | out == d = (x {output = d'}) : runSwap xs swaps
      | out == d' = (x {output = d}) : runSwap xs swaps
      | otherwise = x : runSwap xs swaps

stepWires :: [Gate] -> Wires -> Wires
stepWires [] wires = wires
stepWires ((Gate gateType (a, b) out) : xs) wires
  | M.lookup a wires == Just Nothing = stepWires xs wires
  | M.lookup b wires == Just Nothing = stepWires xs wires
  | otherwise = do
      let Just (Just inA) = M.lookup a wires
      let Just (Just inB) = M.lookup b wires
      stepWires xs $ flip (M.insert out) wires . Just $ case gateType of
        And -> inA && inB
        Or -> inA || inB
        Xor -> inA `xor` inB

readWires :: Char -> Wires -> Int
readWires c = head . map fst . readBin . concatMap (show . fromBool) . reverse . mapMaybe snd . filter (\(k, _) -> head k == c) . M.toList

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

parser :: Parser (Wires, [Gate])
parser = do
  wires <- M.fromList <$> sepEndBy1 parseWire newline
  newline
  gates <- sepEndBy1 parseGate newline
  return (M.unionWith const wires . M.fromList . map (,Nothing) . concatMap (\gate -> [fst $ input gate, snd $ input gate, output gate]) $ gates, gates)

parseWire :: Parser (WireName, Wire)
parseWire = do
  name <- many alphaNum
  string ": "
  value <- binDigit
  return (name, Just (value == '1'))

parseGate :: Parser Gate
parseGate = do
  input <- many alphaNum
  char ' '
  gateType <- try (string "AND" $> And) <|> try (string "OR" $> Or) <|> try (string "XOR" $> Xor)
  char ' '
  input <- (input,) <$> many alphaNum
  string " -> "
  output <- many alphaNum
  return $ Gate gateType input output
