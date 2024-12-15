import Control.Monad (foldM_, forM_)
import Data.Array.IO (IOArray, getAssocs, getBounds, newArray, readArray, writeArray)
import Data.Either (fromRight)
import Data.Functor (($>))
import Text.Parsec (char, many1, newline, sepEndBy1, try, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

data Tile = Robot | Box | Wall | Empty deriving (Eq)

instance Show Tile where
  show :: Tile -> String
  show Robot = "@"
  show Box = "O"
  show Wall = "#"
  show Empty = "."

data Dir = N | E | S | W deriving (Show, Eq)

type Pos = (Int, Int)

type Warehouse = IOArray Pos Tile

part1 :: FilePath -> IO Int
part1 file = do
  (tiles, dirs) <- fromRight ([], []) <$> parseFromFile parse file
  let (sizeX, sizeY) = (length $ head tiles, length tiles)
  warehouse <- newArray ((0, 0), (sizeX - 1, sizeY - 1)) undefined :: IO Warehouse
  mapM_ (\pos@(x, y) -> writeArray warehouse pos $ tiles !! y !! x) [(x, y) | x <- [0 .. sizeX - 1], y <- [0 .. sizeY - 1]]
  pos <- head . map fst . filter (\(_, tile) -> tile == Robot) <$> getAssocs warehouse
  writeArray warehouse pos Empty
  foldM_ (move warehouse) pos dirs
  sum . map ((\(x, y) -> 100 * y + x) . fst) . filter (\(_, tile) -> tile == Box) <$> getAssocs warehouse

move :: Warehouse -> Pos -> Dir -> IO Pos
move warehouse pos@(x, y) N = do
  let pos' = (x, y - 1)
  tile <- readArray warehouse pos'
  case tile of
    Empty -> return pos'
    Wall -> return pos
    Box -> do
      newPos <- move warehouse pos' N
      writeArray warehouse pos' Empty
      writeArray warehouse newPos Box
      if newPos == pos'
        then
          return pos
        else
          return pos'
move warehouse pos@(x, y) E = do
  let pos' = (x + 1, y)
  tile <- readArray warehouse pos'
  case tile of
    Empty -> return pos'
    Wall -> return pos
    Box -> do
      newPos <- move warehouse pos' E
      writeArray warehouse pos' Empty
      writeArray warehouse newPos Box
      if newPos == pos'
        then
          return pos
        else
          return pos'
move warehouse pos@(x, y) S = do
  let pos' = (x, y + 1)
  tile <- readArray warehouse pos'
  case tile of
    Empty -> return pos'
    Wall -> return pos
    Box -> do
      newPos <- move warehouse pos' S
      writeArray warehouse pos' Empty
      writeArray warehouse newPos Box
      if newPos == pos'
        then
          return pos
        else
          return pos'
move warehouse pos@(x, y) W = do
  let pos' = (x - 1, y)
  tile <- readArray warehouse pos'
  case tile of
    Empty -> return pos'
    Wall -> return pos
    Box -> do
      newPos <- move warehouse pos' W
      writeArray warehouse pos' Empty
      writeArray warehouse newPos Box
      if newPos == pos'
        then
          return pos
        else
          return pos'

printWarehouse :: Warehouse -> IO ()
printWarehouse warehouse = do
  (_, (sizeX, sizeY)) <- getBounds warehouse
  forM_ [0 .. sizeY] $ \y -> do
    forM_ [0 .. sizeX] $ \x -> putStr . show =<< readArray warehouse (x, y)
    putStrLn ""

parse :: Parser ([[Tile]], [Dir])
parse = do
  tiles <- parseTiles
  newline
  dirs <- parseDirs
  return (tiles, dirs)

parseTiles :: Parser [[Tile]]
parseTiles = sepEndBy1 (many1 parseTile) newline

parseDirs :: Parser [Dir]
parseDirs = concat <$> sepEndBy1 (many1 parseDir) newline

parseTile :: Parser Tile
parseTile = try (char '@' $> Robot) <|> try (char 'O' $> Box) <|> try (char '#' $> Wall) <|> try (char '.' $> Empty)

parseDir :: Parser Dir
parseDir = try (char '^' $> N) <|> try (char '>' $> E) <|> try (char 'v' $> S) <|> try (char '<' $> W)
