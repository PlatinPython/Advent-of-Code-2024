{-# LANGUAGE LambdaCase #-}

import Control.Monad (foldM_)
import Data.Array.IO (IOArray, getAssocs, getBounds, newListArray, readArray, writeArray)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (nub, sortOn, transpose)
import Data.Maybe (fromMaybe)
import Text.Parsec (char, many1, newline, sepEndBy1, try, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

data Tile = Robot | Box | Wall | Empty | LeftBox | RightBox deriving (Eq)

data Dir = N | E | S | W

type Pos = (Int, Int)

type Warehouse = IOArray Pos Tile

part1 :: FilePath -> IO Int
part1 file = uncurry (solve Box) . fromRight ([], []) =<< parseFromFile parse file

part2 :: FilePath -> IO Int
part2 file = uncurry (solve LeftBox) . first (map (concatMap (\case Wall -> [Wall, Wall]; Box -> [LeftBox, RightBox]; Empty -> [Empty, Empty]; Robot -> [Robot, Empty]))) . fromRight ([], []) =<< parseFromFile parse file

solve :: Tile -> [[Tile]] -> [Dir] -> IO Int
solve gpsTile tiles dirs = do
  let (sizeX, sizeY) = (length (head tiles) - 1, length tiles - 1)
  warehouse <- newListArray ((0, 0), (sizeX, sizeY)) . concat . transpose $ tiles
  pos <- head . map fst . filter (\(_, tile) -> tile == Robot) <$> getAssocs warehouse
  writeArray warehouse pos Empty
  foldM_ (move warehouse) pos dirs
  sum . map ((\(x, y) -> 100 * y + x) . fst) . filter (\(_, tile) -> tile == gpsTile) <$> getAssocs warehouse

move :: Warehouse -> Pos -> Dir -> IO Pos
move warehouse pos@(x, y) N = do
  let pos'@(x', y') = (x, y - 1)
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
    LeftBox -> fromMaybe pos <$> handleWideBox warehouse pos pos' N
    RightBox -> fromMaybe pos <$> handleWideBox warehouse pos (x' - 1, y') N
move warehouse pos@(x, y) E = do
  let pos'@(x', y') = (x + 1, y)
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
    LeftBox -> fromMaybe pos <$> handleWideBox warehouse pos pos' E
    RightBox -> fromMaybe pos <$> handleWideBox warehouse pos (x' - 1, y') E
move warehouse pos@(x, y) S = do
  let pos'@(x', y') = (x, y + 1)
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
    LeftBox -> fromMaybe pos <$> handleWideBox warehouse pos pos' S
    RightBox -> fromMaybe pos <$> handleWideBox warehouse pos (x' - 1, y') S
move warehouse pos@(x, y) W = do
  let pos'@(x', y') = (x - 1, y)
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
    LeftBox -> fromMaybe pos <$> handleWideBox warehouse pos pos' W
    RightBox -> fromMaybe pos <$> handleWideBox warehouse pos (x' - 1, y') W

handleWideBox :: Warehouse -> Pos -> Pos -> Dir -> IO (Maybe Pos)
handleWideBox warehouse rpos@(rx, ry) pos@(x, y) dir = do
  changes <- moveWideBox warehouse pos dir
  case changes of
    Nothing -> return Nothing
    Just changes -> do
      mapM_
        ( \(from@(fx, fy), to@(tx, ty)) -> do
            writeArray warehouse from Empty
            writeArray warehouse (fx + 1, fy) Empty
            writeArray warehouse to LeftBox
            writeArray warehouse (tx + 1, ty) RightBox
        )
        $ reverse changes
      return . return $ case dir of
        N -> (rx, ry - 1)
        E -> (rx + 1, ry)
        S -> (rx, ry + 1)
        W -> (rx - 1, ry)

moveWideBox :: Warehouse -> Pos -> Dir -> IO (Maybe [(Pos, Pos)])
moveWideBox warehouse pos@(x, y) N = do
  let lpos@(lx, ly) = (x, y - 1)
  let rpos@(rx, ry) = (x + 1, y - 1)
  ltile <- readArray warehouse lpos
  rtile <- readArray warehouse rpos
  l <- case ltile of
    Empty -> return $ Just [(pos, lpos)]
    Wall -> return Nothing
    LeftBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse lpos N
    RightBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse (lx - 1, ly) N
  r <- case rtile of
    Empty -> return $ Just [(pos, lpos)]
    Wall -> return Nothing
    LeftBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse rpos N
    RightBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse (rx - 1, ry) N
  return . fmap (sortOn (negate . snd . fst) . nub) $ (++) <$> l <*> r
moveWideBox warehouse pos@(x, y) E = do
  let pos' = (x + 1, y)
  let pos'' = (x + 2, y)
  tile <- readArray warehouse pos''
  case tile of
    Empty -> return $ Just [(pos, pos')]
    Wall -> return Nothing
    LeftBox -> fmap ((pos, pos') :) <$> moveWideBox warehouse pos'' E
moveWideBox warehouse pos@(x, y) S = do
  let lpos@(lx, ly) = (x, y + 1)
  let rpos@(rx, ry) = (x + 1, y + 1)
  ltile <- readArray warehouse lpos
  rtile <- readArray warehouse rpos
  l <- case ltile of
    Empty -> return $ Just [(pos, lpos)]
    Wall -> return Nothing
    LeftBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse lpos S
    RightBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse (lx - 1, ly) S
  r <- case rtile of
    Empty -> return $ Just [(pos, lpos)]
    Wall -> return Nothing
    LeftBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse rpos S
    RightBox -> fmap ((pos, lpos) :) <$> moveWideBox warehouse (rx - 1, ry) S
  return . fmap (sortOn (snd . fst) . nub) $ (++) <$> l <*> r
moveWideBox warehouse pos@(x, y) W = do
  let pos' = (x - 1, y)
  let pos'' = (x - 2, y)
  tile <- readArray warehouse pos'
  case tile of
    Empty -> return $ Just [(pos, pos')]
    Wall -> return Nothing
    RightBox -> fmap ((pos, pos') :) <$> moveWideBox warehouse pos'' W

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
