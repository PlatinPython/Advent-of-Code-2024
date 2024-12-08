import Control.Monad (when)
import Data.Array.IO (IOArray, Ix, MArray, getAssocs, getElems, newArray, readArray, writeArray)
import Data.List (tails)

type Pos = (Int, Int)

part1 :: FilePath -> IO Int
part1 file = solve check . lines =<< readFile file
  where
    check (x, y) ((x1, y1), (x2, y2)) = (x - x1, y - y1) == (x1 - x2, y1 - y2) || (x - x2, y - y2) == (x2 - x1, y2 - y1)

part2 :: FilePath -> IO Int
part2 file = solve check . lines =<< readFile file
  where
    check (x, y) ((x1, y1), (x2, y2)) = (y - y1) * (x - x2) == (y - y2) * (x - x1)

solve :: (Pos -> (Pos, Pos) -> Bool) -> [String] -> IO Int
solve valid antennaMap = do
  let bounds@(xSize, ySize) = (length $ head antennaMap, length antennaMap)
  nodeMap <- newArray ((0, 0), bounds) False :: IO (IOArray Pos Bool)
  antennas <- newArray ('0', 'z') [] :: IO (IOArray Char [Pos])
  mapM_ (\(c, pos) -> modifyArray antennas c (pos :)) [(c, (x, y)) | x <- [0 .. xSize - 1], y <- [0 .. ySize - 1], let c = (antennaMap !! y) !! x, c /= '.']
  assocs <- getAssocs antennas
  mapM_ (findAntinodes valid bounds nodeMap) assocs
  length . filter id <$> getElems nodeMap

findAntinodes :: (Pos -> (Pos, Pos) -> Bool) -> Pos -> IOArray Pos Bool -> (Char, [Pos]) -> IO ()
findAntinodes valid (xSize, ySize) nodeMap (c, pos) = do
  let posPairs = pairs pos
  let possiblePos = [(x, y) | x <- [0 .. xSize - 1], y <- [0 .. ySize - 1]]
  mapM_ (\pos -> when (check posPairs pos) $ writeArray nodeMap pos True) possiblePos
  where
    pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
    check posPairs p = any (valid p) posPairs

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i f = do
  x <- readArray arr i
  writeArray arr i $ f x
