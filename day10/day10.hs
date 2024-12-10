import Data.Array.IO (IOArray, getAssocs, getBounds, newArray, readArray, writeArray)
import Data.List (nub)

type Pos = (Int, Int)

type Bounds = ((Int, Int), (Int, Int))

type TopoMap = IOArray Pos Int

part1 :: FilePath -> IO Int
part1 file = solve nub . lines =<< readFile file

part2 :: FilePath -> IO Int
part2 file = solve id . lines =<< readFile file

solve :: ([Pos] -> [Pos]) -> [String] -> IO Int
solve f input = do
  let (xSize, ySize) = (length $ head input, length input)
  topoMap <- newArray ((0, 0), (xSize - 1, ySize - 1)) undefined :: IO TopoMap
  mapM_ (\(height, pos) -> writeArray topoMap pos height) [(read . (: []) $ (input !! y) !! x, (x, y)) | x <- [0 .. xSize - 1], y <- [0 .. ySize - 1]]
  scores <- mapM (\(pos, _) -> length <$> findPath f topoMap pos 0) . filter (\(_, i) -> i == 0) =<< getAssocs topoMap
  return . sum $ scores

findPath :: ([Pos] -> [Pos]) -> TopoMap -> Pos -> Int -> IO [Pos]
findPath f topoMap pos height = do
  bounds <- getBounds topoMap
  if not $ checkInBounds bounds pos
    then
      return []
    else do
      currHeight <- readArray topoMap pos
      if currHeight /= height
        then
          return []
        else
          if height == 9
            then
              return [pos]
            else do
              let height' = height + 1
              peaks <- mapM (\pos -> findPath f topoMap pos height') [(fst pos, snd pos - 1), (fst pos + 1, snd pos), (fst pos, snd pos + 1), (fst pos - 1, snd pos)]
              return . f $ concat peaks

checkInBounds :: Bounds -> Pos -> Bool
checkInBounds ((xMin, yMin), (xMax, yMax)) (x, y) = not $ x < xMin || y < yMin || x > xMax || y > yMax
