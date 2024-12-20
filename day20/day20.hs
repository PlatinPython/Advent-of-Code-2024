import Control.Monad (filterM)
import Data.Array.IO (IOArray, getAssocs, newArray, newListArray, readArray, writeArray)
import Data.List (find, tails, transpose)

type Pos = (Int, Int)

type Maze = IOArray Pos Char

type Steps = IOArray Pos Int

part1 :: FilePath -> IO Int
part1 = solve 2

part2 :: FilePath -> IO Int
part2 = solve 20

solve :: Int -> FilePath -> IO Int
solve maxSteps file = do
  contents <- lines <$> readFile file
  let max@(maxX, maxY) = (length (head contents) - 1, length contents - 1)
  maze <- newListArray ((0, 0), max) . concat . transpose $ contents :: IO Maze
  steps <- newArray ((0, 0), max) (-1) :: IO Steps
  Just start <- fmap fst . find ((== 'S') . snd) <$> getAssocs maze
  Just end <- fmap fst . find ((== 'E') . snd) <$> getAssocs maze
  writeArray steps start 0
  fillSteps steps maze end start
  length . filter (\(steps, (a, b)) -> abs (a - b) >= (100 + steps)) . filter ((<= maxSteps) . fst) . map (\((aPos, aSteps), (bPos, bSteps)) -> (stepsToCheat aPos bPos, (aSteps, bSteps))) . pairs . filter ((>= 0) . snd) <$> getAssocs steps
  where
    fillSteps :: Steps -> Maze -> Pos -> Pos -> IO ()
    fillSteps steps maze end current =
      if current == end
        then
          return ()
        else do
          pos <-
            fmap head
              . filterM
                ( \pos -> do
                    tile <- readArray maze pos
                    step <- readArray steps pos
                    return $ tile /= '#' && step < 0
                )
              $ [\(x, y) -> (x, y - 1), \(x, y) -> (x + 1, y), \(x, y) -> (x, y + 1), \(x, y) -> (x - 1, y)] <*> [current]
          writeArray steps pos . (+ 1) =<< readArray steps current
          fillSteps steps maze end pos
    pairs :: [a] -> [(a, a)]
    pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
    stepsToCheat :: Pos -> Pos -> Int
    stepsToCheat (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)
