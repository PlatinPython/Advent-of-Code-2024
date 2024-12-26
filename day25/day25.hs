import Data.Bifunctor (first, second)
import Data.List (transpose)
import Data.List.Split (chunksOf)

part1 :: FilePath -> IO Int
part1 file = length . filter id . map (not . any (> 5) . uncurry (zipWith (+))) . pairUp . group . chunksOf 7 . filter (/= "") . lines <$> readFile file
  where
    group :: [[String]] -> ([[Int]], [[Int]])
    group [] = ([], [])
    group (x : xs) = (if head x == "#####" then first else second) ((:) . map (length . drop 1 . filter (== '#')) . transpose $ x) $ group xs
    pairUp :: ([[Int]], [[Int]]) -> [([Int], [Int])]
    pairUp (locks, keys) = [(lock, key) | lock <- locks, key <- keys]
