import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.List (sort)

part1 :: String -> IO Int
part1 file = do
  sum . map (\(x, y) -> abs $ x - y) . uncurry zip . join bimap sort . unzip . map ((\[x, y] -> (read x, read y)) . words) . lines <$> readFile file

part2 :: String -> IO Int
part2 file = do
  sum . (\(x, y) -> map (\x -> x * (length . filter (== x) $ y)) x) . unzip . map ((\[x, y] -> (read x, read y)) . words) . lines <$> readFile file
