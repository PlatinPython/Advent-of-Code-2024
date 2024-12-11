import Data.IntMap (IntMap, assocs, fromListWith)

part1 :: FilePath -> IO Int
part1 file = solve 25 . map read . words <$> readFile file

part2 :: FilePath -> IO Int
part2 file = solve 75 . map read . words <$> readFile file

solve :: Int -> [Int] -> Int
solve n = sum . (!! n) . iterate blink . fromListWith (+) . map (,1)

blink :: IntMap Int -> IntMap Int
blink = fromListWith (+) . concatMap transform . assocs

transform :: (Int, Int) -> [(Int, Int)]
transform (key, val) =
  if key == 0
    then
      [(1, val)]
    else do
      let n = floor (logBase 10 $ fromIntegral key) + 1
      if even n
        then do
          let n' = n `div` 2
          [(key `div` 10 ^ n', val), (key `mod` 10 ^ n', val)]
        else
          [(key * 2024, val)]
