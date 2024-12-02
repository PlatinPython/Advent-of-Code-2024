part1 :: FilePath -> IO Int
part1 file = do
  length . filter id . map (checkSafe . (zipWith (-) <*> tail) . map read . words) . lines <$> readFile file

part2 :: FilePath -> IO Int
part2 file = do
  contents <- map (map read . words) . lines <$> readFile file :: IO [[Int]]
  return . length . filter id . map (any (checkSafe . (zipWith (-) <*> tail)) . subLists) $ contents
  where
    subLists xs = xs : [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

checkSafe :: [Int] -> Bool
checkSafe (x : xs)
  | x > 0 && x < 4 = all (\x -> x > 0 && x < 4) xs
  | x < 0 && x > -4 = all (\x -> x < 0 && x > -4) xs
  | otherwise = False
