import Data.Bits (xor)
import Data.List (groupBy, nubBy, sortOn)
import Data.Function (on)

main :: IO ()
main = do
  putStrLn . ("Part 1: "++) . show =<< part1 "day22/input.txt"
  putStrLn . ("Part 2: "++) . show =<< part2 "day22/input.txt"

part1 :: FilePath -> IO Int
part1 file = sum . map ((!! 2000) . iterate update . read) . lines <$> readFile file

part2 :: FilePath -> IO Int
part2 file = maximum . map (sum . map fst) . groupBy ((==) `on` snd) . sortOn snd . concatMap (nubBy ((==) `on` snd) . toChangeLists . assocs . map (`mod` 10) . take 2001 . iterate update . read) . lines <$> readFile file

toChangeLists :: [(Int, Int)] -> [(Int, (Int, Int, Int, Int))]
toChangeLists ((_, a):bs@((_, b):(_, c):(x, d):_)) = (x, (a, b, c, d)) : toChangeLists bs
toChangeLists _ = []

assocs :: [Int] -> [(Int, Int)]
assocs xs = zip (drop 1 xs) (changes xs)

changes :: [Int] -> [Int]
changes xs = zipWith (-) (tail xs) xs

update :: Int -> Int
update s = do
  let s' = (s `xor` (s * 64)) `mod` 16777216
  let s'' = (s' `xor` (s' `div` 32)) `mod` 16777216
  let s''' = (s'' `xor` (s'' * 2048)) `mod` 16777216
  s'''
