import Data.List (isPrefixOf, tails, transpose)

part1 :: FilePath -> IO Int
part1 file = count . lines <$> readFile file
  where
    count xss = countBoth xss + countBoth (transpose xss) + countBoth (diagonals xss) + countBoth (diagonals $ reverse xss)
    countBoth xss = sum (map countXMAS xss) + sum (map countSAMX xss)
    countXMAS xs = length . filter id . map (isPrefixOf "XMAS") $ tails xs
    countSAMX xs = length . filter id . map (isPrefixOf "SAMX") $ tails xs

part2 :: FilePath -> IO Int
part2 file = count . lines <$> readFile file
  where
    count xss = sum [1 | i <- [1 .. length xss - 2], j <- [1 .. length (xss !! i) - 2], (xss !! i) !! j == 'A', test xss i j]
    test xss i j = testDiag xss i j && testDiag (reverse xss) ((length xss - 1) - i) j
    testDiag xss i j = testSingle xss i j 'M' 'S' || testSingle xss i j 'S' 'M'
    testSingle xss i j c1 c2 = testOne xss i j c1 (-) && testOne xss i j c2 (+)
    testOne xss i j c f = (xss !! f i 1) !! f j 1 == c

diagonals :: [[a]] -> [[a]]
diagonals xss = lowerDiagonals ++ upperDiagonals
  where
    lowerDiagonals = map diagonal . tails $ xss
    upperDiagonals = tail . map diagonal . tails $ transpose xss
    diagonal [] = []
    diagonal ([] : xss) = []
    diagonal ((x : xs) : xss) = x : diagonal (map tail xss)
