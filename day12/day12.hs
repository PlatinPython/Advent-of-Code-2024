{-# LANGUAGE LambdaCase #-}

import Control.Monad (filterM)
import Data.Array.IO (IOArray, getAssocs, getBounds, newArray, newListArray, readArray, writeArray)
import Data.Bifunctor (first, second)
import Data.List (find, transpose)

type Pos = (Int, Int)

type Region = (Int, Int)

type Bounds = (Pos, Pos)

type Farm = IOArray Pos Char

type Checked = IOArray Pos Bool

type Transform = Farm -> Checked -> Bounds -> Char -> Int -> Pos -> IO Int

part1 :: FilePath -> IO Int
part1 file = solve transform id' (1, 4) . lines =<< readFile file
  where
    id' _ _ _ _ i _ = return i
    transform :: Transform
    transform farm checked bounds crop peri pos = (peri + 4 -) . (2 *) . length <$> seen farm checked crop bounds (dirs pos)
    seen :: Farm -> Checked -> Char -> Bounds -> [Pos] -> IO [Pos]
    seen farm checked crop bounds dirs = do
      neighbors <- zip dirs <$> mapM (sequence . guarded (not . checkOutOfBounds bounds) (readArray checked)) dirs
      filterM (fmap (== crop) . readArray farm) . map fst . filter (\case (_, Nothing) -> False; (_, Just x) -> x) $ neighbors

part2 :: FilePath -> IO Int
part2 file = solve id' transform (1, 0) . lines =<< readFile file
  where
    id' _ _ _ _ i _ = return i
    transform :: Transform
    transform farm _ bounds crop sides pos = do
      ne <- mapM (sequence . guarded (not . checkOutOfBounds bounds) (readArray farm)) $ [\(x, y) -> (x, y - 1), \(x, y) -> (x + 1, y - 1), \(x, y) -> (x + 1, y)] <*> [pos]
      se <- mapM (sequence . guarded (not . checkOutOfBounds bounds) (readArray farm)) $ [\(x, y) -> (x + 1, y), \(x, y) -> (x + 1, y + 1), \(x, y) -> (x, y + 1)] <*> [pos]
      sw <- mapM (sequence . guarded (not . checkOutOfBounds bounds) (readArray farm)) $ [\(x, y) -> (x, y + 1), \(x, y) -> (x - 1, y + 1), \(x, y) -> (x - 1, y)] <*> [pos]
      nw <- mapM (sequence . guarded (not . checkOutOfBounds bounds) (readArray farm)) $ [\(x, y) -> (x - 1, y), \(x, y) -> (x - 1, y - 1), \(x, y) -> (x, y - 1)] <*> [pos]
      let f [a, b, c] = (a == Just crop && b /= Just crop && c == Just crop) || (a /= Just crop && c /= Just crop)
      return . (sides +) . length . filter id $ f <$> [ne, se, sw, nw]

solve :: Transform -> Transform -> Region -> [String] -> IO Int
solve f1 f2 start contents = do
  let (maxX, maxY) = (length $ head contents, length contents)
  farm <- newListArray ((0, 0), (maxX - 1, maxY - 1)) $ concat $ transpose contents :: IO Farm
  checked <- newArray ((0, 0), (maxX - 1, maxY - 1)) False :: IO Checked
  fmap sum . calculateRegionPrices farm checked f1 f2 start . fmap fst . find (not . snd) =<< getAssocs checked

calculateRegionPrices :: Farm -> Checked -> Transform -> Transform -> Region -> Maybe Pos -> IO [Int]
calculateRegionPrices _ _ _ _ _ Nothing = return []
calculateRegionPrices farm checked f1 f2 start (Just pos) = do
  crop <- readArray farm pos
  price <- uncurry (*) <$> fillRegion farm checked crop f1 f2 start pos
  fmap (price :) . calculateRegionPrices farm checked f1 f2 start . fmap fst . find (not . snd) =<< getAssocs checked

fillRegion :: Farm -> Checked -> Char -> Transform -> Transform -> Region -> Pos -> IO Region
fillRegion farm checked crop f1 f2 region@(area, sides) pos@(x, y) = do
  writeArray checked pos True
  bounds <- getBounds checked
  region@(area, sides) <- do
    pos@(x, y) <- return . head $ dirs pos
    notSeen <- check checked bounds pos
    if notSeen
      then do
        crop' <- readArray farm pos
        if crop' == crop
          then do
            sides <- f1 farm checked bounds crop sides pos
            fillRegion farm checked crop f1 f2 (area + 1, sides) pos
          else
            return region
      else
        return region
  region@(area, sides) <- do
    pos@(x, y) <- return $ dirs pos !! 1
    notSeen <- check checked bounds pos
    if notSeen
      then do
        crop' <- readArray farm pos
        if crop' == crop
          then do
            sides <- f1 farm checked bounds crop sides pos
            fillRegion farm checked crop f1 f2 (area + 1, sides) pos
          else
            return region
      else
        return region
  region@(area, sides) <- do
    pos@(x, y) <- return $ dirs pos !! 2
    notSeen <- check checked bounds pos
    if notSeen
      then do
        crop' <- readArray farm pos
        if crop' == crop
          then do
            sides <- f1 farm checked bounds crop sides pos
            fillRegion farm checked crop f1 f2 (area + 1, sides) pos
          else
            return region
      else
        return region
  (area, sides) <- do
    pos@(x, y) <- return $ dirs pos !! 3
    notSeen <- check checked bounds pos
    if notSeen
      then do
        crop' <- readArray farm pos
        if crop' == crop
          then do
            sides <- f1 farm checked bounds crop sides pos
            fillRegion farm checked crop f1 f2 (area + 1, sides) pos
          else
            return region
      else
        return region
  (area,) <$> f2 farm checked bounds crop sides pos
  where
    check :: Checked -> Bounds -> Pos -> IO Bool
    check checked bounds pos =
      if checkOutOfBounds bounds pos
        then
          return False
        else
          not <$> readArray checked pos
    seen :: Bounds -> [Pos] -> IO [Pos]
    seen bounds dirs = do
      neighbors <- zip dirs <$> mapM (sequence . guarded (not . checkOutOfBounds bounds) (readArray checked)) dirs
      filterM (fmap (== crop) . readArray farm) . map fst . filter (\case (_, Nothing) -> False; (_, Just x) -> x) $ neighbors

dirs :: Pos -> [Pos]
dirs pos = [second (flip (-) 1), first (+ 1), second (+ 1), first (flip (-) 1)] <*> [pos]

checkOutOfBounds :: Bounds -> Pos -> Bool
checkOutOfBounds ((minX, minY), (maxX, maxY)) (x, y) = x < minX || y < minY || x > maxX || y > maxY

guarded :: (a -> Bool) -> (a -> b) -> a -> Maybe b
guarded check f a =
  if check a
    then
      Just $ f a
    else
      Nothing
