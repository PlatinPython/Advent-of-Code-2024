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

part1 :: FilePath -> IO Int
part1 file = do
  contents <- lines <$> readFile file
  let (maxX, maxY) = (length $ head contents, length contents)
  farm <- newListArray ((0, 0), (maxX - 1, maxY - 1)) $ concat $ transpose contents :: IO Farm
  checked <- newArray ((0, 0), (maxX - 1, maxY - 1)) False :: IO Checked
  fmap sum . calculateRegionPrices farm checked . fmap fst . find (not . snd) =<< getAssocs checked

calculateRegionPrices :: Farm -> Checked -> Maybe Pos -> IO [Int]
calculateRegionPrices _ _ Nothing = return []
calculateRegionPrices farm checked (Just pos) = do
  crop <- readArray farm pos
  price <- uncurry (*) <$> fillRegion farm checked crop (1, 4) pos
  fmap (price :) . calculateRegionPrices farm checked . fmap fst . find (not . snd) =<< getAssocs checked

fillRegion :: Farm -> Checked -> Char -> Region -> Pos -> IO Region
fillRegion farm checked crop region@(area, peri) pos@(x, y) = do
  writeArray checked pos True
  bounds <- getBounds checked
  region@(area, peri) <- do
    pos <- return . head $ dirs pos
    notSeen <- check checked bounds pos
    if notSeen
      then do
        c <- readArray farm pos
        if c == crop
          then do
            l <- length <$> seen bounds (dirs pos)
            fillRegion farm checked crop (area + 1, peri + 4 - 2 * l) pos
          else
            return region
      else
        return region
  region@(area, peri) <- do
    pos <- return $ dirs pos !! 1
    notSeen <- check checked bounds pos
    if notSeen
      then do
        c <- readArray farm pos
        if c == crop
          then do
            l <- length <$> seen bounds (dirs pos)
            fillRegion farm checked crop (area + 1, peri + 4 - 2 * l) pos
          else
            return region
      else
        return region
  region@(area, peri) <- do
    pos <- return $ dirs pos !! 2
    notSeen <- check checked bounds pos
    if notSeen
      then do
        c <- readArray farm pos
        if c == crop
          then do
            l <- length <$> seen bounds (dirs pos)
            fillRegion farm checked crop (area + 1, peri + 4 - 2 * l) pos
          else
            return region
      else
        return region
  pos <- return $ dirs pos !! 3
  notSeen <- check checked bounds pos
  if notSeen
    then do
      c <- readArray farm pos
      if c == crop
        then do
          l <- length <$> seen bounds (dirs pos)
          fillRegion farm checked crop (area + 1, peri + 4 - 2 * l) pos
        else
          return region
    else
      return region
  where
    dirs :: Pos -> [Pos]
    dirs p = [second (flip (-) 1), first (+ 1), second (+ 1), first (flip (-) 1)] <*> [p]
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

checkOutOfBounds :: Bounds -> Pos -> Bool
checkOutOfBounds ((minX, minY), (maxX, maxY)) (x, y) = x < minX || y < minY || x > maxX || y > maxY

guarded :: (a -> Bool) -> (a -> b) -> a -> Maybe b
guarded check f a =
  if check a
    then
      Just $ f a
    else
      Nothing
