import Control.Monad (forM_, unless, when)
import Data.Array.IO (IOArray, Ix, MArray, getAssocs, getBounds, newListArray, readArray, writeArray)
import Data.Bifunctor (second)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Maybe.HT (toMaybe)
import Foreign (fromBool)

type Disk = IOArray Int (Maybe Int)

part1 :: FilePath -> IO Int
part1 file = solve fillBlocks =<< readFile file
  where
    fillBlocks :: Disk -> Int -> Int -> IO ()
    fillBlocks disk headIx tailIx = do
      headIx' <- findNextEmpty disk headIx
      if isNothing headIx'
        then
          return ()
        else do
          let headIx'' = fromMaybe headIx headIx'
          tailIx' <- findNextFile disk tailIx
          when (headIx'' < tailIx') $ do
            swapElem disk headIx'' tailIx'
            fillBlocks disk headIx'' tailIx'

part2 :: FilePath -> IO Int
part2 file = solve fillBlocks =<< readFile file
  where
    fillBlocks :: Disk -> Int -> Int -> IO ()
    fillBlocks disk min max = do
      headIx' <- findNextEmpty disk min
      if isNothing headIx'
        then
          return ()
        else do
          let headIx'' = fromMaybe min headIx'
          tailIx' <- findNextFile disk max
          when (headIx'' < tailIx') $ do
            fileSize <- sizeOfFile disk tailIx' =<< readArray disk tailIx'
            emptyBlock <- findEmptyBlock disk fileSize tailIx' headIx''
            unless (isNothing emptyBlock) $ forM_ [0 .. fileSize - 1] $ \x -> swapElem disk (fromMaybe 0 emptyBlock + x) (tailIx' - x)
            fillBlocks disk headIx'' (tailIx' - fileSize)

solve :: (Disk -> Int -> Int -> IO ()) -> String -> IO Int
solve swap str = do
  let (_, disk) = second concat . mapAccumL (\(free, id) len -> ((not free, id + fromBool free), replicate len $ toMaybe (not free) id)) (False, 0) . map (read . (: [])) . concat . lines $ str
  blocks <- newListArray (0, length disk - 1) disk :: IO Disk
  (min, max) <- getBounds blocks
  swap blocks min max
  sum . mapMaybe (\(ix, val) -> (* ix) <$> val) <$> getAssocs blocks

findNextEmpty :: Disk -> Int -> IO (Maybe Int)
findNextEmpty disk ix = do
  (_, max) <- getBounds disk
  if ix >= max
    then
      return Nothing
    else do
      x <- readArray disk ix
      if isNothing x
        then
          return $ Just ix
        else
          findNextEmpty disk $ ix + 1

findNextFile :: Disk -> Int -> IO Int
findNextFile disk ix = do
  x <- readArray disk ix
  if isJust x
    then
      return ix
    else
      findNextFile disk $ ix - 1

sizeOfEmpty :: Disk -> Int -> IO Int
sizeOfEmpty disk ix = do
  (_, max) <- getBounds disk
  if ix >= max
    then
      return 0
    else do
      x <- readArray disk ix
      if isNothing x
        then
          (+ 1) <$> sizeOfEmpty disk (ix + 1)
        else
          return 0

sizeOfFile :: Disk -> Int -> Maybe Int -> IO Int
sizeOfFile disk ix id = do
  x <- readArray disk ix
  if x == id
    then
      (+ 1) <$> sizeOfFile disk (ix - 1) id
    else
      return 0

findEmptyBlock :: Disk -> Int -> Int -> Int -> IO (Maybe Int)
findEmptyBlock disk size max ix = do
  if ix >= max
    then
      return Nothing
    else do
      blockSize <- sizeOfEmpty disk ix
      if blockSize >= size
        then
          return $ Just ix
        else do
          ix' <- findNextEmpty disk (ix + blockSize)
          if isNothing ix'
            then
              return Nothing
            else
              findEmptyBlock disk size max $ fromMaybe ix ix'

swapElem :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapElem arr a b = do
  aVal <- readArray arr a
  bVal <- readArray arr b
  writeArray arr b aVal
  writeArray arr a bVal
