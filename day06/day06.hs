import Control.Monad.State (State, evalState, execState, get, put)
import Data.Set (Set, empty, fromList, insert, member, toList)

main :: IO ()
main = do
  putStrLn . ("Part 1: " ++) . show =<< part1 "day06/input.txt"
  putStrLn . ("Part 2: " ++) . show =<< part2 "day06/input.txt"

part1 :: FilePath -> IO Int
part1 file = do
  map <- lines <$> readFile file
  let size = (length map, length $ head map)
  let [pos] = [(y, x) | y <- [0 .. length map - 1], x <- [0 .. length (map !! y) - 1], (map !! y) !! x == '^']
  let blocks = fromList [(y, x) | y <- [0 .. length map - 1], x <- [0 .. length (map !! y) - 1], (map !! y) !! x == '#']
  let (set', _, _) = execState (part1State (blocks, size)) (empty, (pos, N), False)
  return $ length set'

part2 :: FilePath -> IO Int
part2 file = do
  map <- lines <$> readFile file
  let size = (length map, length $ head map)
  let [pos] = [(y, x) | y <- [0 .. length map - 1], x <- [0 .. length (map !! y) - 1], (map !! y) !! x == '^']
  let blocks = fromList [(y, x) | y <- [0 .. length map - 1], x <- [0 .. length (map !! y) - 1], (map !! y) !! x == '#']
  let (set', _, _) = execState (part1State (blocks, size)) (empty, (pos, N), False)
  let maps = [insert pos blocks | pos <- toList set']
  return . length . filter id $ (\x -> evalState (part2State (x, size)) ((pos, N), empty, False)) <$> maps

type Map = (Set Pos, (Int, Int))

type Pos = (Int, Int)

data Dir = N | E | S | W deriving (Show, Eq, Ord)

type DirPos = (Pos, Dir)

part1State :: Map -> State (Set Pos, DirPos, Bool) ()
part1State map = do
  let loop True = return ()
      loop False = do
        (set, pos, done) <- get
        let (set', pos', done') = move map set pos
        put (set', pos', done')
        loop done'
  loop False

part2State :: Map -> State (DirPos, Set DirPos, Bool) Bool
part2State map = do
  let loop True = return False
      loop False = do
        (pos, seenSet, done) <- get
        let (_, pos', done') = move map empty pos
        if not done' && member pos' seenSet
          then
            return True
          else do
            let seenSet' = insert pos' seenSet
            put (pos', seenSet', done')
            loop done'
  loop False

move :: Map -> Set Pos -> DirPos -> (Set Pos, DirPos, Bool)
move (blocks, (ys, xs)) set ((y, x), N)
  | y == 0 = (insert (y, x) set, ((y, x), N), True)
  | member (y - 1, x) blocks = (set, ((y, x), E), False)
  | otherwise = (insert (y, x) set, ((y - 1, x), N), False)
move (blocks, (ys, xs)) set ((y, x), E)
  | x == xs - 1 = (insert (y, x) set, ((y, x), E), True)
  | member (y, x + 1) blocks = (set, ((y, x), S), False)
  | otherwise = (insert (y, x) set, ((y, x + 1), E), False)
move (blocks, (ys, xs)) set ((y, x), S)
  | y == ys - 1 = (insert (y, x) set, ((y, x), S), True)
  | member (y + 1, x) blocks = (set, ((y, x), W), False)
  | otherwise = (insert (y, x) set, ((y + 1, x), S), False)
move (blocks, (ys, xs)) set ((y, x), W)
  | x == 0 = (insert (y, x) set, ((y, x), W), True)
  | member (y, x - 1) blocks = (set, ((y, x), N), False)
  | otherwise = (insert (y, x) set, ((y, x - 1), W), False)
