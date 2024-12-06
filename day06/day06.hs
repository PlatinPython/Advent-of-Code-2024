import Control.Monad.State (State, evalState, execState, get, put)
import Data.Set (Set, empty, insert, member)

part1 :: FilePath -> IO Int
part1 file = do
  map <- lines <$> readFile file
  let [pos] = [(y, x) | y <- [0 .. length map - 1], x <- [0 .. length (map !! y) - 1], (map !! y) !! x == '^']
  let (map', _, _) = execState part1State (map, (pos, N), False)
  return . sum $ length . filter (== 'X') <$> map'

part2 :: FilePath -> IO Int
part2 file = do
  map <- lines <$> readFile file
  let [pos] = [(y, x) | y <- [0 .. length map - 1], x <- [0 .. length (map !! y) - 1], (map !! y) !! x == '^']
  let (map', _, _) = execState part1State (map, (pos, N), False)
  let options = [(y, x) | y <- [0 .. length map' - 1], x <- [0 .. length (map' !! y) - 1], (map' !! y) !! x == 'X', (y, x) /= pos]
  let maps = [setPos '#' map pos | pos <- options]
  return . length . filter id $ (evalState part2State . (,(pos, N),empty,False) <$> maps)

type Map = [String]

type Pos = (Int, Int)

data Dir = N | E | S | W deriving (Show, Eq, Ord)

type DirPos = (Pos, Dir)

part1State :: State (Map, DirPos, Bool) ()
part1State = do
  let loop True = return ()
      loop False = do
        (map, pos, done) <- get
        let (map', pos', done') = move map pos
        put (map', pos', done')
        loop done'
  loop False

part2State :: State (Map, DirPos, Set DirPos, Bool) Bool
part2State = do
  let loop True = return False
      loop False = do
        (map, pos, set, done) <- get
        let (map', pos', done') = move map pos
        if not done' && member pos' set
          then
            return True
          else do
            let set' = insert pos' set
            put (map', pos', set', done')
            loop done'
  loop False

move :: Map -> DirPos -> (Map, DirPos, Bool)
move map ((y, x), N)
  | y == 0 = (setPos 'X' map (y, x), ((y, x), N), True)
  | (map !! (y - 1)) !! x == '#' = (map, ((y, x), E), False)
  | otherwise = (setPos 'X' map (y, x), ((y - 1, x), N), False)
move map ((y, x), E)
  | x == length (map !! y) - 1 = (setPos 'X' map (y, x), ((y, x), E), True)
  | (map !! y) !! (x + 1) == '#' = (map, ((y, x), S), False)
  | otherwise = (setPos 'X' map (y, x), ((y, x + 1), E), False)
move map ((y, x), S)
  | y == length map - 1 = (setPos 'X' map (y, x), ((y, x), S), True)
  | (map !! (y + 1)) !! x == '#' = (map, ((y, x), W), False)
  | otherwise = (setPos 'X' map (y, x), ((y + 1, x), S), False)
move map ((y, x), W)
  | x == 0 = (setPos 'X' map (y, x), ((y, x), W), True)
  | (map !! y) !! (x - 1) == '#' = (map, ((y, x), N), False)
  | otherwise = (setPos 'X' map (y, x), ((y, x - 1), W), False)

setPos :: Char -> Map -> Pos -> Map
setPos c map (y, x) = changeList map y $ changeList (map !! y) x c
  where
    changeList :: [a] -> Int -> a -> [a]
    changeList xs x val = before ++ (val : tail after)
      where
        (before, after) = splitAt x xs
