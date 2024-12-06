import Control.Monad.State (State, execState, get, put)

part1 :: FilePath -> IO Int
part1 file = do
  map <- lines <$> readFile file
  let [pos] = [(y, x) | y <- [0 .. length map - 1], x <- [0 .. length (map !! y) - 1], (map !! y) !! x == '^']
  let (map', _, _) = execState update (map, (pos, N), False)
  return . sum $ length . filter (== 'X') <$> map'

type Map = [String]

type Pos = (Int, Int)

data Dir = N | E | S | W deriving (Show)

type DirPos = (Pos, Dir)

update :: State (Map, DirPos, Bool) ()
update = do
  let loop True = return ()
      loop False = do
        (map, pos, done) <- get
        let (map', pos', done') = move map pos
        put (map', pos', done')
        loop done'
  loop False

move :: Map -> DirPos -> (Map, DirPos, Bool)
move map ((y, x), N)
  | y == 0 = (setPos map (y, x), ((y, x), N), True)
  | (map !! (y - 1)) !! x == '#' = (map, ((y, x), E), False)
  | otherwise = (setPos map (y, x), ((y - 1, x), N), False)
move map ((y, x), E)
  | x == length (map !! y) - 1 = (setPos map (y, x), ((y, x), E), True)
  | (map !! y) !! (x + 1) == '#' = (map, ((y, x), S), False)
  | otherwise = (setPos map (y, x), ((y, x + 1), E), False)
move map ((y, x), S)
  | y == length map - 1 = (setPos map (y, x), ((y, x), S), True)
  | (map !! (y + 1)) !! x == '#' = (map, ((y, x), W), False)
  | otherwise = (setPos map (y, x), ((y + 1, x), S), False)
move map ((y, x), W)
  | x == 0 = (setPos map (y, x), ((y, x), W), True)
  | (map !! y) !! (x - 1) == '#' = (map, ((y, x), N), False)
  | otherwise = (setPos map (y, x), ((y, x - 1), W), False)

setPos :: Map -> Pos -> Map
setPos map (y, x) = changeList map y $ changeList (map !! y) x 'X'
  where
    changeList :: [a] -> Int -> a -> [a]
    changeList xs x val = before ++ (val : tail after)
      where
        (before, after) = splitAt x xs
