import Control.Lens (view, _1)
import Data.Array ((!))
import Data.Either (fromRight)
import Data.Graph (Edge, Graph, Vertex, graphFromEdges)
import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H (fromList, insert, view)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM (empty, insert, singleton, (!?))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS (empty, insert, member)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Tree (flatten, unfoldTree)
import Text.Parsec (char, newline, sepEndBy1)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

type Pos = (Int, Int)

data DijkstraState = DijkstraState
  { visited :: IntSet,
    distanceMap :: IntMap Dist,
    nodeQueue :: MinPrioHeap Dist Int,
    parentsMap :: IntMap [Vertex]
  }

data Dist = Dist Int | Infinity deriving (Show, Eq)

instance Ord Dist where
  (<=) :: Dist -> Dist -> Bool
  _ <= Infinity = True
  Infinity <= Dist _ = False
  Dist x <= Dist y = x <= y

addDist :: Dist -> Dist -> Dist
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: IntMap Dist -> Int -> Dist
(!??) distanceMap key = fromMaybe Infinity (distanceMap IM.!? key)

part1 :: FilePath -> Int -> IO (Maybe Dist)
part1 file i = do
  poss <- fromRight [] <$> parseFromFile parse file
  let maxX = maximum . map fst $ poss
  let maxY = maximum . map snd $ poss
  let corrupted = take i poss
  let (_, vertexFromKey, (distances, _)) = runDijkstra (maxX, maxY) corrupted
  return $ (distances !??) <$> vertexFromKey (maxX, maxY)

part2 :: FilePath -> Int -> IO (Maybe Pos)
part2 file i = do
  poss <- fromRight [] <$> parseFromFile parse file
  let maxX = maximum . map fst $ poss
  let maxY = maximum . map snd $ poss
  let corrupted = take i poss
  let (nodeFromVertex, vertexFromKey, (_, parents)) = runDijkstra (maxX, maxY) corrupted
  let Just exit = vertexFromKey (maxX, maxY)
  return . findBlockingPos (maxX, maxY) corrupted (drop i poss) $ map nodeFromVertex . getPath parents $ exit
  where
    getPath :: IntMap [Vertex] -> Vertex -> [Vertex]
    getPath parents = nub . flatten . unfoldTree (\v -> (v, concat $ parents IM.!? v))
    findBlockingPos :: Pos -> [Pos] -> [Pos] -> [Pos] -> Maybe Pos
    findBlockingPos _ _ [] _ = Nothing
    findBlockingPos max@(maxX, maxY) corrupted (byte : bytes) path = do
      let corrupted' = byte : corrupted
      if byte `elem` path
        then do
          let (nodeFromVertex, vertexFromKey, (distances, parents)) = runDijkstra max corrupted'
          let Just exit = vertexFromKey (maxX, maxY)
          if distances !?? exit == Infinity
            then
              Just byte
            else
              findBlockingPos max corrupted' bytes $ map nodeFromVertex . getPath parents $ exit
        else
          findBlockingPos max corrupted' bytes path

runDijkstra :: Pos -> [Pos] -> (Vertex -> Pos, Pos -> Maybe Vertex, (IntMap Dist, IntMap [Vertex]))
runDijkstra (maxX, maxY) corrupted = do
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [(pos, pos, children ((0, 0), (maxX, maxY)) pos) | x <- [0 .. maxX], y <- [0 .. maxY], let pos = (x, y), pos `notElem` corrupted]
  let Just dijkstraResult = dijkstra graph <$> vertexFromKey (0, 0)
  (view _1 . nodeFromVertex, vertexFromKey, dijkstraResult)

children :: (Pos, Pos) -> Pos -> [Pos]
children ((minX, minY), (maxX, maxY)) (x, y) = [pos | pos@(x, y) <- [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)], x >= minX, y >= minY, x <= maxX, y <= maxY]

dijkstra :: Graph -> Vertex -> (IntMap Dist, IntMap [Vertex])
dijkstra graph start = processQueue initialState
  where
    initialVisited = IS.empty
    initialDistances = IM.singleton start (Dist 0)
    initialQueue = H.fromList [(Dist 0, start)]
    initialParents = IM.empty
    initialState = DijkstraState IS.empty initialDistances initialQueue initialParents
    processQueue :: DijkstraState -> (IntMap Dist, IntMap [Vertex])
    processQueue ds@(DijkstraState v0 d0 q0 p0) = case H.view q0 of
      Nothing -> (d0, p0)
      Just ((minDist, node), q1) ->
        if IS.member node v0
          then processQueue (ds {nodeQueue = q1})
          else
            let v1 = IS.insert node v0
                allNeighbors = (graph ! node)
                unvisitedNeighbors = filter (\n -> not (IS.member n v1)) allNeighbors
             in processQueue $ foldr (foldNeighbor node) (DijkstraState v1 d0 q1 p0) unvisitedNeighbors
    foldNeighbor :: Vertex -> Vertex -> DijkstraState -> DijkstraState
    foldNeighbor node neighborNode ds@(DijkstraState v1 d0 q1 p0) =
      let altDistance = addDist (d0 !?? node) $ cost (node, neighborNode)
       in if altDistance < d0 !?? neighborNode
            then
              DijkstraState v1 (IM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1) (IM.insert neighborNode [node] p0)
            else
              ds
    cost :: Edge -> Dist
    cost (u, v) = Dist $ if u == v then 0 else 1

parse :: Parser [Pos]
parse = flip sepEndBy1 newline $ do
  x <- int
  char ','
  y <- int
  return (x, y)
