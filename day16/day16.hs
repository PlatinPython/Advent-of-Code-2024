import Control.Lens (view, _1, _2)
import Control.Monad ((<=<))
import Data.Array ((!))
import Data.Graph (Edge, Graph, Tree, Vertex, graphFromEdges)
import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H (fromList, insert, view)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM (adjust, empty, insert, singleton, (!?))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS (empty, insert, member)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set (fromList, member, toList)
import Data.Tree (flatten, unfoldTree)

data Dir = N | S | E | W deriving (Eq, Ord)

type Pos = (Int, Int)

type Key = (Pos, Dir)

data DijkstraState = DijkstraState
  { visited :: IntSet,
    distanceMap :: IntMap Dist,
    nodeQueue :: MinPrioHeap Dist Int,
    parentsMap :: IntMap [Vertex]
  }

type CostFn = Edge -> Dist

data Dist = Dist Int | Infinity deriving (Show, Eq)

instance Ord Dist where
  (<=) :: Dist -> Dist -> Bool
  _ <= Infinity = True
  Infinity <= Dist _ = False
  Dist x <= Dist y = x <= y

addDist :: Dist -> Dist -> Dist
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

toInt :: Dist -> Maybe Int
toInt Infinity = Nothing
toInt (Dist x) = Just x

(!??) :: IntMap Dist -> Int -> Dist
(!??) distanceMap key = fromMaybe Infinity (distanceMap IM.!? key)

part1 :: FilePath -> IO (Maybe Int)
part1 file = do
  (start, end, (graph, nodeFromVertex, vertexFromKey)) <- setup . lines <$> readFile file
  return $ toInt . shortestDistance (mapMaybe (vertexFromKey . (end,)) [N, E, S, W]) . fst . dijkstra graph (costFromEdge (view _2 . nodeFromVertex)) =<< vertexFromKey (start, E)

part2 :: FilePath -> IO (Maybe Int)
part2 file = do
  (start, end, (graph, nodeFromVertex, vertexFromKey)) <- setup . lines <$> readFile file
  return $ length . nub . map (view _1 . nodeFromVertex) . (flatten <=< (allShortestPaths (mapMaybe (vertexFromKey . (end,)) [N, E, S, W]) . dijkstra graph (costFromEdge (view _2 . nodeFromVertex)))) <$> vertexFromKey (start, E)
  where
    buildPathTree :: IntMap [Vertex] -> Vertex -> Tree Vertex
    buildPathTree parents = unfoldTree (\v -> (v, concat $ parents IM.!? v))
    allShortestPaths :: [Vertex] -> (IntMap Dist, IntMap [Vertex]) -> [Tree Vertex]
    allShortestPaths targets s@(distances, parents) = map (buildPathTree parents) . filter isShortestTarget $ targets
      where
        minDistance = shortestDistance targets $ fst s
        isShortestTarget :: Vertex -> Bool
        isShortestTarget = (== minDistance) . (distances !??)

setup :: [String] -> (Pos, Pos, (Graph, Vertex -> (Pos, Key, [Key]), Key -> Maybe Vertex))
setup maze = do
  let start = head [(x, y) | y <- [0 .. length maze - 1], x <- [0 .. length (maze !! y) - 1], maze !! y !! x == 'S']
  let end = head [(x, y) | y <- [0 .. length maze - 1], x <- [0 .. length (maze !! y) - 1], maze !! y !! x == 'E']
  (start, end, mazeToGraph maze)

mazeToGraph :: [String] -> (Graph, Vertex -> (Pos, Key, [Key]), Key -> Maybe Vertex)
mazeToGraph maze = graphFromEdges [let key = (cell, dir) in (cell, key, children key) | cell <- Set.toList freeSpace, dir <- [N, E, S, W]]
  where
    freeSpace = Set.fromList [(x, y) | y <- [0 .. length maze - 1], x <- [0 .. length (head maze) - 1], maze !! y !! x /= '#']
    children :: Key -> [Key]
    children (cell@(x, y), N) = [(cell', N) | let { cell' = (x, y - 1) }, Set.member cell' freeSpace] <> [(cell, W), (cell, E)]
    children (cell@(x, y), E) = [(cell', E) | let { cell' = (x + 1, y) }, Set.member cell' freeSpace] <> [(cell, N), (cell, S)]
    children (cell@(x, y), S) = [(cell', S) | let { cell' = (x, y + 1) }, Set.member cell' freeSpace] <> [(cell, E), (cell, W)]
    children (cell@(x, y), W) = [(cell', W) | let { cell' = (x - 1, y) }, Set.member cell' freeSpace] <> [(cell, S), (cell, N)]

shortestDistance :: [Vertex] -> IntMap Dist -> Dist
shortestDistance targets distances = minimum ((distances !??) <$> targets)

cost :: (Key, Key) -> Dist
cost ((u, _), (v, _)) = Dist $ if u == v then 1000 else 1

costFromEdge :: (Vertex -> Key) -> Edge -> Dist
costFromEdge keyFromVertex (u, v) = cost (keyFromVertex u, keyFromVertex v)

-- Dijkstra's algorithm based on https://mmhaskell.com/blog/2022/8/22/dijkstras-algorithm-in-haskell (modified)
dijkstra :: Graph -> CostFn -> Vertex -> (IntMap Dist, IntMap [Vertex])
dijkstra graph cost start = processQueue initialState
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
       in case compare altDistance $ d0 !?? neighborNode of
            LT -> DijkstraState v1 (IM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1) (IM.insert neighborNode [node] p0)
            EQ -> DijkstraState v1 d0 q1 (IM.adjust (node :) neighborNode p0)
            GT -> ds
