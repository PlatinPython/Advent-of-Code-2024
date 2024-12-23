import Control.Lens (each, view, (%~), _1, _2, _3)
import Data.Either (fromRight)
import Data.Function (on)
import Data.Graph (Graph, Vertex, edges, graphFromEdges, vertices)
import Data.IntSet (IntSet, delete, empty, insert, intersection, toList)
import Data.IntSet qualified as IS (fromList, null)
import Data.List (intercalate, maximumBy, nub, sort)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S (fromList, member)
import Text.Parsec (char, lower, many, newline, sepBy1, sepEndBy1)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: FilePath -> IO Int
part1 file = do
  edges <- concatMap (\(a, b) -> [(a, b), (b, a)]) . fromRight [] <$> parseFromFile parser file
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [(k, k, c) | k <- nub . map fst $ edges, let c = map snd . filter ((== k) . fst) $ edges]
  return . length . filter (\(a : _, b : _, c : _) -> a == 't' || b == 't' || c == 't') . map (each %~ (view _2 . nodeFromVertex)) . find3Cycles $ graph

part2 :: FilePath -> IO ()
part2 file = do
  edges <- concatMap (\(a, b) -> [(a, b), (b, a)]) . fromRight [] <$> parseFromFile parser file
  let input@(graph, nodeFromVertex, vertexFromKey) = graphFromEdges [(k, k, c) | k <- nub . map fst $ edges, let c = map snd . filter ((== k) . fst) $ edges]
  putStrLn . intercalate "," . sort . map (view _2 . nodeFromVertex) . maximumBy (compare `on` length) . map toList $ maximalCliques input empty (IS.fromList $ vertices graph) empty

find3Cycles :: Graph -> [(Vertex, Vertex, Vertex)]
find3Cycles graph = do
  let es = S.fromList $ edges graph
  let vs = vertices graph
  let cs = nub . map sortTuple . concatMap (\(a, b) -> map (a,b,) . filter (\c -> S.member (a, c) es && S.member (b, c) es) $ vs) $ es
  cs
  where
    sortTuple :: (Ord a) => (a, a, a) -> (a, a, a)
    sortTuple (a, b, c) = do
      let [a', b', c'] = sort [a, b, c]
      (a', b', c')

-- Implementation of https://en.wikipedia.org/wiki/Bron-Kerbosch_algorithm#Without_pivoting
maximalCliques :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex) -> IntSet -> IntSet -> IntSet -> [IntSet]
maximalCliques input@(graph, nodeFromVertex, vertexFromKey) r p x = do
  if IS.null p && IS.null x
    then
      [r]
    else
      concat . view _1 . foldl (\(cliques, p, x) v -> (maximalCliques input (insert v r) (intersection p $ n v) (intersection x $ n v) : cliques, delete v p, insert v x)) ([], p, x) $ toList p
  where
    n :: Int -> IntSet
    n v = IS.fromList . mapMaybe vertexFromKey . view _3 $ nodeFromVertex v

parser :: Parser [(String, String)]
parser = do
  flip sepEndBy1 newline $ do
    [a, b] <- sepBy1 (many lower) $ char '-'
    return (a, b)
