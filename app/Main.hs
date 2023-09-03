import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  file <- getLine
  contents <- readFile $ "app/" ++ "maze" ++ file ++ ".txt"
  case solve $ parse contents of
    Just solution -> putStrLn $ "Solution found:\n" ++ show solution
    Nothing -> putStrLn "No solution found"

type Graph = M.Map Int [Int]

data Maze = Maze
  { start :: Int,
    end :: Int,
    graph :: Graph
  }

newtype Solution = Solution {path :: [Int]}

instance Show Solution where
  show (Solution path) = "length- " ++ show (length path) ++ "\n" ++ unwords (map show path)

parse :: String -> Maze
parse input = case lines input of
  (start : end : rest) -> Maze (read start) (read end) (foldl parseLine M.empty rest)
  where
    parseLine :: Graph -> String -> Graph
    parseLine graph line = M.insert (read key) (map read values) graph
      where
        (key : values) = words line

solve :: Maze -> Maybe Solution
solve (Maze start end graph) = bfs [(start, [start])] S.empty
  where
    bfs :: [(Int, [Int])] -> S.Set Int -> Maybe Solution
    bfs [] _ = Nothing
    bfs ((currentVertex, currentPath) : restOfQueue) visited
      | currentVertex == end = Just (Solution currentPath)
      | otherwise = case M.lookup currentVertex graph of
          Just neighbors ->
            let newNeighbors = filter (`S.notMember` visited) neighbors
                newPaths = [(n, currentPath ++ [n]) | n <- newNeighbors]
                newQueue = restOfQueue ++ newPaths
                newVisited = foldl (\set (vertex, _) -> S.insert vertex set) visited newPaths
             in bfs newQueue newVisited
          Nothing -> bfs restOfQueue visited
