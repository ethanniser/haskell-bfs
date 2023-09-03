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
    bfs [] _ = Nothing -- Queue is empty, so no solution
    bfs ((currentVertex, currentPath) : restOfQueue) visited
      | currentVertex == end = Just (Solution currentPath) -- Found the end vertex, return the solution
      -- Haven't found the end yet, so keep searching
      | otherwise = case M.lookup currentVertex graph of -- get current vertex's neighbors from map
          Just neighbors ->
            -- only look at neighbors that haven't been visited yet
            let newNeighbors = filter (`S.notMember` visited) neighbors
                -- for each neighbor make a tuple of (neighbor, path to neighbor)
                -- where the path is the current path plus the neighbor
                newPaths = [(n, currentPath ++ [n]) | n <- newNeighbors]
                -- and these new paths to the queue
                newQueue = restOfQueue ++ newPaths
                -- and add these new neighbors to the visited set
                newVisited = foldl (\set (vertex, _) -> S.insert vertex set) visited newPaths
             in bfs newQueue newVisited -- continue searching with the new queue and visited set
          Nothing -> bfs restOfQueue visited -- this node is a dead end, so just continue checking the rest of the queue
