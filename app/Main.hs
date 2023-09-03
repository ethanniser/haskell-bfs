import qualified Data.Map as M

main :: IO ()
main = do
  file <- getLine
  contents <- readFile $ "app/" ++ "maze" ++ file ++ ".txt"
  case solve $ parse contents of
    Just solution -> print solution
    Nothing -> putStrLn "No solution found"

type Graph = M.Map Int [Int]

data Maze = Maze
  { start :: Int,
    end :: Int,
    graph :: Graph
  }

newtype Solution = Solution {path :: [Int]}

instance Show Solution where
  show (Solution path) = show (length path) ++ "\n" ++ unwords (map show path)

parse :: String -> Maze
parse input = case lines input of
  (start : end : rest) -> Maze (read start) (read end) (foldl parseLine M.empty rest)
  where
    parseLine :: Graph -> String -> Graph
    parseLine graph line = M.insert (read key) (map read values) graph
      where
        (key : values) = words line

solve :: Maze -> Maybe Solution
solve (Maze start end graph) = bfs [(start, [start])] M.empty
  where
    bfs :: [(Int, [Int])] -> M.Map Int [Int] -> Maybe Solution
    bfs [] _ = Nothing -- Queue is empty, so no solution
    bfs ((currentVertex, currentPath) : restOfQueue) visited
      | currentVertex == end = Just (Solution currentPath) -- Found the end vertex, return the solution
      | otherwise = case M.lookup currentVertex graph of
          Just neighbors -> undefined
          Nothing -> Nothing -- this node is a dead end