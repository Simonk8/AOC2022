import Data.List (transpose, tails, mapAccumL)

-- Number trees visible from the outside of the rectangular forest.
numTreesVisible :: [[Integer]] -> Integer
numTreesVisible trees = fromIntegral . length . filter (any id) . transpose $ allViews
  where allViews = (concat .) <$> [view, down, right, up] <*> [trees]
        down     = transpose . view . transpose      -- Check visibility of all trees from below.
        right    = map reverse . view . map reverse  -- Check visibility of all trees from the right.
        up       = transpose . right . transpose     -- Check visibility of all trees from above.

-- Visibility of trees from the left.
view :: [[Integer]] -> [[Bool]]
view = map $ snd . mapAccumL (\x y -> (max x y, x < y)) (-1)

-- Read the input from a specified file.
readInput :: String -> IO [[Integer]]
readInput path = map (map $ read . (: [])) . lines <$> readFile path :: IO [[Integer]] 

-- Read input and solve.
solve :: String -> IO ()
solve path = readInput path >>= print . numTreesVisible

-- Read input from file, output solution.
main :: IO ()
main = solve "input.txt"