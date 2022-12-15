import Data.List (transpose, mapAccumL, scanl1, scanl, groupBy)

{- ======= Part 1 ======= -}

-- Number trees visible from the outside of the rectangular forest.
numTreesVisible :: [[Integer]] -> Integer
numTreesVisible trees = fromIntegral . length . filter (any id) . transpose $ allViews
  where allViews = (concat .) <$> [view, down, right, up] <*> [trees]
        down     = transpose . view . transpose      -- Check visibility of all trees from above.
        right    = map reverse . view . map reverse  -- Check visibility of all trees from the right.
        up       = transpose . right . transpose     -- Check visibility of all trees from below.

-- Visibility of trees from the left.
view :: [[Integer]] -> [[Bool]]
view = map $ snd . mapAccumL (\x y -> (max x y, x < y)) (-1)


{- ======= Part 2 ======= -}

-- score of trees, when looking to the right.
score :: [[Integer]] -> [[Integer]]
score = map $ reverse . concat . map (tail . scanl (\a _->a+1) 0) . groupBy (==) . reverse . scanl1 max

{- ======= Tests ======== -}

-- Read the input from a specified file.
readInput :: String -> IO [[Integer]]
readInput path = map (map $ read . pure) . lines <$> readFile path :: IO [[Integer]] 

-- Read input and solve.
solve1 :: String -> IO ()
solve1 path = readInput path >>= print . numTreesVisible

-- Read input from file, output solution.
main :: IO ()
main = solve1 "input.txt"