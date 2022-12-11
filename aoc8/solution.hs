import Data.List (transpose, tails, mapAccumL)

-- Number trees visible from the outside
numTreesVisible :: [[Integer]] -> Int
numTreesVisible trees = length . filter id . map (any id) . transpose $ allViews
  where allViews = (concat .) <$> [view, down, right, up] <*> [trees]
        down     = transpose . view . transpose
        right    = map reverse . view . map reverse
        up       = transpose . right . transpose

-- Number of trees visible from the left
view :: [[Integer]] -> [[Bool]]
view = map $ snd . mapAccumL (\x y -> (max x y, x < y)) (-1)

-- Read input from file, output solution
main :: IO ()
main = do
    input <- map (map $ read . (: [])) . lines <$> readFile "input.txt" :: IO [[Integer]]
    print $ numTreesVisible input