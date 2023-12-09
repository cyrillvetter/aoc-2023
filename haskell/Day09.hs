main = do
    history <- map (genHistory . map read . words) . lines <$> readFile "inputs/9.txt"
    print $ sum $ map (foldr (\h v -> v + last h) 0) history
    print $ sum $ map (foldr (\h v -> head h - v) 0) history

genHistory :: [Int] -> [[Int]]
genHistory = takeWhile (any (/= 0)) . iterate (\l -> zipWith (-) (tail l) l)
