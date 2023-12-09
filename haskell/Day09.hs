import Common (windowsOf)

main = do
    input <- map (map read . words) . lines <$> readFile "inputs/9.txt"
    print $ sum $ map processRight input
    print $ sum $ map processLeft input

processRight :: [Int] -> Int
processRight nums
    | all (== 0) nums = 0
    | otherwise = last nums + processRight (map (\[a, b] -> b - a) $ windowsOf 2 nums)

processLeft :: [Int] -> Int
processLeft nums
    | all (== 0) nums = 0
    | otherwise = head nums - processLeft (map (\[a, b] -> b - a) $ windowsOf 2 nums)
