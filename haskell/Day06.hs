import Data.List (transpose)

main = do
    input <- map (tail . words) . lines <$> readFile "inputs/6.txt"
    print $ product $ map (countRecords . map read) $ transpose input
    print $ countRecords $ map (read . concat) input

countRecords :: [Int] -> Int
countRecords [time, dist] = time - (2 * untilRecord) + 1
    where untilRecord = head $ dropWhile (\charge -> (time - charge) * charge <= dist) [1..]
