import Data.List.Split (splitOn)
import Data.List (transpose)
import Common (toTuple)

main = do
    input <- map (words . last . splitOn ":") . lines <$> readFile "inputs/6.txt"
    print $ product $ map (countRecords . toTuple . map read) $ transpose input
    print $ countRecords $ toTuple $ map (read . concat) input

countRecords :: (Int, Int) -> Int
countRecords (time, dist) = time - (2 * untilRecord) + 1
    where untilRecord = head $ dropWhile (\charge -> (time - charge) * charge <= dist) [1..]
