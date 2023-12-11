import Data.List (transpose, tails)

type Point = (Int, Int)

partTwoScaler = 1000000 - 1

main = do
    input <- lines <$> readFile "inputs/11.txt"
    let grid = map fst $ filter ((/= '.') . snd) $ createGrid input
        emptyRows = getEmptyRows input
        emptyColumns = getEmptyRows $ transpose input
        combinations = twoCombinations grid

    print $ sum $ map (calcDistance 1 emptyRows emptyColumns) combinations
    print $ sum $ map (calcDistance partTwoScaler emptyRows emptyColumns) combinations

calcDistance :: Int -> [Int] -> [Int] -> (Point, Point) -> Int
calcDistance scaler emptyRows emptyColumns (a@(x1, y1), b@(x2, y2)) = dist + rowsBetween + columnsBetween
    where dist = manhattanDistance a b
          rowsBetween = scaler * length (filter (isBetween y1 y2) emptyRows)
          columnsBetween = scaler * length (filter (isBetween x1 x2) emptyColumns)

isBetween :: Int -> Int -> Int -> Bool
isBetween x y val = val > min x y && val < max x y

createGrid :: [[Char]] -> [(Point, Char)]
createGrid chars = [((x, y), c) | (y, row) <- zip [0..] chars, (x, c) <- zip [0..] row]

getEmptyRows :: [[Char]] -> [Int]
getEmptyRows = map fst . filter (all (== '.') . snd) . zip [0..]

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

twoCombinations :: [Point] -> [(Point, Point)]
twoCombinations points = [(x, y) | (x:xs) <- tails points, y <- xs]