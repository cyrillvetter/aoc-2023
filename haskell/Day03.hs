import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map as M

type Point = (Int, Int)

neighbours = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, 1), (1, -1), (-1, -1)]

main = do
    input <- createGrid . lines <$> readFile "inputs/3.txt"
    let grid = M.fromList input
    print $ countEngineNumbers grid input
    print $ countGearNumbers grid input

countEngineNumbers :: M.Map Point Char -> [(Point, Char)] -> Int
countEngineNumbers grid p = sum $ concat $ getSurroundingNumbers symbols grid
    where symbols = map fst $ filter (\(_, c) -> c /= '.' && not (isDigit c)) p

countGearNumbers :: M.Map Point Char -> [(Point, Char)] -> Int
countGearNumbers grid p = sum $ map product $ filter ((== 2) . length) $ getSurroundingNumbers gears grid
    where gears = map fst $ filter (\(_, c) -> c == '*') p

getSurroundingNumbers :: [Point] -> M.Map Point Char -> [[Int]]
getSurroundingNumbers [] _ = []
getSurroundingNumbers (x:xs) grid = nums : getSurroundingNumbers xs grid
    where neighbours = filter (\n -> isDigit (grid M.! n)) $ getNeighbours x
          nums = nub $ map (getNumber grid) neighbours

getNumber :: M.Map Point Char -> Point -> Int
getNumber grid (sx, sy) = read $ reverse (move (-1) sx) ++ move 1 (sx + 1)
    where
        move :: Int -> Int -> String
        move step x
            | not (isInBound (x, sy) && isDigit val) = ""
            | otherwise = val : move step (x + step)
            where val = grid M.! (x, sy)

getNeighbours :: Point -> [Point]
getNeighbours (x, y) = filter isInBound $ map (\(nx, ny) -> (x + nx, y + ny)) neighbours

isInBound :: Point -> Bool
isInBound (x, y) = x >= 0 && y >= 0 && x < 140 && y < 140

createGrid :: [[Char]] -> [(Point, Char)]
createGrid chars = [((x, y), c) | (y, row) <- zip [0..] chars, (x, c) <- zip [0..] row]
