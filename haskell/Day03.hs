import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map as M

type Point = (Int, Int)

adjacent = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, 1), (1, -1), (-1, -1)]

main = do
    input <- createGrid . lines <$> readFile "inputs/3.txt"
    let grid = M.fromList input
    print $ countEngineNumbers grid input
    print $ countGearNumbers grid input

countEngineNumbers :: M.Map Point Char -> [(Point, Char)] -> Int
countEngineNumbers grid p = sum $ concatMap (getAdjNumbers grid) symbols
    where symbols = map fst $ filter (\(_, c) -> c /= '.' && not (isDigit c)) p

countGearNumbers :: M.Map Point Char -> [(Point, Char)] -> Int
countGearNumbers grid p = sum $ map product $ filter ((== 2) . length) gearNums
    where gearNums = map (getAdjNumbers grid . fst) $ filter (\(_, c) -> c == '*') p

getAdjNumbers :: M.Map Point Char -> Point -> [Int]
getAdjNumbers grid p = nums
    where adj = filter (\n -> isDigit (grid M.! n)) $ getAdjacent p
          nums = nub $ map (buildNumber grid) adj

buildNumber :: M.Map Point Char -> Point -> Int
buildNumber grid (sx, sy) = read $ reverse (move (-1) sx) ++ move 1 (sx + 1)
    where
        move :: Int -> Int -> String
        move step x
            | not (isInBound (x, sy) && isDigit val) = ""
            | otherwise = val : move step (x + step)
            where val = grid M.! (x, sy)

getAdjacent :: Point -> [Point]
getAdjacent (x, y) = filter isInBound $ map (\(nx, ny) -> (x + nx, y + ny)) adjacent

isInBound :: Point -> Bool
isInBound (x, y) = x >= 0 && y >= 0 && x < 140 && y < 140

createGrid :: [[Char]] -> [(Point, Char)]
createGrid chars = [((x, y), c) | (y, row) <- zip [0..] chars, (x, c) <- zip [0..] row]
