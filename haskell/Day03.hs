import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map as M
import Debug.Trace (trace)

type Point = (Int, Int)

main = do
    input <- createGrid . lines <$> readFile "inputs/3.txt"
    print $ countEngineNumbers input
    print $ countGearNumbers input
    print "Day 3"

countEngineNumbers :: [(Point, Char)] -> Int
countEngineNumbers p = sum $ compute symbols
    where grid = M.fromList p
          symbols = map fst $ filter (\(_, c) -> c /= '.' && not (isDigit c)) p
          compute :: [Point] -> [Int]
          compute [] = []
          compute (x:xs) = nums ++ compute xs
              where neighbours = filter (\n -> isDigit (grid M.! n)) $ getNeighbours x
                    nums = nub $ map (getSurroundingNum grid) neighbours

countGearNumbers :: [(Point, Char)] -> Int
countGearNumbers p = sum $ compute symbols
    where grid = M.fromList p
          symbols = map fst $ filter (\(_, c) -> c == '*') p
          compute :: [Point] -> [Int]
          compute [] = []
          compute (x:xs)
              | length nums == 2 = product nums : compute xs
              | otherwise = compute xs
              where neighbours = filter (\n -> isDigit (grid M.! n)) $ getNeighbours x
                    nums = nub $ map (getSurroundingNum grid) neighbours

getSurroundingNum :: M.Map Point Char -> Point -> Int
getSurroundingNum grid start@(sx, sy) = read $ fromLeft ++ startVal ++ fromRight
    where
        startVal = [grid M.! start]
        fromLeft = reverse $ move (-1) (sx - 1, sy)
        fromRight = move 1 (sx + 1, sy)
        move :: Int -> Point -> String
        move step curr@(x, y)
            | not (isInBound curr) = ""
            | not (isDigit val) = ""
            | otherwise = val : move step (x + step, y)
            where val = grid M.! curr

getNeighbours :: Point -> [Point]
getNeighbours (x, y) = filter isInBound [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]

isInBound :: Point -> Bool
isInBound (x, y) = x >= 0 && y >= 0 && x < 140 && y < 140

createGrid :: [[Char]] -> [(Point, Char)]
createGrid chars = [((x, y), c) | (y, row) <- zip [0..] chars, (x, c) <- zip [0..] row]
