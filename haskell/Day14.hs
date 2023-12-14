import Data.List (transpose, intercalate)
import Debug.Trace (trace)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)

repetitions = 1000000000

main = do
    input <- lines <$> readFile "inputs/14.txt"
    let width = length input
    print $ calcNorthWeight $ map tilt $ transpose input
    -- print $ sum $ map (calcWeight (length input)) $ transpose input
    let until = cycleUntilRepetition (tiltCycle input) M.empty 0
    let res = last $ take (until + 2) $ iterate tiltCycle input
    print $ calcNorthWeight $ transpose res

calcNorthWeight :: [String] -> Int
calcNorthWeight grid = sum $ map (calc . zip [len,len-1..]) grid
    where
        len = length grid
        calc :: [(Int, Char)] -> Int
        calc [] = 0
        calc ((val, c):xs) = if c == 'O' then val + calc xs else calc xs

calcWeight :: Int -> String -> Int
calcWeight solid row
    | null toEnd = currSum
    | otherwise = currSum + calcWeight nextSolid (tail toEnd)
    where (until, toEnd) = span (/= '#') row
          nextSolid = solid - length until - 1
          rocksAmount = length $ filter (== 'O') until
          currSum = sum [solid,solid-1..solid-rocksAmount+1]

cycleUntilRepetition :: [String] -> M.Map [String] Int -> Int -> Int
cycleUntilRepetition grid seen num
    | isJust val = (repetitions - v) `mod` abs (v - num)
    | otherwise = cycleUntilRepetition cycled (M.insert cycled num seen) (num + 1)
    where cycled = tiltCycle grid
          val = cycled `M.lookup` seen
          v = fromJust val

tiltCycle :: [String] -> [String]
tiltCycle grid = east
    where north = transpose $ map tilt $ transpose grid
          west = map tilt north
          south = transpose $ map (reverse . tilt . reverse) $ transpose west
          east = map (reverse . tilt . reverse) south

tilt :: String -> String
tilt row
    | null toEnd = movedRocks
    | otherwise = movedRocks ++ "#" ++ tilt (tail toEnd)
    where (until, toEnd) = span (/= '#') row
          rocksAmount = length $ filter (== 'O') until
          emptyAmount = length until - rocksAmount
          movedRocks = replicate rocksAmount 'O' ++ replicate emptyAmount '.'
