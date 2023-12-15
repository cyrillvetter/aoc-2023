import Data.List (transpose)
import qualified Data.Map.Strict as M

repetitions = 1000000000

main = do
    input <- lines <$> readFile "inputs/14.txt"
    print $ calcNorthWeight $ map tilt $ transpose input
    print $ repeatUntil input M.empty 1

repeatUntil :: [String] -> M.Map [String] Int -> Int -> Int
repeatUntil grid seen num = case cycled `M.lookup` seen of
    Just loopStart -> getNthCycleWeight $ loopStart + (repetitions - loopStart) `mod` (num - loopStart)
    Nothing        -> repeatUntil cycled (M.insert cycled num seen) (num + 1)
    where cycled = tiltCycle grid
          nextSeen = M.insert cycled num seen
          getNthCycleWeight i = calcNorthWeight $ transpose $ fst $ head $ dropWhile ((/= i) . snd) $ M.assocs nextSeen

calcNorthWeight :: [String] -> Int
calcNorthWeight grid = sum $ map (calcRow . zip [len,len-1..]) grid
    where len = length grid
          calcRow = foldl (\acc (val, c) -> if c == 'O' then val + acc else acc) 0

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
