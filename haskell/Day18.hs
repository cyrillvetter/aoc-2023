{-# LANGUAGE TupleSections #-}

import qualified Data.Set as S
import Data.Bifunctor (bimap)

type Point = (Int, Int)

neighbours = [(1, 0), (-1, 0), (0, 1), (0, -1)]

main = do
    input <- lines <$> readFile "inputs/18.txt"
    let trench = buildTrench input (0, 0)
        (topLeftX, topLeftY) = S.findMin trench
    print $ floodFill [(topLeftX + 1, topLeftY + 1)] trench

buildTrench :: [String] -> Point -> S.Set Point
buildTrench [] _ = S.empty
buildTrench (x:xs) p = S.union (S.fromList movements) $ buildTrench xs endPoint
    where (dir:steps:_) = words x
          movements = move p (read steps) (head dir)
          endPoint = last movements

floodFill :: [Point] -> S.Set Point -> Int
floodFill [] visited = S.size visited
floodFill (p@(x, y):ps) visited
    | p `S.member` visited = floodFill ps visited
    | otherwise = floodFill (nearest ++ ps) (p `S.insert` visited)
    where nearest = map (bimap (x +) (y +)) neighbours

move :: Point -> Int -> Char -> [Point]
move (x, y) steps 'R' = map (,y) [x..x+steps]
move (x, y) steps 'L' = map (,y) [x,x-1..x-steps]
move (x, y) steps 'D' = map (x,) [y..y+steps]
move (x, y) steps 'U' = map (x,) [y,y-1..y-steps]