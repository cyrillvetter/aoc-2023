{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Data.List (sortOn)
import qualified Data.Set as S

type Coord = (Int, Int, Int)
type Brick = (Coord, Coord)

main = do
    input <- sortOn (\((_, _, z), _) -> z) . map parseLine . lines <$> readFile "inputs/22.txt"
    let droppedBricks = dropBricks input S.empty
    print $ disintegrate 0 droppedBricks
    print $ countFalls 0 droppedBricks

disintegrate :: Int -> [Brick] -> Int
disintegrate i bricks
    | i >= length bricks - 1 = 1
    | otherwise = res + disintegrate (i + 1) bricks
    where deleted = deleteAt i bricks
          dropped = dropBricks deleted S.empty
          res = if dropped == deleted then 1 else 0

countFalls :: Int -> [Brick] -> Int
countFalls i bricks
    | i >= length bricks - 1 = 0
    | otherwise = amt + countFalls (i + 1) bricks
    where deleted = deleteAt i bricks
          dropped = dropBricks deleted S.empty
          amt = differenceAmounts deleted dropped

dropBricks :: [Brick] -> S.Set Coord -> [Brick]
dropBricks [] _ = []
dropBricks (b:bs) view = brick : dropBricks bs nextCoords
    where
        (brick, nextCoords) = simulateFall b
        simulateFall :: Brick -> (Brick, S.Set Coord)
        simulateFall t@((x1, y1, z1), (x2, y2, z2))
            | z1 == 0 || not (S.null overlaps) = (((x1, y1, z1 + 1), (x2, y2, z2 + 1)), S.union view $ S.map (\(x, y, z) -> (x, y, z + 1)) curr)
            | otherwise = simulateFall ((x1, y1, z1 - 1), (x2, y2, z2 - 1))
            where
                curr
                    | x1 /= x2 = S.fromList $ map (,y1,z1) [x1..x2]
                    | y1 /= y2 = S.fromList $ map (x1,,z1) [y1..y2]
                    | otherwise = S.fromList $ map (x1,y1,) [z1..z2]
                overlaps = view `S.intersection` curr

differenceAmounts :: Eq a => [a] -> [a] -> Int
differenceAmounts [] _ = 0
differenceAmounts _ [] = 0
differenceAmounts (x:xs) (y:ys)
    | x == y = differenceAmounts xs ys
    | otherwise = 1 + differenceAmounts xs ys

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

parseLine :: String -> Brick
parseLine l = (left, right)
    where [left, right] = map (toTriple . map read . splitOn ",") $ splitOn "~" l
          toTriple [x, y, z] = (x, y, z)
