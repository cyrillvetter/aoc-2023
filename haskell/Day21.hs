import qualified Data.Set as S
import qualified Data.Array.Unboxed as A

type Point = (Int, Int)

neighbours = [(1, 0), (-1, 0), (0, 1), (0, -1)]

main = do
    input <- lines <$> readFile "inputs/21.txt"
    let bound = length input - 1
        grid = A.listArray ((0, 0), (bound, bound)) $ concat input
        start = S.singleton $ fst $ head $ filter ((== 'S') . snd) $ A.assocs grid
    print $ S.size $ iterate (\p -> step (getNeighbours $ S.toList p) grid S.empty) start !! 64

step :: [Point] -> A.UArray Point Char -> S.Set Point -> S.Set Point
step [] _ visited = visited
step (p:ps) grid visited
    | isOutOfBounds p bound || tile == '#' = step ps grid visited
    | otherwise = step ps grid (p `S.insert` visited)
    where (_, (_, bound)) = A.bounds grid
          tile = grid A.! p

isOutOfBounds :: Point -> Int -> Bool
isOutOfBounds (x, y) bound = x < 0 && y < 0 && x >= bound && y >= bound

getNeighbours :: [Point] -> [Point]
getNeighbours = concatMap get
    where
        get :: Point -> [Point]
        get (x, y) = map (\(px, py) -> (px + x, py + y)) neighbours
