import Data.Char (digitToInt)
import qualified Data.Array.Unboxed as A
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as PQ

type Point = (Int, Int)
type Move = (Point, Direction, Int)
data Direction = R | D | L | U | None deriving (Enum, Eq, Ord, Show)

main = do
    input <- map (map digitToInt) . lines <$> readFile "inputs/17.txt"
    let yBound = length input - 1
        xBound = length (head input) - 1
        grid = A.listArray ((0, 0), (yBound, xBound)) $ concat input
        origin = (0, 0)
        target = (xBound, yBound)
        startQueue = PQ.singleton 0 (origin, None, 1)
    print $ dijkstra startQueue grid S.empty target

dijkstra :: PQ.MinPQueue Int Move -> A.UArray Point Int -> S.Set Move -> Point -> Int
dijkstra queue grid visited target
    | coord == target = prio
    | m `S.member` visited = dijkstra q grid visited target
    | otherwise = dijkstra nextQueue grid (m `S.insert` visited) target
    where ((prio, m@(coord, _, _)), q) = PQ.deleteFindMin queue
          (_, bounds) = A.bounds grid
          neighbours = findNeighbours bounds m
          nextQueue = enqueue neighbours prio grid q

enqueue :: [Move] -> Int -> A.UArray Point Int -> PQ.MinPQueue Int Move -> PQ.MinPQueue Int Move
enqueue [] _ _ queue = queue
enqueue (m@((x, y), _, _):ms) heatLoss grid queue = enqueue ms heatLoss grid $ PQ.insert nextHeatLoss m queue
    where nextHeatLoss = heatLoss + (grid A.! (y, x))

findNeighbours :: (Int, Int) -> Move -> [Move]
findNeighbours (yBound, xBound) ((x, y), dir, consecutive) = neighbours
    where near = [((x + 1, y), R, getConsecutive R), ((x - 1, y), L, getConsecutive L), ((x, y + 1), D, getConsecutive D), ((x, y - 1), U, getConsecutive U)]
          neighbours = filter (\(p, d, s) -> isInBounds p && invertDirection d /= dir && s <= 3) near
          isInBounds (px, py) = px >= 0 && px <= xBound && py >= 0 && py <= yBound
          getConsecutive d = if d == dir then consecutive + 1 else 1

invertDirection :: Direction -> Direction
invertDirection dir = case dir of
    R -> L
    L -> R
    U -> D
    D -> U
