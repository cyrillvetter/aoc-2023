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
        bound = (yBound, xBound)
        grid = A.listArray ((0, 0), bound) $ concat input
        origin = (0, 0)
        target = (xBound, yBound)
        startQueue = PQ.singleton 0 (origin, None, 1)
    print $ dijkstra (findP1Neighbours bound grid) startQueue grid S.empty target
    print $ dijkstra (findP2Neighbours bound grid) startQueue grid S.empty target

dijkstra :: (Move -> Int -> [(Int, Move)]) -> PQ.MinPQueue Int Move -> A.UArray Point Int -> S.Set Move -> Point -> Int
dijkstra findNeighbours queue grid visited target
    | coord == target = prio
    | m `S.member` visited = dijkstra findNeighbours q grid visited target
    | otherwise = dijkstra findNeighbours nextQueue grid (m `S.insert` visited) target
    where ((prio, m@(coord, _, _)), q) = PQ.deleteFindMin queue
          (_, bounds) = A.bounds grid
          neighbours = findNeighbours m prio
          nextQueue = enqueue neighbours q

enqueue :: [(Int, Move)] -> PQ.MinPQueue Int Move -> PQ.MinPQueue Int Move
enqueue [] queue = queue
enqueue ((heatLoss, m@((x, y), _, _)):ms) queue = enqueue ms $ PQ.insert heatLoss m queue

-- TODO: Refactor find neighbour functions.

findP1Neighbours :: (Int, Int) -> A.UArray Point Int -> Move -> Int -> [(Int, Move)]
findP1Neighbours (yBound, xBound) grid ((x, y), dir, consecutive) heatLoss = neighbours
    where near = [((x + 1, y), R, getConsecutive R), ((x - 1, y), L, getConsecutive L), ((x, y + 1), D, getConsecutive D), ((x, y - 1), U, getConsecutive U)]
          neighbours = map (\m@((x, y), _, _) -> (heatLoss + grid A.! (y, x), m)) $ filter (\(p, d, s) -> isInBounds p && invertDirection d /= dir && s <= 3) near
          isInBounds (px, py) = px >= 0 && px <= xBound && py >= 0 && py <= yBound
          getConsecutive d = if d == dir then consecutive + 1 else 1

findP2Neighbours :: (Int, Int) -> A.UArray Point Int -> Move -> Int -> [(Int, Move)]
findP2Neighbours (yBound, xBound) grid ((x, y), dir, consecutive) heatLoss = neighbours
    where near = [((x + addition R, y), R, getConsecutive R), ((x - addition L, y), L, getConsecutive L), ((x, y + addition D), D, getConsecutive D), ((x, y - addition U), U, getConsecutive U)]
          neighbours = map (\m@(nextPoint, nextDir, nextStreak) -> (heatLoss + getHeatLossRange (x, y) nextPoint grid, m)) $ filter (\(p, d, s) -> isInBounds p && invertDirection d /= dir && s <= 10) near
          isInBounds (px, py) = px >= 0 && px <= xBound && py >= 0 && py <= yBound
          getConsecutive d = if d == dir then consecutive + 1 else 4
          addition d = if d == dir then 1 else 4

getHeatLossRange :: Point -> Point -> A.UArray Point Int -> Int
getHeatLossRange (x1, y1) (x2, y2) grid
    | xDiff == 0 = getYValues
    | otherwise = getXValues
    where xDiff = abs (x1 - x2)
          xMin = min x1 x2
          yDiff = abs (y1 - y2)
          yMin = min y1 y2
          lossAtOrigin = grid A.! (y1, x1)
          getXValues = sum (map (\x -> grid A.! (y1, x)) [xMin..xMin+xDiff]) - lossAtOrigin
          getYValues = sum (map (\y -> grid A.! (y, x1)) [yMin..yMin+yDiff]) - lossAtOrigin

invertDirection :: Direction -> Direction
invertDirection dir = case dir of
    R -> L
    L -> R
    U -> D
    D -> U
