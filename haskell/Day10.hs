import qualified Data.Map as M

type Point = (Int, Int)

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

main = do
    input <- createGrid . lines <$> readFile "inputs/10.txt"
    let grid = M.fromList input
        start = fst $ head $ filter ((== 'S') . snd) input
        startPipes = map (addPoints start) [up, right, down, left]
        connectedPipe = head $ filter (\p -> start `elem` getNeighbours p (grid M.! p)) startPipes

    print $ getMainLoopLength start connectedPipe 0 grid

getMainLoopLength :: Point -> Point -> Int -> M.Map Point Char -> Int
getMainLoopLength prev curr steps pipes
    | currPipe == 'S' = (steps `div` 2) + 1
    | otherwise = getMainLoopLength curr next (steps + 1) pipes
    where currPipe = pipes M.! curr
          next = head $ filter (/= prev) $ getNeighbours curr currPipe

getNeighbours :: Point -> Char -> [Point]
getNeighbours curr = map (addPoints curr) . getConnectingPipes

getConnectingPipes :: Char -> [Point]
getConnectingPipes '|' = [up, down]
getConnectingPipes '-' = [left, right]
getConnectingPipes 'L' = [up, right]
getConnectingPipes 'J' = [up, left]
getConnectingPipes '7' = [down, left]
getConnectingPipes 'F' = [down, right]
getConnectingPipes _ = []

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

createGrid :: [[Char]] -> [(Point, Char)]
createGrid chars = [((x, y), c) | (y, row) <- zip [0..] chars, (x, c) <- zip [0..] row]
