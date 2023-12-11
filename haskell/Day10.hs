import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (transpose)

type Point = (Int, Int)

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

neighbours = [up, down, left, right]

main = do
    input <- scale . lines <$> readFile "inputs/10.txt"
    let grid = createGrid input
        width = length (head input)
        height = length input
        pipeMap = M.fromList grid
        start = fst $ head $ filter ((== 'S') . snd) grid
        startNeighbourPipes = filter (\p -> isInBound p width height) $ map (addPoints start) neighbours
        connectedPipe = head $ filter (\p -> start `elem` getNeighbours p (pipeMap M.! p)) startNeighbourPipes
        loopPath = getMainLoop start connectedPipe pipeMap (S.singleton start)
        floodedElements = floodFill [(0, 0)] width height loopPath
        inner = filter (\(p, c) -> p `S.notMember` floodedElements) grid

    print $ S.size loopPath `div` 4
    print $ (`countElements` 0) $ map fst inner

countElements :: [Point] -> Int -> Int
countElements [] count = count
countElements ((x, y):ps) count = countElements ps val
    where val = if odd x && odd y then count + 1 else count

floodFill :: [Point] -> Int -> Int -> S.Set Point -> S.Set Point
floodFill [] _ _ visited = visited
floodFill (p:ps) width height visited = floodFill (n ++ ps) width height (S.insert p visited)
    where n = filter (\e -> isInBound e width height && e `S.notMember` visited) $ map (addPoints p) neighbours

isInBound :: Point -> Int -> Int -> Bool
isInBound (x, y) width height = x >= 0 && y >= 0 && x < width && y < height

getMainLoop :: Point -> Point -> M.Map Point Char -> S.Set Point -> S.Set Point
getMainLoop prev curr pipes visited
    | currPipe == 'S' = visited
    | otherwise = getMainLoop curr next pipes (S.insert curr visited)
    where currPipe = pipes M.! curr
          test = filter (/= prev) $ getNeighbours curr currPipe
          next = head test

getNeighbours :: Point -> Char -> [Point]
getNeighbours curr = map (addPoints curr) . getConnectingPipes

scale :: [[Char]] -> [[Char]]
scale = transpose . map (addBefore upReplace) . transpose . map (addBefore leftReplace)
    where
        addBefore :: (Char -> Char) -> [Char] -> [Char]
        addBefore f = concatMap (\c -> [f c, c])

upReplace :: Char -> Char
upReplace c
    | c == '|' || c == 'L' || c == 'J' = '|'
    | otherwise = '.'

leftReplace :: Char -> Char
leftReplace c
    | c == '-' || c == 'J' || c == '7' = '-'
    | c == 'S' = '-'
    | otherwise = '.'

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
