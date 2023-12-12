import Data.List (transpose)
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

neighbours = [up, down, left, right]

main = do
    input <- expandOuter . scale . lines <$> readFile "inputs/10.txt"
    let grid = createGrid input
        width = length (head input)
        height = length input
        pipeMap = M.fromList grid
        loopPath = getMainLoop grid pipeMap width height
        floodedElements = floodFillOuter [(0, 0)] width height loopPath
        inner = filter (\(p, c) -> p `S.notMember` floodedElements) grid

    print $ S.size loopPath `div` 4
    print $ (`countElements` 0) $ map fst inner

followMainLoop :: Point -> Point -> M.Map Point Char -> S.Set Point -> S.Set Point
followMainLoop prev curr pipes visited
    | currPipe == 'S' = visited
    | otherwise = followMainLoop curr next pipes (S.insert curr visited)
    where currPipe = pipes M.! curr
          test = filter (/= prev) $ getNeighbours curr currPipe
          next = head test

countElements :: [Point] -> Int -> Int
countElements [] count = count
countElements ((x, y):ps) count = countElements ps val
    where val = if odd x && odd y then count + 1 else count

floodFillOuter :: [Point] -> Int -> Int -> S.Set Point -> S.Set Point
floodFillOuter [] _ _ visited = visited
floodFillOuter (p:ps) width height visited
    | p `S.member` visited = floodFillOuter ps width height visited
    | otherwise = floodFillOuter (ps ++ n) width height (S.insert p visited)
    where n = filter (\e -> isInBound e width height && e `S.notMember` visited) $ map (addPoints p) neighbours

getMainLoop :: [(Point, Char)] -> M.Map Point Char -> Int -> Int -> S.Set Point
getMainLoop grid pipeMap width height = followMainLoop start connectedPipe pipeMap (S.singleton start)
    where start = fst $ head $ filter ((== 'S') . snd) grid
          startNeighbourPipes = filter (\p -> isInBound p width height) $ map (addPoints start) neighbours
          connectedPipe = head $ filter (\p -> start `elem` getNeighbours p (pipeMap M.! p)) startNeighbourPipes

expandOuter :: [[Char]] -> [[Char]]
expandOuter l = map (++ ".") l ++ [replicate (length (head l) + 1) '.']

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
    | otherwise = '.'

isInBound :: Point -> Int -> Int -> Bool
isInBound (x, y) width height = x >= 0 && y >= 0 && x < width && y < height

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
