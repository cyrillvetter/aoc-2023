import Data.List (transpose)
import Data.Tuple (swap)
import qualified Data.Array.Unboxed as A
import qualified Data.Set as S

type Point = (Int, Int)

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

neighbours = [up, down, left, right]

main = do
    input <- expandOuter . scale . lines <$> readFile "inputs/10.txt"
    let upperBound = length input - 1
        grid = A.listArray ((0, 0), (upperBound, upperBound)) $ concat input
        loopPath = getMainLoop grid upperBound
        floodedElements = floodFillOuter [(0, 0)] upperBound loopPath
        inner = filter (\(p, c) -> p `S.notMember` floodedElements) $ A.assocs grid

    print $ S.size loopPath `div` 4
    print $ (`countElements` 0) $ map fst inner

followMainLoop :: Point -> Point -> A.UArray (Int, Int) Char -> S.Set Point -> S.Set Point
followMainLoop prev curr@(x, y) pipes visited
    | currPipe == 'S' = visited
    | otherwise = followMainLoop curr next pipes (S.insert curr visited)
    where currPipe = pipes A.! (y, x)
          test = filter (/= prev) $ getNeighbours curr currPipe
          next = head test

countElements :: [Point] -> Int -> Int
countElements [] count = count
countElements ((x, y):ps) count = countElements ps val
    where val = if odd x && odd y then count + 1 else count

floodFillOuter :: [Point] -> Int -> S.Set Point -> S.Set Point
floodFillOuter [] _ visited = visited
floodFillOuter (p:ps) upperBound visited
    | p `S.member` visited = floodFillOuter ps upperBound visited
    | otherwise = floodFillOuter (ps ++ n) upperBound (S.insert p visited)
    where n = filter (\e -> isInBound e upperBound && e `S.notMember` visited) $ map (addPoints p) neighbours

getMainLoop :: A.UArray (Int, Int) Char -> Int -> S.Set Point
getMainLoop pipes upperBound = followMainLoop start connectedPipe pipes (S.singleton start)
    where start = swap $ fst $ head $ filter ((== 'S') . snd) $ A.assocs pipes
          startNeighbourPipes = filter (`isInBound` upperBound) $ map (addPoints start) neighbours
          connectedPipe = head $ filter (\p@(x, y) -> start `elem` getNeighbours p (pipes A.! (y, x))) startNeighbourPipes

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

isInBound :: Point -> Int -> Bool
isInBound (x, y) upperBound = x >= 0 && y >= 0 && x <= upperBound && y <= upperBound

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
