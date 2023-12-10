import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate, transpose)

type Point = (Int, Int)

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

main = do
    -- input <- createGrid . lines <$> readFile "inputs/10.txt"
    input <- lines <$> readFile "inputs/10.txt"
    let grid = createGrid input
        pipeMap = M.fromList grid
        start = fst $ head $ filter ((== 'S') . snd) grid
        startNeighbourPipes = map (addPoints start) [up, right, down, left]
        connectedPipe = head $ filter (\p -> start `elem` getNeighbours p (pipeMap M.! p)) startNeighbourPipes
        notConnectedPipe = head $ filter (\p -> start `notElem` getNeighbours p (pipeMap M.! p)) startNeighbourPipes

    print notConnectedPipe
    print $ getMainLoop start connectedPipe 0 pipeMap
    putStrLn $ intercalate "\n" $ scale input

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

getMainLoop :: Point -> Point -> Int -> M.Map Point Char -> Int
getMainLoop prev curr steps pipes
    | currPipe == 'S' = (steps `div` 2) + 1
    | otherwise = getMainLoop curr next (steps + 1) pipes
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
