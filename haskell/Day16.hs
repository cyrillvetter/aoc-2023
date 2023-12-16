import qualified Data.Set as S
import qualified Data.Map as M

type Point = (Int, Int)
type Movement = (Point, Direction)
data Direction = R | D | L | U deriving (Enum, Eq, Ord)

main = do
    input <- lines <$> readFile "inputs/16.txt"
    let grid = M.fromList $ createGrid input
        origin = ((0, 0), R)
    print $ followBeams [origin] grid S.empty
    print $ maximum $ map (\s -> followBeams [s] grid S.empty) $ startingDirections $ length input

startingDirections :: Int -> [Movement]
startingDirections size = top ++ bottom ++ left ++ right
    where bound = size - 1
          top = map (\x -> ((x, 0), D)) [0..bound]
          bottom = map (\x -> ((x, bound), U)) [0..bound]
          left = map (\y -> ((0, y), R)) [0..bound]
          right = map (\y -> ((bound, y), L)) [0..bound]

followBeams :: [Movement] -> M.Map Point Char -> S.Set Movement -> Int
followBeams [] _ visited = S.size $ S.map fst visited
followBeams (m@(p, dir):ms) grid visited
    | m `S.member` visited = followBeams ms grid visited
    | otherwise = case p `M.lookup` grid of
        Just c  -> followBeams (move ms m c) grid (m `S.insert` visited)
        Nothing -> followBeams ms grid visited

move :: [Movement] -> Movement -> Char -> [Movement]
move moves curr n
    | n == '.' = moveEmpty curr : moves
    | n == '\\' || n == '/' = moveMirror curr n : moves
    | otherwise = moveSplitter curr n ++ moves

moveEmpty :: Movement -> Movement
moveEmpty ((x, y), dir)
    | dir == R = ((x + 1, y), dir)
    | dir == D = ((x, y + 1), dir)
    | dir == L = ((x - 1, y), dir)
    | dir == U = ((x, y - 1), dir)

moveMirror :: Movement -> Char -> Movement
moveMirror ((x, y), dir) '/'
    | dir == R = ((x, y - 1), U)
    | dir == D = ((x - 1, y), L)
    | dir == L = ((x, y + 1), D)
    | dir == U = ((x + 1, y), R)
moveMirror ((x, y), dir) '\\'
    | dir == R = ((x, y + 1), D)
    | dir == D = ((x + 1, y), R)
    | dir == L = ((x, y - 1), U)
    | dir == U = ((x - 1, y), L)

moveSplitter :: Movement -> Char -> [Movement]
moveSplitter m@((x, y), dir) '-'
    | dir == R || dir == L = [moveEmpty m]
    | otherwise = [((x - 1, y), L), ((x + 1, y), R)]
moveSplitter m@((x, y), dir) '|'
    | dir == U || dir == D = [moveEmpty m]
    | otherwise = [((x, y + 1), D), ((x, y - 1), U)]

createGrid :: [[Char]] -> [(Point, Char)]
createGrid chars = [((x, y), c) | (y, row) <- zip [0..] chars, (x, c) <- zip [0..] row]
