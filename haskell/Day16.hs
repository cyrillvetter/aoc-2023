import qualified Data.Set as S
import qualified Data.Array.Unboxed as A

type Point = (Int, Int)
type Movement = (Point, Direction)
data Direction = R | D | L | U deriving (Enum, Eq, Ord)

main = do
    input <- lines <$> readFile "inputs/16.txt"
    let upperBound = length input - 1
        grid = A.listArray ((0, 0), (upperBound, upperBound)) $ concat input
    print $ followBeams [((0, 0), R)] grid S.empty
    print $ maximum $ map (\s -> followBeams [s] grid S.empty) $ startingDirections $ length input

startingDirections :: Int -> [Movement]
startingDirections size = top ++ bottom ++ left ++ right
    where bound = size - 1
          top = map (\x -> ((x, 0), D)) [0..bound]
          bottom = map (\x -> ((x, bound), U)) [0..bound]
          left = map (\y -> ((0, y), R)) [0..bound]
          right = map (\y -> ((bound, y), L)) [0..bound]

followBeams :: [Movement] -> A.UArray (Int, Int) Char -> S.Set Movement -> Int
followBeams [] _ visited = S.size $ S.map fst visited
followBeams (m@((x, y), dir):ms) grid visited
    | isOutOfBounds || m `S.member` visited = followBeams ms grid visited
    | otherwise = followBeams (getNextMoves m val ++ ms) grid (m `S.insert` visited)
    where (_, (_, upper)) = A.bounds grid
          isOutOfBounds = x < 0 || x > upper || y < 0 || y > upper
          val = grid A.! (y, x)

getNextMoves :: Movement -> Char -> [Movement]
getNextMoves (p, dir) c
    | c == '.' = [move p dir]
    | c == '\\' || c == '/' = [move p (handleMirror dir c)]
    | otherwise = map (move p) $ handleSplitter dir c

handleMirror :: Direction -> Char -> Direction
handleMirror dir '/' = case dir of
    R -> U
    U -> R
    L -> D
    D -> L
handleMirror dir '\\' = case dir of
    R -> D
    D -> R
    L -> U
    U -> L

handleSplitter :: Direction -> Char -> [Direction]
handleSplitter dir '-' = case dir of
    R -> [R]
    L -> [L]
    _ -> [R, L]
handleSplitter dir '|' = case dir of
    U -> [U]
    D -> [D]
    _ -> [U, D]

move :: Point -> Direction -> Movement
move (x, y) R = ((x + 1, y), R)
move (x, y) D = ((x, y + 1), D)
move (x, y) L = ((x - 1, y), L)
move (x, y) U = ((x, y - 1), U)
