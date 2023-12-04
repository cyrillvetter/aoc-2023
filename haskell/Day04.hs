import Data.List.Split (splitOn)
import Data.List (intersect)
import qualified Data.IntMap as M

main = do
    input <- map parse . lines <$> readFile "inputs/4.txt"
    print $ sum $ map calcCardPoints input
    print $ calcTotalCards 1 input $ M.fromList $ map (\l -> (l, 1)) [1..length input]

calcCardPoints :: Int -> Int
calcCardPoints n = if n == 0 then 0 else 2 ^ (n - 1)

calcTotalCards :: Int -> [Int] -> M.IntMap Int -> Int
calcTotalCards _ [] cards = sum $ M.elems cards
calcTotalCards currCard (x:xs) cards = calcTotalCards (currCard + 1) xs nextCards
    where amount = cards M.! currCard
          copies = take x [currCard+1..]
          nextCards = foldl (flip $ M.adjust (+ amount)) cards copies

parse :: String -> Int
parse l = length $ intersect mine winning
    where [mine, winning] = map words $ splitOn " | " $ last $ splitOn ": " l